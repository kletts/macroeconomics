
library(tidyverse)
library(grid)
library(gridExtra)
library(tidyquant)

get_fullhpindex <- function() { 
  splice_index <- function(...) { 
    series <-  list(...)
    series <- lapply(series, \(x) difference(log(x))) 
    joint <- coalesce(!!!series)
    exp(cumsum(c(0, joint[-1]))) }
  file <- "~/Bizscisolve/rwa-calc/credit-solutions-rmd/data/aust_housepriceindex.rds"
  data <- read_rds(file) 
  if (max(data$date) < Sys.Date() %m+% months(-7)) { 
    hp <- c(  
      "A83728647F" = "Series3",
      "A83728455L" = "Series2",
      "A2333613R"  = "Series1") 
    data <- readabs::read_abs(series_id = names(hp))  
    dataindex <- data %>% 
      mutate(name=hp[series_id]) %>% 
      pivot_wider(id_cols=date, names_from = name, values_from=value) %>% 
      arrange(date) %>% 
      transmute(date=date, 
                index=splice_index(Series3, Series2, Series1)) 
    data <- data %>%  
      left_join(dataindex, by="date") %>%  
      mutate(period=tsibble::yearquarter(date)) 
    write_rds(data, file) }
  return(data) } 

eco <- c("FIRMMCRT"=  "CashRate", 
        "GCPIEITCYP"="Inflation", 
        "GGDPCVGDPY"="GDPGrowth", 
        "GLFSURSA"=  "UnempRate", 
        "GNFAEYP"=   "LabourEarn",
        "GLFOSVT" = "JobVacancies")

datahp <- get_fullhpindex() %>%
      arrange(period) %>% 
      group_by(period) %>% 
      summarise(index=head(index,1)) %>% 
      mutate(HousePriceGrowth=100*(index/dplyr::lag(index, 4)-1)) 
data <- readrba::read_rba(series_id=names(eco)) %>% 
      mutate(name=eco[series_id]) %>% 
      select(date, name, value) %>% 
      pivot_wider(id_cols=date, names_from=name, values_from = value) %>% 
      mutate(period = tsibble::yearquarter(date)) %>% 
      left_join(datahp %>% select(period, HousePriceGrowth), 
                by="period") %>% 
      arrange(period) %>% 
      drop_na() %>%
      select(-date) %>% 
      select(period, everything())
    mod <- prcomp(data.matrix(select(data, all_of(prvar))),
                  scale. = TRUE)
    data <- data %>% mutate(
      Zindex = mod$x[,"PC1"], 
      DCovid = ifelse(between(period, yearquarter("2020 Q1"), yearquarter("2021 Q4")),1,0))


lag <- dplyr::lag

mod <- lm(LabourEarn ~  lag(Inflation,4) + I(lag(UnempRate,4)/lag(UnempRate, 12)-1),
          data=data, subset=period<yearquarter("2020 Q1")) 
summary(mod)

t1 <- broom::tidy(mod) %>%  
  data.frame(row.names ="term") %>% 
  mutate(across(-p.value, ~scales::label_number(accuracy=0.01)(.x)), 
         p.value = scales::label_percent(accuracy=0.01)(p.value)) %>% 
  tableGrob(theme=ttheme_minimal(
    base_size=9, 
    padding = unit(c(2, 2), "mm")))

p01 <- data %>% 
  ggplot(aes(x=period, y=Inflation)) + 
  geom_line() +
  geom_ma(ma_fun=SMA, n=12, color = "red", linetype=1) + 
  labs(title="Inflation", x=NULL, y=NULL) + 
  theme_bw() 

p02 <- data %>% 
  ggplot(aes(x=period, y=lag(UnempRate,4)/lag(UnempRate, 12)-1)) + 
  geom_line() + 
  labs(title="Unemployment", x=NULL, y=NULL) + 
  theme_bw()

p1 <- data %>% 
  mutate(LabourEarn.pred = predict(mod, .)) %>% 
  ggplot(aes(x=period, y=LabourEarn, col="Actual")) + 
  geom_line() + 
  geom_ma(ma_fun=SMA, n=12, color ="black", linetype=1) + 
  geom_line(aes(y=LabourEarn.pred, col="Predicted")) + 
  theme_bw() + 
  theme(legend.position = c(1,1), 
        legend.justification = c(1,1), 
        legend.background = element_blank()) + 
  labs(x=NULL, y=NULL, title="Labour Earnings", col=NULL)

p31 <- acf(mod$residuals, plot=FALSE) %>%
  { data.frame(lag=as.vector(.$lag), 
               acf=as.vector(.$acf)) } %>% 
  ggplot(aes(x=lag, y=acf)) + 
  geom_col() + 
  theme_bw() + 
  labs(title="ACF", y=NULL, x=NULL)

p32 <- mod$residuals %>% 
  enframe() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(aes(y = after_stat(density)), fill="pink", col="red") + 
  geom_function(fun=dnorm, 
                args=list(mean=mean(mod$residuals), 
                          sd=sd(mod$residuals))) + 
  scale_y_continuous(labels=scales::percent) + 
  theme_bw() + 
  labs(title="Residual density distribution", y=NULL, x=NULL)

p30 <- arrangeGrob(p31, p32, ncol=2, widths=c(1,1))    
t2 <- textGrob("Simple Australian Labour Earnings Macroeconomic Model", 
               gp = gpar(fontsize=12))

p0 <- arrangeGrob(p01, p02, ncol=2, widths=c(1,1))
pt <- arrangeGrob(t2, p1, p0, t1, p30,  
                  ncol=1, heights=c(0.5, 6, 3, 2, 3), 
                  vp= viewport(width=0.96, height=0.96)) 
ggsave("wages.pdf", pt, height=29, width=21, units="cm")
    

# tidymodels ----- 

library(tidymodels)
library(recipes)
library(parsnip)
library(timetk)

rec <- recipe(LabourEarn ~ CashRate + UnempRate + Inflation + GDPGrowth + HousePriceGrowth, 
              data = data) %>%  
  step_diff(UnempRate, lag=4) %>% 
  step_lag(all_predictors(),  lag = 1:4) 
  
mod <- linear_reg(penalty = 0.01) %>% 
  set_engine("glmnet")

fit <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod) %>% 
  fit(data=data)

tidy(fit)


#---- VAR model ----- 
prvar <- c("UnempRate"   ,     "Inflation"    ,   "LabourEarn")

varmod <- vars::VAR(data %>% 
                      mutate(UnempRate = difference(log(UnempRate), 4)) %>% 
                      drop_na() %>% 
                      dplyr::select(all_of(prvar)), 
                    p=2, type = "const", season=NULL, exog=NULL)

summary(varmod)



