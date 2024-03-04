library(readrba)
library(dplyr)
library(ggplot2)
library(tidyr)
library(splines)
library(lubridate)
library(tsibble)

eco <- c("FIRMMCRT"=  "CashRate", 
         "GCPIEITCYP"="Inflation", 
         "GGDPCVGDPY"="GDPGrowth", 
         "GLFSURSA"=  "UnempRate", 
         "GNFAEYP"=   "LabourEarn",
         "GLFOSVTLF"= "JobVacancies")
  
lastnona <- function(x) { 
  x <- x[!is.na(x)]
  if (length(x)==0) { return(NA) }
  else { return(tail(x,1)) }
  }
    
calc_decade <- function(x) { 
  (lubridate::year(x) %/% 10)*10 } 

data <- readrba::read_rba(series_id=names(eco)) %>% 
  mutate(name=eco[series_id])  %>% 
  select(date, name, value) %>% 
  arrange(date) %>% 
  pivot_wider(id_cols=date, names_from=name, values_from = value) %>% 
  mutate(period = tsibble::yearquarter(date)) %>%  
  group_by(period) %>% 
  summarise(across(any_of(unname(eco)), ~lastnona(.x))) %>% 
  mutate(decade = calc_decade(period)) %>% 
  filter(decade >= 1970) 

data %>% 
  ggplot(aes(x=log(UnempRate/JobVacancies), y=Inflation))  + 
  geom_point(aes(col=paste0(decade, "s"))) + 
  geom_smooth(data=~filter(., decade>=1990), 
              method="lm", formula="y ~ bs(x, degree=1, knots=c(1))", col="black") + 
  scale_y_continuous(limits=c(NA, 13)) + 
  labs(x=expression(log ~ frac(u,v)), 
       y=expression(pi), 
       col="Decade", 
       title="Vacancies adjusted Phillips Curve", 
       caption="Regression excludes pre 1990 data") + 
  theme_bw()




