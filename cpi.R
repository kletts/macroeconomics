

library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(readabs)
library(haven)

cut_midzero <- function(x, length.out=11) { 
  lower <- seq(min(x), 0, length.out= length.out/2)
  upper <- seq(0, max(x), length.out= length.out/2)
  breaks <- c(lower[lower!=0], upper[upper!=0]) 
  labels <- round(breaks*100,1)
  labels <- paste(head(labels, -1), tail(labels, -1), sep="..")
  cut(x, breaks=breaks, labels=labels, include.lowest=TRUE)  } 


qcpi <- readabs::read_api(id="CPI",
        datakey = list(measure=1, 
                       index=c("10001", "20001", "20002", "20003", "20004", "20005", "20006", 
                              "115486", "115488", "115489", "115493", "126670", "999901"), 
                       tsest= "10",
                       region=c("8", "7", "6", "5", "4", "3", "2", "1", "50"), 
                       freq="Q"))

citystate <- c("Sydney"="New South Wales", 
  "Melbourne"="Victoria", 
  "Brisbane"="Queensland", 
  "Adelaide"="South Australia", 
  "Perth"="Western Australia", 
  "Weighted average of eight capital cities"= "Australia", 
  "Hobart"="Tasmania", 
  "Darwin"="Northern Territory", 
  "Canberra"="Australian Capital Territory")
citystate <- setNames(names(citystate), citystate)

indexcategory <- c("All groups CPI"="Total", 
                   "Food and non-alcoholic beverages"="Food", 
                   "Clothing and footwear"="Clothing and footwear", 
                   "Furnishings, household equipment and services"="Furnishings and household equipment", 
                   "Transport"="Transport", 
                   "Alcohol and tobacco"="Alcoholic beverages and tobacco", 
                   "Recreation and culture"="Recreation and culture")


qcpi2 <- qcpi %>% 
  mutate(time_period = tsibble::yearquarter(time_period),
         state = fct_recode(as_factor(region), !!!setNames(names(citystate), citystate)), 
         category =  fct_recode(as_factor(index), !!!setNames(names(indexcategory), indexcategory))) %>% 
  tsibble(key=c(state, category), index=time_period) %>% 
  group_by_key() %>% 
  mutate(price.4q= obs_value/dplyr::lag(obs_value, 4)-1) 

mexp %>% 
  mutate(time_period=as.Date(time_period)) %>% 
  inner_join(qcpi2 %>% 
               mutate(time_period=as.Date(time_period)) %>% 
               select(category, state, time_period, "cpi_value"=obs_value, price.4q), 
             by=c("state"="state", "time_period"="time_period", "category"="category")) %>%  
  mutate(
    time_period=tsibble::yearquarter(time_period),
    spend.4q = trend/dplyr::lag(trend, 4)-1, 
    quantity.4q = spend.4q - price.4q) %>% 
  drop_na(spend.4q) %>% 
  pivot_longer(cols=c(price.4q, spend.4q, quantity.4q)) %>% 
  #filter(time_period > yearquarter("2021 Q1")) %>% 
  ggplot(aes(x=time_period, y=value, col=name)) + 
    geom_line() + 
    facet_grid(category ~ state, 
               scales="free_y", 
               labeller = labeller(.default=~str_wrap(.x, 10))) + 
    theme_bw() + 
    theme(strip.background=element_blank(), 
          strip.text=element_text(face = "bold")) + 
    scale_x_yearquarter(date_breaks="1 year", 
                        date_labels="%y") + 
    scale_y_continuous(labels = scales::percent)

mexp %>% 
  mutate(time_period=as.Date(time_period)) %>% 
  inner_join(qcpi2 %>% 
               mutate(time_period=as.Date(time_period)) %>% 
               select(category, state, time_period, "cpi_value"=obs_value, "Price"=price.4q), 
             by=c("state"="state", "time_period"="time_period", "category"="category")) %>%  
  mutate(
    time_period=tsibble::yearquarter(time_period),
    Spend = trend/dplyr::lag(trend, 4)-1, 
    Quantity = Spend - Price) %>% 
  drop_na(Spend) %>% 
  pivot_longer(cols=c(Price, Spend, Quantity)) %>% 
  filter(time_period==max(time_period)) %>% 
  filter(state=="Australia") %>% 
  mutate(value.b = cut_midzero(value)) %>% 
  ggplot(aes(y=value, x=name, fill=name)) + 
  geom_col() + 
  facet_wrap(vars(category), 
             nrow=1, 
             labeller = labeller(.default=~str_wrap(.x, 10))) + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        strip.background = element_blank(), 
        strip.text=element_text(face="bold"),
        axis.text.x=element_blank()) + 
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_brewer(palette="Set1") + 
  labs(fill=NULL, x=NULL, y="(%pa)", 
       title="Household expenditure by category decomposition", 
       subtitle="Year ended June 2023")

