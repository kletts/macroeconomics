

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
         state = fct_recode(as_factor(region), !!!citystate), 
         category =  fct_recode(as_factor(index), !!!indexcategory)) %>% 
  tsibble(key=c(state, category), index=time_period) %>% 
  group_by_key() %>% 
  mutate(price.4q= obs_value/dplyr::lag(obs_value, 4)-1) 

