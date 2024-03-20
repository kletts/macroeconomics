
library(tidyverse)
library(tsibble)
library(readabs)


cut_midzero <- function(x, length.out=11) { 
  lower <- seq(min(x), 0, length.out= length.out/2)
  upper <- seq(0, max(x), length.out= length.out/2)
  breaks <- c(lower[lower!=0], upper[upper!=0]) 
  labels <- round(breaks*100,1)
  labels <- paste(head(labels, -1), tail(labels, -1), sep="..")
  cut(x, breaks=breaks, labels=labels, include.lowest=TRUE)  } 


mcpi <- readabs::read_api(id="CPI_M",
  datakey = list(measure=1, 
                 index=c("10001", "20001", "20002", "20003", "20004", "20005", "20006", 
                         "115486", "115488", "115489", "115493", "126670", "999901"), 
                 tsest= "10",
                 region="50", 
                 freq="M"))


mcpi <- mcpi %>% 
  mutate(time_period = tsibble::yearmonth(time_period),
         category =  as_factor(index)) %>% 
  tsibble(key=category, index=time_period) %>% 
  group_by_key() %>% 
  mutate(price.4q= obs_value/dplyr::lag(obs_value, 12) -1) 

qcpi <- readabs::read_api(id="CPI",
  datakey = list(measure=1, 
                 index=c("10001", "20001", "20002", "20003", "20004", "20005", "20006", 
                         "115486", "115488", "115489", "115493", "126670", "999901"), 
                 tsest= "10",
                 region="50", 
                 freq="Q"))

qcpi <- qcpi %>% 
  mutate(time_period = tsibble::yearquarter(time_period),
         category =  as_factor(index)) %>% 
  tsibble(key=category, index=time_period) %>% 
  group_by_key() %>% 
  mutate(price.4q= obs_value/dplyr::lag(obs_value, 4) -1) 


