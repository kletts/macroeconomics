
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(readabs)
library(haven)

mexp <- readabs::read_api(id="HSI_M",
                          datakey = list(measure=1, 
                                         category=c("50", "45", "40", "35", "30", "25", "20", "15", "10", "TOT"), 
                                         price_adjustment="CUR", 
                                         state=c("8", "7", "6", "5", "4", "3", "2", "1", "AUS"), 
                                         freq="M"))


mexp <- mexp %>% 
  mutate(time_period = tsibble::yearmonth(time_period, "%Y-%m")) %>% 
  mutate(across(c(category, price_adjustment, state), as_factor)) %>% 
  tsibble(key=c(category, state), index=time_period) %>% 
  model(STL(obs_value ~ trend() + season(period=12))) %>% 
  components() 

write_csv(mexp %>% mutate(time_period=as.Date(time_period)), "mexp.csv")

cut_midzero <- function(x, length.out=11) { 
  lower <- seq(min(x), 0, length.out= length.out/2)
  upper <- seq(0, max(x), length.out= length.out/2)
  breaks <- c(lower[lower!=0], upper[upper!=0]) 
  labels <- round(breaks*100,1)
  labels <- paste(head(labels, -1), tail(labels, -1), sep="..")
  cut(x, breaks=breaks, labels=labels, include.lowest=TRUE)  } 

mexp %>% 
  group_by(category, state) %>% 
  mutate(trend.chg = trend/dplyr::lag(trend, 12)-1) %>% 
  ungroup() %>% 
  filter(time_period==max(time_period)) %>% 
  mutate(trend.chg.b = cut_midzero(trend.chg)) %>% 
  ggplot(aes(y=category, x=state, fill=trend.chg.b)) + 
  geom_tile() + 
  geom_text(aes(label=scales::percent(trend.chg, accuracy =0.1), 
                col = abs(trend.chg)> 0.05), 
            size=3, show.legend = FALSE)  + 
  theme_bw() + 
  scale_fill_brewer(palette="PRGn") + 
  scale_color_manual(values=c("black", "white")) + 
  scale_x_discrete(labels= ~str_wrap(.x, width = 10)) + 
  scale_y_discrete(labels= ~str_wrap(.x, width = 20)) + 
  labs(x=NULL, y=NULL, 
       fill="Chg: %pa", 
       title="Trend Change in Household Expenditure") 


