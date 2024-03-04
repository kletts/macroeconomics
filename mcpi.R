
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

p1 <- mcpi %>%  
  filter(category!="All groups CPI") %>% 
  ggplot(aes(x=time_period, y=price.4q)) +
  geom_line(aes(color=category)) + 
  geom_text(
    data=~filter(.x, time_period==max(time_period)) %>% 
      mutate(label= scales::label_percent(accuracy=0.1)(price.4q)), 
    aes(color=category, label=label), 
    hjust="left", size=3) + 
  geom_line(data=mcpi %>% filter(category=="All groups CPI") %>% 
              rename(all=category), 
            aes(x=time_period, y=price.4q, linetype="All Groups")) +
  facet_wrap(vars(category), 
             labeller = labeller(category= \(x) str_wrap(x, width=20))) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_yearmonth(date_labels = "%y-%b", 
                    expand=expansion(add = c(0, 500))) +
  theme_bw() + 
  theme(strip.background = element_blank(), 
        legend.position = c(1,0), 
        legend.justification = c(1,0)) + 
  guides(color="none") + 
  labs(title="CPI by Category",
       subtitle="Year on Year Change",
       x=NULL, y=NULL, linetype=NULL)

ggsave("mcpi.png", p1, device="png", scale = 1.5,
       width=13, height=10, units="cm")


