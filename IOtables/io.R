
library(readxl)
library(tidyverse)
library(DiagrammeR)
library(data.tree)


calc_leaf_agg <- function(data, max, i) {
  stopifnot("Node" %in% class(data))
  data.tree::Do(Traverse(data, filterFun = function(x) x$level == i), 
        \(x){ x$rollupName <- ifelse(x$rollSum > max, x$name, NA)
              x$rollupLevel <- ifelse(x$rollSum > max, i, NA)
              x$rollSum <- is.na(x$rollupName) * x$rollSum })
  data.tree::Do(Traverse(data, filterFun = function(x) x$level > i & is.na(x$rollupName)), 
        \(x) { x$rollupName <- ifelse(!is.na(x$parent$rollupName), x$parent$rollupName, NA)
               x$rollupLevel <- ifelse(!is.na(x$parent$rollupName), i, NA)
               x$rollSum <- ifelse(!is.na(x$parent$rollupName), 0,  x$rollSum)  })
  data.tree::Do(Traverse(data, traversal = "post-order"), 
        \(x) { x$rollSum <- data.tree::Aggregate(node = x, attribute = "rollSum", aggFun = sum) })
  return(data) }

calc_anzsic_rollup <- function(data, vsize, nmax=10) {
  stopifnot("pathString" %in% colnames(data))
  dtree <- data.tree::as.Node(data %>% dplyr::mutate(n = .data[[vsize]])) 
  data.tree::Do(Traverse(dtree, "post-order"), function(x) {
    x$n <- data.tree::Aggregate(node = x, attribute = "n", aggFun = sum)
    x$rollupName <- NA
    x$rollupLevel <- NA
    x$rollSum <- x$n })
  size <- data.tree::Aggregate(dtree, "n", sum)/nmax
  height <- dtree$height
  stopifnot(height>1)
  for (i in height:1) {
    calc_leaf_agg(dtree, size, i) }
  data.tree::ToDataFrameTable(dtree, 
    "name", "rollupName", "rollupLevel", vsize) }



setwd("/Users/cklettner/Documents/Macroeconomics/IOtables")

url <- "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/2020-21/520905500105.xlsx"

remove.to <- c("Final Uses (Q1 to Q7)", "Total Industry Uses", "Total Supply")
remove.from <- c("Total Intermediate Use", "Australian Production", "Value Added")

finaluse.tocode <- c(
  "Changes in Inventories"  =   "Q1",                         
  "Exports of Goods and Services" = "Q2",
  "General Government Final Consumption Expenditure" = "Q3", 
  "General Government Gross Fixed Capital Formation" ="Q4",
  "Households Final Consumption Expenditure"  ="Q5",
  "Private Gross Fixed Capital Formation" ="Q6",    
  "Public Corporations Gross Fixed Capital Formation"="Q7") 


anzsic <- read_rds("~/Bizscisolve/asic-insolvency/data/abs/anzsic/anzsic06-classification.rds") %>% 
  group_by(IOIG) %>% slice(1) %>% ungroup() %>% 
  mutate(GroupCnt = length(Group), .by ="Group") %>%  
  mutate(SubdivisionCnt = length(Subdivision), .by="Subdivision") %>%  
  mutate(DivisionCnt = length(Division), .by="Division") %>%  
  rowwise() %>% 
  mutate(pathString = paste(c("Total", 
                              if(DivisionCnt>1) Division else NULL, 
                              if(SubdivisionCnt>1) Subdivision else NULL, 
                              if(GroupCnt > 1) Group else NULL, 
                              IOIG), collapse ="/")) %>% 
  ungroup() %>% 
  select(-ends_with("Cnt"))

data <- read_excel("520905500105.xlsx", sheet="Table 5", skip=1, col_types="text") %>% 
  rename("FromCode"=`...1`, 
         "From"=`...2`) %>% 
  filter(row_number()>1) %>% 
  pivot_longer(cols=-c(1,2), names_to="To", values_to="Use") %>% 
  mutate(Use = parse_number(Use), 
         To = str_remove_all(To, "\r\n|;"), 
         To = str_squish(To), 
         From = str_remove_all(From, ";"), 
         FromCode = ifelse(str_detect(FromCode, "^P"), FromCode, sprintf("%04.f", parse_number(FromCode)))) %>%  
  filter(!(To %in% remove.to)) %>%  
  filter(!(From %in% remove.from)) %>% 
  mutate(FinalUse = ifelse(To %in% From, FALSE, TRUE), 
         InitialInputs = ifelse(From %in% To, FALSE, TRUE)) %>% 
  drop_na(Use) %>%  
  mutate(ToCode = FromCode[match(To, From)], 
         ToCode = ifelse(is.na(ToCode), finaluse.tocode[To], ToCode))

tree <- data %>% 
  group_by(FromCode, From) %>% 
  summarise(Use =sum(Use)) %>% 
  inner_join(anzsic %>% select(IOIG, pathString), 
            by=c("FromCode"="IOIG")) %>% 
  calc_anzsic_rollup(vsize="Use", 20) 

data.agg <- data %>% 
  left_join(tree %>% select(name, "FromCodeSumm"=rollupName), 
            by=c("FromCode"="name" )) %>% 
  left_join(tree %>% select(name, "ToCodeSumm"=rollupName), 
            by=c("ToCode"="name")) %>%  
  mutate(FromCode = coalesce(FromCodeSumm, FromCode), 
         ToCode = coalesce(ToCodeSumm, ToCode)) %>% 
  group_by(FromCode, ToCode) %>% 
  summarise(Use = sum(Use), 
            across(c(FinalUse, InitialInputs, FromCodeSumm, ToCodeSumm), ~head(.x, 1)))



