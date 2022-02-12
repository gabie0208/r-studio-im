library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(clipr)

getwd()
setwd("/Users/hyejungko/Desktop/Raw Data/Indian E-commerce website/")

ord_info <- fread("practices/List of Orders.csv") %>% tibble()
ord_prd <- fread("practices/Order Details.csv") %>% tibble()
cust_info <- fread("practices/Customer Segmentation.csv") %>% tibble()

a_table <- ord_info %>%
  filter(cust_nm %in% c('Ekta'))
a_table
b_table <- ord_info %>%
  filter(ord_cd %in% c('B-25677', 'B-25703', 'B-25601'))
b_table

left_join(a_table, b_table, by = 'ord_cd')
right_join(a_table, b_table, by = 'ord_cd')
inner_join(a_table, b_table, by = 'ord_cd')
full_join(a_table, b_table, by = 'ord_cd')

left_join(ord_info, ord_prd)
right_join(ord_info, ord_prd)
inner_join(ord_info, ord_prd)
full_join(ord_info, ord_prd)

week_sales <- left_join(ord_info, ord_prd) %>%
  select(ord_dt, ord_cd, ord_amt) %>%
  mutate(ord_week = ord_dt %>% dmy %>% week,
         ord_dow = ord_dt %>% dmy %>% wday(label = T)) %>%
  select(ord_week, ord_dow, ord_amt)
week_sales

week_sales %>%
  group_by(ord_dow) %>%
  summarise(dow_sales = sum(ord_amt))

week_sales %>%
  group_by(ord_week, ord_dow) %>%
  summarise(dow_sales = sum(ord_amt)) %>%
  data.table() %>%
  dcast.data.table(ord_week ~ ord_dow, value.var = 'dow_sales', sum) %>%
  tibble()

ord_info %>% left_join(cust_info) %>%
  distinct(ord_cd, .keep_all = T) %>%
  select(ord_cd, ord_dt, cust_nm, gender, age) %>%
  arrange(cust_nm, ord_cd)

gender_catg_info <- ord_info %>% left_join(cust_info) %>%
  distinct(ord_cd, .keep_all = T) %>%
  left_join(ord_prd) %>%
  select(ord_cd, ord_dt, cust_nm, state, gender, ord_amt, ord_profit, qty, catg_1)
gender_catg_info

gender_catg_info %>% group_by(gender, catg_1) %>%
  summarise(revenue = sum(ord_amt), 
            cust = n_distinct(cust_nm)) %>%
  mutate(aov = revenue / cust) %>%
  data.table() %>%
  dcast.data.table(catg_1 ~ gender, value.var = "aov") %>%
  tibble()
