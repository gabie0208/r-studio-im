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

cust_info %>% arrange(age)
cust_info %>% arrange(age %>% desc)
cust_info %>%
  select(cust_nm, age, tenure, job) %>%
  arrange(tenure %>% desc, age %>% desc)
cust_info %>%
  select(cust_nm, age, tenure, job) %>%
  arrange(age %>% desc, tenure %>% desc)
cust_info %>%
  select(cust_nm, age, tenure, job) %>%
  arrange(tenure %>% desc, age)

# pracrtice

ord_prd %>%
  mutate(ord_dc = ord_amt - ord_profit) %>%
  arrange(ord_dc %>% desc) %>%
  select(ord_cd, ord_amt, ord_profit, ord_dc)

ord_prd %>%
  filter(catg_1 == "Electronics") %>%
  arrange(ord_profit)

ord_prd %>%
  filter(catg_1 == "Clothing") %>%
  arrange(qty %>% desc)

ord_info %>%
  arrange(state, city) %>%
  distinct(state, city) %>%
  print(n = 100)

ord_info %>%
  arrange(city, cust_nm) %>%
  distinct(cust_nm, city) %>%
  select(city, cust_nm) %>%
  print(n = 100)

ord_info %>%
  mutate(ord_dow = ord_dt %>% dmy() %>% wday(label = T)) %>%
  arrange(ord_dow, cust_nm) %>%
  distinct(ord_dow, cust_nm) %>%
  select(ord_dow, cust_nm) %>%
  print(n = 100)

cust_info %>%
  filter(graduated == "Yes") %>%
  arrange(family %>% desc, age) %>%
  select(cust_nm, family, age, job)
cust_info
cust_info %>%
  filter(gender == 'Male', married == 'Yes', age > 30) %>%
  arrange(spend) %>%
  select(cust_nm, spend, job, family) %>%
  print(n = 100)
