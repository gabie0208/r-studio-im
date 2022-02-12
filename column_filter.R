getwd()
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

ord_info %>%
  filter(cust_nm == "Pearl")
ord_info %>%
  filter(state == "Maharashtra")
ord_info %>%
  filter(!state == "Maharasthtra")
ord_info %>%
  filter(state == "Maharashtra" &
           city == "Pune")
ord_info %>%
  filter(state == "Maharashtra" ,
           city == "Pune")
ord_info %>%
  filter(city == "Pune" |
           city == "Mumbai" |
           city == "Delhi") %>%
  print(n=1000)

ord_info %>%
  filter(city %in% c('Pune', 'Mumbai', 'Delhi')) %>%
  print(n=100)

my_city <- c('Pune', 'Mumbai', 'Delhi')

ord_info %>%
  filter(city %in% my_city) %>%
  print(n=100)

ord_info %>%
  print(n=3000)

ord_info <- ord_info %>%
  mutate(cust_nm = ifelse(nchar(cust_nm) < 1, NA, cust_nm))

ord_info %>%
  print(n=1000)
ord_info %>%
  filter(!cust_nm %>% is.na)
ord_info %>%
  filter(!cust_nm %>% is.na,
         state %in% c("Gujarat", "Bihar"))

ord_info %>%
  filter(state %>% str_detect("adh")) %>%
  print(n=1000)

ord_info %>%
  filter(state %>% str_detect(" "))
ord_prd %>%
  filter(ord_amt > 5000)
ord_prd %>%
  filter(ord_amt == 100)
ord_prd %>%
  filter(ord_amt != 100)
ord_prd %>%
  filter(ord_profit == 100)

# practice

ord_info %>%
  filter(cust_nm == "Pooja")
ord_info %>%
  filter(cust_nm == "Pooja",
         state == "Goa")
ord_info %>%
  filter(cust_nm == "Pooja" & state == "Goa")
ord_prd %>%
  filter(catg_1 == "Clothing")
ord_prd %>%
  filter(catg_1 == "Clothing",
         catg_2 == "T-Shirt",
         qty >= 10)
ord_prd %>%
  filter(catg_1 == "Clothing",
         catg_2 == "T-Shirt")

ord_prd %>%
  filter(catg_1 == "Clothing",
         catg_2 == "T-shirt",
         qty >= 10)

ord_prd %>%
  filter(ord_amt >= 1000,
         !ord_profit >= 0)

ord_prd %>%
  filter(ord_amt >= 1000,
         ord_profit < 0,
         catg_1 %in% c("Clothing", "Electronics"))
ord_prd %>%
  filter(ord_amt >= 1000,
         ord_profit < 0,
         catg_1 == "Clothing" | catg_1 == "Electronics")
ord_prd %>%
  filter(catg_2 %>% str_detect("P"),
         ord_profit == 0,
         qty <= 2)
