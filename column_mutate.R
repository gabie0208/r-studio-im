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

ord_prd %>%
  select(-ord_cd, -ord_profit, -qty, -catg_1, -catg_2) %>%
  mutate(tag = Sys.time())

ord_prd %>%
  select(-ord_cd, -ord_profit, -qty, -catg_1, -catg_2) %>%
  mutate(tag = ord_amt *  10 - 100,
         tag1 = tag * 2,
         tag2 = tag1 * 4)

ord_prd %>%
  mutate(a = "mydate", b = 1,
         c = 2, d = "your date",
         e = ord_amt)

cust_info %>%
  select(cust_nm, age) %>%
  mutate(adult_yn = ifelse(age < 20, '미성년자', '성인')) %>%
  print(n=100)

ord_prd %>%
  mutate(ord_dc = ord_amt - ord_profit)
ord_info %>%
  mutate(ord_no = ord_cd %>% str_sub(3, 7)) %>%
  select(ord_cd, ord_no, ord_dt, cust_nm, state, city)

ord_prd %>%
  mutate(catg_1 = catg_1 %>% str_replace_all("Electronics", "eltr"))

ord_info %>%
  mutate(year = ord_dt %>% str_sub(7, 11))
ord_info %>%
  mutate(day = ord_dt %>% str_sub(4, 5))
ord_info %>%
  mutate(year = ord_dt %>% str_sub(7, 11),
         month = ord_dt %>% str_sub(1, 2),
         day = ord_dt %>% str_sub(4, 5))
ord_info %>%
  mutate(year = ord_dt %>% str_sub(7, 11),
         month = ord_dt %>% str_sub(1, 2),
         day = ord_dt %>% str_sub(4, 5),
         ord_dt_ymd = paste0(year, "-", month, "-", day))
ord_info %>%
  mutate(ord_ymd = ord_dt %>% dmy(),
         ord_ymd_1 = ord_ymd + weeks(1))
ord_info %>%
  mutate(ord_ymd = ord_dt %>% dmy(),
         ord_week = ord_ymd %>% week(),
         ord_dow_default = ord_dt %>% wday(),
         ord_dow_label = ord_dt %>% wday(label = T))
cust_info
cust_info %>%
  select(cust_nm, age) %>%
  mutate(adult_yn = ifelse(age < 20, '미성년자', '성인')) %>%
  print(n = 30)
cust_info %>%
  mutate(adult_yn = ifelse(age < 20, '10대',
                    ifelse(age < 30, '20대',
                    ifelse(age < 40, '30대', '40대 이상')))) %>%
  select(cust_nm, age, adult_yn)

gender_info <- cust_info %>%
  mutate(gender_simple = gender %>% str_sub(1, 1)) %>%
  select(cust_nm, gender_simple)
gender_info

tenure_info <-cust_info %>%
  filter(!tenure %>% is.na) %>%
  mutate(t_grp = ifelse(tenure <= 1, 'junior', 'senior'),
         gender_simple = gender %>% str_sub(1, 1)) %>%
  select(cust_nm, gender_simple, t_grp)
tenure_info

cust_info %>% count(tenure)

cust_info %>%
  mutate(t_grp = ifelse(tenure == 1, 'junior', 'senior'),
         gender_simple = gender %>% str_sub(1, 1),
         f_grp = ifelse(family == 1, '1인가구',
                 ifelse(family == 2, '2인가구',
                 ifelse(family == 3, '3인가구',
                 ifelse(family == 4, '대가구', 'many'))))) %>%
  select(cust_nm, gender_simple, t_grp, f_grp)

addr_info <- ord_info %>%
  mutate(addr = paste(state, city),
         addr2 = paste0(state, " ", city)) %>%
  select(cust_nm, addr)
addr_info

price_200_info <- ord_prd %>%
  mutate(price = ord_amt/qty) %>%
  filter(price >= 200)
price_200_info

dow_info <- ord_info %>%
  mutate(ord_dow = ord_dt %>% dmy %>% wday(label = T)) %>%
  select(ord_cd, ord_dow, cust_nm)
dow_info

cust_info %>%
  filter(!tenure %>% is.na,
         !family %>% is.na) %>%
  mutate(married = ifelse(nchar(married) < 1, 'no reply', married))

cust_info %>%
  mutate(id = cust_no,
         name = cust_nm) %>%
  select(-cust_no, -cust_nm)
cust_info %>%
  rename(id = cust_no,
         name = cust_nm)
cust_info %>%
  distinct(job)
