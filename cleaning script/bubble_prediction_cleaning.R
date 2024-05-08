library(tidyverse)
library(lubridate)

disposable_income <- read_csv(here::here("raw data/disposable income.csv"))
money_supply <- read_csv(here::here("raw data/M1 money supply.csv"))
houses_built <- readxl::read_xlsx(here::here("raw data/ukhousebuilding.xlsx"), sheet = 9)
interest_rates <- read_csv(here::here("raw data/interest rates.csv"))
gdp <- read_csv(here::here("raw data/GDP ONS.csv"))
house_price_ratio_wages <- readxl::read_xlsx(here::here("raw data/All buyers HPER by region affordability ratio.xlsx"))
house_prices <- readxl::read_xls(here::here("raw data/UK_house_price_since_1952.xls"))
mortgage_interest <- read_csv(here::here("raw data/mortgage interest payments.csv"))
mortgage_ratio_wages <- readxl::read_xlsx(here::here("raw data/mrtg payments ratio of pay by region.xlsx"))

disposable_income <- disposable_income %>% 
  mutate(year = Title, disposable_income = `HH & NPISH (S.14 + S.15): Disposable income, gross (B.6g): Uses/Resources: Current price: £m: NSA`) %>% 
  filter(year %in% 2020:2021) %>% 
  select(year, disposable_income)

money_supply <- money_supply %>% 
  mutate(date = ymd(DATE), supply = MANMM101GBM189S) %>% 
  filter(date > as.Date("2020-01-01") & date < as.Date("2022-01-01")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_supply = mean(supply)) %>% 
  select(year, average_supply)

houses_built <- houses_built %>% 
  mutate(year = ...2,
         houses = ...7) %>% 
  select(year, houses) %>% 
  mutate(year = str_extract(year, "^[0-9]{4}")) %>% 
  mutate(year = as.numeric(year)  + 1) %>% 
  filter(year %in% 2020:2021)

interest_rates <- interest_rates %>% 
  mutate(date = dmy(`Date Changed`), rate = Rate) %>% 
  arrange(date) %>% 
  filter(date > as.Date("2020-01-01") & date < as.Date("2022-01-01")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_interest_rate = mean(rate)) %>% 
  select(year, average_interest_rate)

years <- tibble(year = 2010:2015, average_interest_rate = 0.5)
one_year_2002 <- tibble(year = 2002, average_interest_rate = 4)
one_year_2019 <- tibble(year = 2019, average_interest_rate = 0.75)

#impute interest rate for missing values since the interest rate never changed in those years

interest_rates_filled <- interest_rates %>% 
  rows_insert(years) %>% 
  rows_insert(one_year_2002) %>% 
  rows_insert(one_year_2019) %>% 
  arrange(year)

gdp_ph <- gdp %>% 
  select(year = Title, `Gross domestic product (Average) per head, CVM market prices: SA`) %>% 
  filter(year %in% 2020:2021)

house_price_ratio_wages <- house_price_ratio_wages %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2021) %>% 
  group_by(year) %>% 
  summarise(average_hprw = mean(UK)) %>% 
  select(year, average_hprw)

house_prices <- house_prices %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2021) %>% 
  group_by(year) %>% 
  summarise(average_hp_change = mean(as.numeric(...4))) %>% 
  select(year, average_hp_change)

mortgage_interest <- mortgage_interest %>% 
  mutate(year = Title) %>% 
  filter(year %in% 2020:2021) %>% 
  select(year, `RPI: housing: mortgage interest payments (Jan 1987=100)`)

mortgage_ratio_wages <- mortgage_ratio_wages %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2021) %>% 
  group_by(year) %>% 
  summarise(average_mort_ratio_wages = mean(as.numeric(UK))) %>% 
  select(year, average_mort_ratio_wages)


df <- tibble(year = mortgage_ratio_wages$year, disposable_income = disposable_income$disposable_income,
             money_supply = money_supply$average_supply, houses_built = houses_built$houses,
             interest_rates = interest_rates$average_interest_rate, gdp = gdp_ph$`Gross domestic product (Average) per head, CVM market prices: SA`,
             house_price_ratio_wages = house_price_ratio_wages$average_hprw, house_prices = house_prices$average_hp_change,
             mortgage_interest = mortgage_interest$`RPI: housing: mortgage interest payments (Jan 1987=100)`, 
             mortgage_ratio_wages = mortgage_ratio_wages$average_mort_ratio_wages)

write_csv(df, "clean data/bubble_data_prediction.csv")
