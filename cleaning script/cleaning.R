library(tidyverse)
library(janitor)
library(lubridate)

disposable_income <- read_csv(here::here("raw data/disposable income.csv"))
construction_wages <- read_csv(here::here("raw data/house-building_construction_wages_indices.csv"))
unemployment <- readxl::read_xls(here::here("raw data/Labour market - Age Group (not seasonally adjusted).xls"))
credit <- read_csv(here::here("raw data/LPMB86L  Bank of England  Credit.csv"))
money_supply <- read_csv(here::here("raw data/M1 money supply.csv"))
wages <- read_csv("raw data/Wages until 2021.csv")
gdp <- read_csv(here::here("raw data/GDP ONS.csv"))
houses_built <- readxl::read_xlsx(here::here("raw data/ukhousebuilding.xlsx"), sheet = 9)
interest_rates <- read_csv(here::here("raw data/interest rates.csv"))
housing_prices <- read_csv(here::here("raw data/Average-prices-2022-09.csv"))

unemployment_values <- unemployment$...5[101:316]
unemployment_dates <- unemployment$`Table A05: Labour market by age group: People by economic activity and age (not seasonally adjusted)`[101:316]

unemployment <- tibble(date = unemployment_dates, rate = unemployment_values) %>% 
  mutate(year = str_extract(date, "[0-9]{4}$")) %>% 
  group_by(year) %>% 
  summarise(average_yearly_unemployment = mean(as.numeric(rate))) %>% 
  select(year, average_yearly_unemployment)

construction_values <- unlist(strsplit(construction_wages$`2023-01-09T13:20:00+00:00`[6],","))
construction_times <- unlist(strsplit(construction_wages$`2023-01-09T13:20:00+00:00`[5],","))
con_values <- construction_values[2:78]
con_times <- construction_times[2:78]

construction <- tibble(time = con_times, wages = con_values) %>% 
  mutate(year = str_extract(time, "^[0-9]{4}")) %>% 
  filter(year < 2018) %>% 
  group_by(year) %>% 
  summarise(average_yearly_construction_wages = mean(as.numeric(wages))) %>% 
  select(year, average_yearly_construction_wages)

gdp_ph <- gdp %>% 
  select(year = Title, `Gross domestic product (Average) per head, CVM market prices: SA`) %>% 
  filter(year %in% 2000:2017)

credit <- credit %>% 
  mutate(date = dmy(Date),
         credit = `Monthly amount of total sterling total repayments of secured lending by individuals (in sterling millions) seasonally adjusted              [a] [b]             LPMB86L`) %>% 
  arrange(date) %>% 
  filter(date > as.Date("2000-01-01") & date < as.Date("2017-12-31")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_yearly_credit = mean(credit)) %>% 
  select(year, average_yearly_credit)

disposable_income <- disposable_income %>% 
  mutate(year = Title, disposable_income = `HH & NPISH (S.14 + S.15): Disposable income, gross (B.6g): Uses/Resources: Current price: £m: NSA`) %>% 
  filter(year %in% 2000:2017) %>% 
  select(year, disposable_income)

houses_built <- houses_built %>% 
  mutate(year = ...2,
         houses = ...7) %>% 
  select(year, houses) %>% 
  mutate(year = str_extract(year, "^[0-9]{4}")) %>%
  mutate(year = as.numeric(year)  + 1) %>% 
  filter(year %in% 2000:2017)

interest_rates <- interest_rates %>% 
  mutate(date = dmy(`Date Changed`), rate = Rate) %>% 
  arrange(date) %>% 
  filter(date > as.Date("2000-01-01") & date < as.Date("2017-12-31")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_interest_rate = mean(rate)) %>% 
  select(year, average_interest_rate)

money_supply <- money_supply %>% 
  mutate(date = ymd(DATE), supply = MANMM101GBM189S) %>% 
  filter(date > as.Date("2000-01-01") & date < as.Date("2017-12-31")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_supply = mean(supply)) %>% 
  select(year, average_supply)

wages <- wages %>% 
  filter(COUNTRY == "GBR", SERIES == "USDPPP", Time %in% 2000:2017) %>% 
  select(year = Time, Value)


years <- tibble(year = 2010:2015, average_interest_rate = 0.5)
one_year <- tibble(year = 2002, average_interest_rate = 4)


interest_rates_filled <- interest_rates %>% 
  rows_insert(years) %>% 
  rows_insert(one_year) %>% 
  arrange(year)
  
#impute interest rate for missing values since the interest rate never changed in those years

housing_prices <- housing_prices %>% 
  mutate(year = str_extract(Date, "^[0-9]{4}")) %>% 
  filter(year > 1999 & year < 2018) %>% 
  filter(Region_Name == "United Kingdom") %>% 
  group_by(year) %>% 
  summarise(average_price = mean(Average_Price)) %>% 
  select(year, average_price)



df <- tibble(year = construction$year, housing_prices = housing_prices$average_price, construction_wages = construction$average_yearly_construction_wages,
             credit = credit$average_yearly_credit, disposable_income = disposable_income$disposable_income,
             gdp_ph = gdp_ph$`Gross domestic product (Average) per head, CVM market prices: SA`,
             houses_built = houses_built$houses, interest_rates = interest_rates_filled$average_interest_rate,
             money_supply = money_supply$average_supply, wages = wages$Value, unemployment = unemployment$average_yearly_unemployment)



write_csv(df, here::here("clean data/housing_prices_data.csv"))


