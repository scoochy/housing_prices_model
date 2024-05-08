library(tidyverse)


housing_prices <- read_csv(here::here("raw data/Average-prices-2022-09.csv"))
wages <- read_csv("raw data/Wages until 2021.csv")
gdp <- read_csv(here::here("raw data/GDP ONS.csv"))
money_supply <- read_csv(here::here("raw data/M1 money supply.csv"))

gdp_ph <- gdp %>% 
  select(year = Title, `Gross domestic product (Average) per head, CVM market prices: SA`) %>% 
  filter(year %in% 2018:2021)

wages <- wages %>% 
  filter(COUNTRY == "GBR", SERIES == "USDPPP", Time %in% 2018:2021) %>% 
  select(year = Time, Value)

money_supply <- money_supply %>% 
  mutate(date = ymd(DATE), supply = MANMM101GBM189S) %>% 
  filter(date > as.Date("2017-12-31") & date < as.Date("2022-01-01")) %>% 
  group_by(year = year(date)) %>% 
  summarise(average_supply = mean(supply)) %>% 
  select(year, average_supply)

housing_prices <- housing_prices %>% 
  mutate(year = str_extract(Date, "^[0-9]{4}")) %>% 
  filter(year > 2017 & year < 2022) %>% 
  filter(Region_Name == "United Kingdom") %>% 
  group_by(year) %>% 
  summarise(average_price = mean(Average_Price)) %>% 
  select(year, average_price)

df <- tibble(year = gdp_ph$year, housing_prices = housing_prices$average_price, gdp = gdp_ph$`Gross domestic product (Average) per head, CVM market prices: SA`,
             wages = wages$Value, money_supply = money_supply$average_supply)

write_csv(df, here::here("clean data/housing_prices_data_pred.csv"))
