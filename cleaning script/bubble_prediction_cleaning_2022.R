library(tidyverse)
library(lubridate)


houses_built <- readxl::read_xlsx(here::here("raw data/ukhousebuilding.xlsx"), sheet = 9)
house_price_ratio_wages <- readxl::read_xlsx(here::here("raw data/All buyers HPER by region affordability ratio.xlsx"))
house_prices <- readxl::read_xls(here::here("raw data/UK_house_price_since_1952.xls"))
mortgage_ratio_wages <- readxl::read_xlsx(here::here("raw data/mrtg payments ratio of pay by region.xlsx"))


houses_built <- houses_built %>% 
  mutate(year = ...2,
         houses = ...7) %>% 
  select(year, houses) %>% 
  mutate(year = str_extract(year, "^[0-9]{4}")) %>% 
  mutate(year = as.numeric(year)  + 1) %>% 
  filter(year %in% 2020:2022)


house_price_ratio_wages <- house_price_ratio_wages %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2022) %>% 
  group_by(year) %>% 
  summarise(average_hprw = mean(UK)) %>% 
  select(year, average_hprw)

house_prices <- house_prices %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2022) %>% 
  group_by(year) %>% 
  summarise(average_hp_change = mean(as.numeric(...4))) %>% 
  select(year, average_hp_change)


mortgage_ratio_wages <- mortgage_ratio_wages %>% 
  mutate(year = str_extract(...1, "[0-9]{4}$")) %>% 
  filter(year %in% 2020:2022) %>% 
  group_by(year) %>% 
  summarise(average_mort_ratio_wages = mean(as.numeric(UK))) %>% 
  select(year, average_mort_ratio_wages)


df <- tibble(year = mortgage_ratio_wages$year, houses_built = houses_built$houses,
             house_price_ratio_wages = house_price_ratio_wages$average_hprw, house_prices = house_prices$average_hp_change,
             mortgage_ratio_wages = mortgage_ratio_wages$average_mort_ratio_wages)

write_csv(df, "clean data/bubble_data_prediction_2022.csv")
