## This is the cleaning script for the World Bank Forestation.
## Original data was obtained on 2022-03-23.

library(tidyverse)
forestation_raw <- read_csv("worldbank/API_AG.LND.FRST.ZS_DS2_en_csv_v2_3731376.csv", skip = 3) |> 
  janitor::clean_names()

forestation_tidy <- forestation_raw |> 
  filter(!is.na(x2000) & !is.na(x2020)) |> 
  pivot_longer(-c(1:4), names_to = "Year", values_to = "surface", names_prefix = "^x") |> 
  filter(Year %in% c(2000, 2020)) |> 
  select(country_name, country_code, Year, surface)

write_csv(forestation_tidy, file = here::here("worldbank", "forestation_tidy.csv"))