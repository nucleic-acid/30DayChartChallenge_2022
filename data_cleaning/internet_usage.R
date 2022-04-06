# Cleaning of the internet usage data
# data obtained from https://ourworldindata.org/technology-adoption on 2022-04-04

library(tidyverse)

iusers <- read_csv(here::here("owid", "share-of-individuals-using-the-internet.csv")) |> 
  janitor::clean_names() |> 
  filter(!is.na(code)) |> 
  filter(str_detect(code, "^OWID", negate = TRUE))

write_csv(iusers, file = here::here("owid", "internet_cleaned.csv"))

