## This is the cleaning script for the Shakespeare Data.
## Original data was obtained on 2022-03-22.

# load libraries
library(tidyverse)

# Read data downloaded from kaggle by user LiamLarsen (Version 4)
tbl_pear <- read_csv(here::here("shakespeare", "kaggle", "Shakespeare_data.csv"))

# Add missing data for Henry IV pt. 2
henryIV2 <- read_csv("https://raw.githubusercontent.com/aodhanlutetiae/shakespeare/master/HenryIV_part2.csv") %>% 
  select(-`...1`)

shakespeare_raw <- rbind(tbl_pear, henryIV2)

# remove stage directions and keep only spoken lines.
# also split the ActSceneLine Column into three separate variables
shakespeare_spoken <- shakespeare_raw %>% filter(!is.na(ActSceneLine)) |>
  tidyr::separate(ActSceneLine, into = c("Act", "Scene", "spoken_line"), sep = "\\.", convert = TRUE) |> 
  select(-Dataline) |> 
  mutate(
    Play = ifelse(Play == "macbeth", "Macbeth", Play),
    Play = ifelse(Play == "Henry IV, part two", "Henry IV, Part 2", Play)
  )

write_csv(shakespeare_spoken, here::here("shakespeare", "shakespeare_tidy.csv"))


# provide a dataset WITH the stage directions

shakespeare_complete <- shakespeare_raw |>
  tidyr::separate(ActSceneLine, into = c("Act", "Scene", "spoken_line"), sep = "\\.", convert = TRUE) |> 
  select(-Dataline) |> 
  mutate(
    Play = ifelse(Play == "macbeth", "Macbeth", Play),
    Play = ifelse(Play == "Henry IV, part two", "Henry IV, Part 2", Play)
  )

write_csv(shakespeare_complete, here::here("shakespeare", "shakespeare_complete.csv"))