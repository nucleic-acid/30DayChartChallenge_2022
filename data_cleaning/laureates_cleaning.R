# Data of Nobel Prize Laureates
# Accessed 2022-03-23

library(tidyverse)

# API call according to Open API documentation for API version 2.1:
# https://app.swaggerhub.com/apis/NobelMedia/NobelMasterData/2.1
laureates <- read_csv("http://api.nobelprize.org/2.1/laureates?limit=1000&format=csv")

write_csv(laureates, file = here::here("nobel", "laureates.csv"))