library(lubridate)
library(dplyr)
library(tidyverse)
library(fuzzyjoin)
library(visdat)
wdata <- read_csv("C:/Users/paogr/Documents/juntos_teste.csv")


wdata_fo <- wdata %>% select(...1:FO9) %>% rename(indice = ...1) %>% 
  pivot_longer(c("FO1", "FO2", "FO3", "FO4", "FO5", "FO6", "FO7",
                 "FO8", "FO9"))