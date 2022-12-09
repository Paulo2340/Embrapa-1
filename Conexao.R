set.seed(123)
library(tidyverse)
library(kpodclustr)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/EMBRAPA2/input/banco_ajustado.csv") %>%
  rename("index" = ...2) %>% select(-...1) %>% arrange(index)
wdata <- wdata %>% mutate(data_colheita = data_semeadura + lubridate::ddays(90)) %>%
  select(index:data_semeadura, data_colheita, safra:FS11)
  
write.csv(wdata, "banco_ajustado.csv")