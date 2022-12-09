set.seed(123)
library(tidyverse)
library(kpodclustr)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read.csv("C:/Users/paogr/Desktop/EMBRAPA2/input/V29.csv", sep = ";") %>% select(-X)

