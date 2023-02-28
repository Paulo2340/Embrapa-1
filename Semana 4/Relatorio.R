source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
library(tidyverse)


wdata <- read_csv("C:/Users/paogr/Documents/dados_limpos.csv")

##### Filtrando por quantidade de notas minimas dadas 

wdata_mais_quinto <- wdata %>% filter(Nao_Nulos_Fo >= 5, Nao_Nulos_Fs >= 5)
wdata_mais_quarto <- wdata %>% filter(Nao_Nulos_Fo >= 4, Nao_Nulos_Fs >= 4)



wdata_mais_quinto %>% select(genotipo) %>% unique() %>% n()
wdata_mais_quarto %>% select(genotipo) %>% unique() %>% n()

filtrado_quinto <- wdata_mais_quinto %>% group_by(safra, genotipo) %>% 
summarize(quantidade = n()) %>% group_by(genotipo) %>% summarize(quant = n())


filtrado_quarto <- wdata_mais_quarto %>% group_by(safra, genotipo) %>% 
  summarize(quantidade = n()) %>% group_by(genotipo) %>% summarize(quant = n())
