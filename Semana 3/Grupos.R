set.seed(12432)

library(tidyverse)

source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")

source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")

wdata <- read.csv("C:/Users/paogr/Desktop/EMBRAPA2/input/doenca_v1.csv", sep = ";")

library(cluster)

library(stringr)

library(rpart)

library(fdm2id)

library(rpart.plot)

wdata <- readRDS("C:/Users/paogr/Documents/Kmeans")

for (indice in 1:length(colnames(wdata))){
  print(colnames(wdata)[indice])
  print(length(unique(wdata[,indice])))
}


resultados <- wdata %>% mutate(total = n()) %>% group_by(cidade, tipo_de_grao, data_semeadura, safra, predito_fs) %>% 
  summarize(quantidade = n()) %>% unique()

modelo_teste <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata)

rpart.plot(modelo_teste)


genot <- wdata$genotipo %>% unique()
primeiro <- genot[1:9]
segundo <- genot[10:18]
terceiro <- genot[20:28]
quarto <- genot[30:38]
quinto <- genot[40:48]
sexto <- genot[50:58]
setimo <- genot[60:68]
oitavo <- genot[69:72]


modelo_primeiro <- rpart(predito_fo ~ cidade + tipo_de_grao + data_semeadura + safra + genotipo, method = "class", data = wdata %>% 
                        filter(genotipo %in% primeiro))

rpart.plot(modelo_primeiro)


modelo_segundo <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra + genotipo, method = "class", data = wdata %>% 
                        filter(genotipo %in% segundo))
View(wdata %>% filter(genotipo %in% segundo) %>% group_by(cidade, tipo_de_grao, data_semeadura, safra, genotipo, predito_fs) %>% summarize(total = n()))


modelo_terceiro <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                          filter(genotipo %in% terceiro))

modelo_quarto <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                          filter(genotipo %in% quarto))

modelo_quinto <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                         filter(genotipo %in% quinto))

modelo_sexto <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                         filter(genotipo %in% sexto))

modelo_setimo <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                         filter(genotipo %in% setimo))

modelo_oitavo <- rpart(predito_fs ~ cidade + tipo_de_grao + data_semeadura + safra, method = "class", data = wdata %>% 
                         filter(genotipo %in% oitavo))