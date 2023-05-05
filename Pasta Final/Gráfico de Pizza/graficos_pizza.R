set.seed(123)
library(tidyverse)
library(lubridate)
library(kpodclustr)
library(tidytext) 
library(tidyverse)
library("cowplot")
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/banco_com_efeitos2.csv")




### FO

###Criação da tabela da moda do FO 
wdata_fo <- wdata[,3:50] %>% mutate(fo_grupo = as.character(fo_grupo)) %>% group_by(genotipo, fo_grupo) %>%
  summarize(quantidade = n()) %>% group_by(genotipo) %>% mutate(soma = sum(quantidade), maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% mutate(porcentagem = maximo / soma * 100) %>% dplyr::select(genotipo, fo_grupo, porcentagem) %>%
  distinct(genotipo, .keep_all = TRUE)
### Criação do gráfico da Moda para FO


### Muito Tolerante
x11()
ggplot(wdata_fo %>% filter(fo_grupo == "Muito Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                           high  = "red") + 
   labs(x = "",
   y = "Frêquencia Relativa(%)",
  title = paste("Intensidade percentual dos genotipos classificados como Muito Tolerante na Doença FO"),
  fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                axis.title.y = element_blank()) 
x11()
### Tolerante
ggplot(wdata_fo %>% filter(fo_grupo == "Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Tolerante na Doença FO"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()) 
x11()
### Muito Pouco Tolerante 

ggplot(wdata_fo %>% filter(fo_grupo == "Muito Pouco Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Muito Pouco Tolerante na Doença FO"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()) 



#### FS


wdata_fs <- wdata[,3:50] %>% mutate(fs_grupo = as.character(fs_grupo)) %>% group_by(genotipo, fs_grupo) %>%
  summarize(quantidade = n()) %>% group_by(genotipo) %>% mutate(soma = sum(quantidade), maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% mutate(porcentagem = maximo / soma * 100) %>% dplyr::select(genotipo, fs_grupo, porcentagem) %>%
  distinct(genotipo, .keep_all = TRUE)


x11()
### Muito Tolerante
ggplot(wdata_fs %>% filter(fs_grupo == "Muito Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Muito Tolerante na Doença FS"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()) 
x11()
### Tolerante
ggplot(wdata_fs %>% filter(fs_grupo == "Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Tolerante na Doença FS"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank()) 
x11()
### Muito Pouco Tolerante 

ggplot(wdata_fs %>% filter(fs_grupo == "Muito Pouco Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Muito Pouco Tolerante na Doença FS"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank())

### Pouco Tolerante
x11()
ggplot(wdata_fs %>% filter(fs_grupo == "Pouco Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, fill = porcentagem)) + 
  geom_bar(stat = "identity") + coord_polar("x", 0) + theme_minimal() + scale_fill_gradient(low = "blue", 
                                                                                            high  = "red") + 
  labs(x = "",
       y = "Frêquencia Relativa(%)",
       title = paste("Intensidade percentual dos genotipos classificados como Pouco Tolerante na Doença FS"),
       fill = "Porcentagem") + theme(axis.text.y = element_blank(),
                                     axis.title.y = element_blank())
