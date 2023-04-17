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

### Gráfico de pontos
x11()
ggplot(wdata_fo, aes(reorder(genotipo, porcentagem), porcentagem, ymin = 0, ymax = porcentagem, color = fo_grupo)) + 
geom_point() + geom_pointrange() + coord_flip() + facet_wrap(vars(fo_grupo), scales = "free", drop = TRUE) + theme_bw() + 
labs(x = "Genótipos", y = "Porcentagem", title = 'Porcentagem de vezes em que cada genótipo aparece em cada grupo modal de doença FO')

#### FS


wdata_fs <- wdata[,3:50] %>% mutate(fs_grupo = as.character(fs_grupo)) %>% group_by(genotipo, fs_grupo) %>%
  summarize(quantidade = n()) %>% group_by(genotipo) %>% mutate(soma = sum(quantidade), maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% mutate(porcentagem = maximo / soma * 100) %>% dplyr::select(genotipo, fs_grupo, porcentagem) %>%
  distinct(genotipo, .keep_all = TRUE)


x11()
ggplot(wdata_fs %>% filter(fs_grupo != "Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, ymin = 0, ymax = porcentagem, color = fs_grupo)) + 
  geom_point() + geom_pointrange() + coord_flip() + facet_wrap(vars(fs_grupo), scales = "free", drop = TRUE) + theme_bw() + 
  labs(x = "Genótipos", y = "Porcentagem", title = 'Porcentagem de vezes em que cada genótipo aparece em cada grupo modal de doença FS') + 
theme(axis.text.y = element_text(size = 7.5))



#### Tolerante


ggplot(wdata_fs %>% filter(fs_grupo == "Tolerante"), aes(reorder(genotipo, porcentagem), porcentagem, ymin = 0, ymax = porcentagem, color = fs_grupo)) + 
  geom_point() + geom_pointrange() + coord_flip() + facet_wrap(vars(fs_grupo), scales = "free", drop = TRUE) + theme_bw() + 
  labs(x = "Genótipos", y = "Porcentagem", title = 'Porcentagem de vezes em que cada genótipo aparece em cada grupo modal de doença FS') + 
  theme(axis.text.y = element_text(size = 7.5))
