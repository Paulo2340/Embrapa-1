set.seed(123)
library(tidyverse)
library(lubridate)
library(kpodclustr)
library(tidytext) 
library(tidyverse)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/EMBRAPA2/input/banco_com_efeitos.csv") %>% mutate(fo_grupo = as.character(fo_grupo),
                                                                                            fs_grupo = as.character(fs_grupo)) %>% mutate(predito = case_when(predito == 1 ~ "Neutro",
                                                                                                                                                    predito == 2 ~ "El Nino",
                                                                                                                                                    predito == 3 ~ "La Nina"))
### Grupo 2

grupo_1 <- wdata %>% filter(predito == "La Nina")
grupo_1 <- grupo_1[,5:52]


### Grupo 1 FO
tabela_sumarizada <- grupo_1 %>% group_by(ano_colheita, genotipo, fo_grupo) %>% 
  summarize(quantidade = n()) %>% group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% distinct(ano_colheita, genotipo, .keep_all = TRUE) %>% arrange(genotipo)
tabela_sumarizada$mudanca <- NA
tabela_sumarizada$nao_mudanca <- NA
tabela_sumarizada[1,"mudanca"] <- 0
tabela_sumarizada[1,"nao_mudanca"] <- 0


### Criação dos índices para grupo de efeito climático 1
for (c in 2:nrow(tabela_sumarizada)){
  genotipo <- tabela_sumarizada[c, 2] == tabela_sumarizada[c-1, 2]
  ano <- tabela_sumarizada[c,1] != tabela_sumarizada[c-1,1]
  if (genotipo & ano){
    if(tabela_sumarizada[c,3] != tabela_sumarizada[c-1,3]){
      tabela_sumarizada[c,"mudanca"] <- 1
      tabela_sumarizada[c,"nao_mudanca"] <- 0
    }else{
      tabela_sumarizada[c,"mudanca"] <- 0
      tabela_sumarizada[c,"nao_mudanca"] <- 1
    }
  }else{
    tabela_sumarizada[c,"mudanca"] <- 0
    tabela_sumarizada[c,"nao_mudanca"] <- 0
  }
}

### Calculo das Porcetagens
tabela_sumarizada <- tabela_sumarizada %>% select(-c(quantidade, maximo))
contar <- tabela_sumarizada %>% group_by(genotipo) %>% summarise(quantidade = n()) %>% filter(quantidade > 1) %>% select(genotipo)
tabela_sumarizada1 <- tabela_sumarizada %>% filter(genotipo %in% contar$genotipo) %>%
  group_by(genotipo) %>% summarize(quantidade = n(), soma_mudanca = sum(mudanca) / (quantidade -1),
                                   soma_nao_mudanca = sum(nao_mudanca) / (quantidade - 1))


x11()

### Mudança FO
ggplot(tabela_sumarizada1 %>% filter(soma_mudanca != 0), aes(reorder(genotipo, soma_mudanca), soma_mudanca, ymin = 0, ymax = soma_mudanca)) + geom_pointrange() + 
  geom_point() + theme_bw() + theme(legend.position = "none") + coord_flip() + 
  labs(title = "Porcentagem de Mudança para Doença FO do efeito La Nina", x = "Genotipo", y = "Porcentagem de mudança") + geom_text(aes(label = round(soma_mudanca, 2)), hjust = -0.5)

x11()
ggplot(tabela_sumarizada1 %>% filter(soma_nao_mudanca != 0), aes(reorder(genotipo, soma_nao_mudanca), soma_nao_mudanca, ymin = 0, ymax = soma_nao_mudanca)) + geom_pointrange() + 
  geom_point() + theme_bw() + theme(legend.position = "none") + coord_flip() + 
  labs(title = "Porcentagem de Não Mudança para Doença FO do efeito La Nina", x = "Genotipo", y = "Porcentagem de não mudança") + geom_text(aes(label = round(soma_nao_mudanca, 2)), hjust = -0.5)



### FS
tabela_sumarizada <- grupo_1 %>% group_by(ano_colheita, genotipo, fs_grupo) %>% 
  summarize(quantidade = n()) %>% group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% distinct(ano_colheita, genotipo, .keep_all = TRUE) %>% arrange(genotipo)
tabela_sumarizada$mudanca <- NA
tabela_sumarizada$nao_mudanca <- NA
tabela_sumarizada[1,"mudanca"] <- 0
tabela_sumarizada[1,"nao_mudanca"] <- 0


### Criação dos índices para grupo de efeito climático 1
for (c in 2:nrow(tabela_sumarizada)){
  genotipo <- tabela_sumarizada[c, 2] == tabela_sumarizada[c-1, 2]
  ano <- tabela_sumarizada[c,1] != tabela_sumarizada[c-1,1]
  if (genotipo & ano){
    if(tabela_sumarizada[c,3] != tabela_sumarizada[c-1,3]){
      tabela_sumarizada[c,"mudanca"] <- 1
      tabela_sumarizada[c,"nao_mudanca"] <- 0
    }else{
      tabela_sumarizada[c,"mudanca"] <- 0
      tabela_sumarizada[c,"nao_mudanca"] <- 1
    }
  }else{
    tabela_sumarizada[c,"mudanca"] <- 0
    tabela_sumarizada[c,"nao_mudanca"] <- 0
  }
}


### Calculo das Porcetagens
tabela_sumarizada <- tabela_sumarizada %>% select(-c(quantidade, maximo))
contar <- tabela_sumarizada %>% group_by(genotipo) %>% summarise(quantidade = n()) %>% filter(quantidade > 1) %>% select(genotipo)
tabela_sumarizada1 <- tabela_sumarizada %>% filter(genotipo %in% contar$genotipo) %>%
  group_by(genotipo) %>% summarize(quantidade = n(), soma_mudanca = sum(mudanca) / (quantidade -1),
                                   soma_nao_mudanca = sum(nao_mudanca) / (quantidade - 1))

x11()
### Mudança FS
ggplot(tabela_sumarizada1 %>% filter(soma_mudanca != 0), aes(reorder(genotipo, soma_mudanca), soma_mudanca, ymin = 0, ymax = soma_mudanca)) + geom_pointrange() + 
  geom_point() + theme_bw() + theme(legend.position = "none") + coord_flip() + 
  labs(title = "Porcentagem de Mudança para Doença FS do Efeito La Nina", x = "Genotipo", y = "Porcentagem de mudança") + geom_text(aes(label = round(soma_mudanca, 2)), hjust = -0.5)

x11()
### Nao Mudança FS
ggplot(tabela_sumarizada1 %>% filter(soma_nao_mudanca != 0), aes(reorder(genotipo, soma_nao_mudanca), soma_nao_mudanca, ymin = 0, ymax = soma_nao_mudanca)) + geom_pointrange() + 
  geom_point() + theme_bw() + theme(legend.position = "none") + coord_flip() + 
  labs(title = "Porcentagem de Não Mudança para Doença FS do efeito La Nina", x = "Genotipo", y = "Porcentagem de não mudança") + geom_text(aes(label = round(soma_nao_mudanca, 2)), hjust = -0.5)
