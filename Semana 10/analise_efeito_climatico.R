set.seed(123)
library(tidyverse)
library(lubridate)
library(kpodclustr)
library(tidytext) 
library(tidyverse)
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA\Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/EMBRAPA2/input/wdata_atual.csv") %>% mutate(fo_grupo = as.character(fo_grupo),
                                                                                      fs_grupo = as.character(fs_grupo))


### Moda do grupo de cada genotipo, levando em consideração o ano e efeito climático
quantidade <- wdata %>% group_by(ano_colheita, genotipo, ONI_C, fo_grupo) %>% summarize(quantidade = n()) %>%
group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade)) %>% filter(quantidade == maximo) %>%
distinct(ano_colheita, genotipo, .keep_all = TRUE) %>% arrange(genotipo)

filtro <- quantidade %>% group_by(genotipo) %>% summarize(quantidade = n()) %>% filter(quantidade >= 3)

quantidade <- quantidade %>% filter(genotipo %in% filtro$genotipo)

### Criação dos índices
for (indice in 1:nrow(quantidade)){
  ano <- quantidade[indice, "ano_colheita"]
  ano_anterior <- quantidade[indice - 1, "ano_colheita"]
  genotipo <- quantidade[indice, "genotipo"]
  genotipo_anterior <- quantidade[indice - 1, "genotipo"]
  efeito_climatico <- quantidade[indice, "ONI_C"]
  efeito_climatico_anterior <- quantidade[indice - 1, "ONI_C"]
  grupo <- quantidade[indice, "fo_grupo"]
  grupo_anterior <- quantidade[indice - 1, "fo_grupo"]
  if (indice != 1){
    if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo != grupo_anterior) & (efeito_climatico_anterior != efeito_climatico)){
      quantidade[indice, "mudanca"] <- 1
      quantidade[indice, "nao_mudanca"] <- 0
    }else if(genotipo != genotipo_anterior){
      quantidade[indice, "mudanca"] <- 0
      quantidade[indice, "nao_mudanca"] <- 0
    }else if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo != grupo_anterior) & (efeito_climatico_anterior == efeito_climatico)){
      quantidade[indice, "mudanca"] <- 0
      quantidade[indice, "nao_mudanca"] <- 1
    }else if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo == grupo_anterior)){
      quantidade[indice, "mudanca"] <- 0
      quantidade[indice, "nao_mudanca"] <- 1
    }
  }else{
    quantidade[indice, "mudanca"] <- 0
    quantidade[indice, "nao_mudanca"] <- 0
  }
}

### Criação da porcentagem
quantidade <- quantidade %>% group_by(genotipo) %>% mutate(mudanca = mudanca / (n() - 1) * 100,
                                                           nao_mudanca = nao_mudanca / (n() - 1) * 100) %>%
group_by(genotipo) %>% summarize(mudanca = sum(mudanca), 
                                 nao_mudanca = sum(nao_mudanca)) %>% filter(!is.na(mudanca)) %>% mutate(media = (mudanca + nao_mudanca) / 2)


### Gráfico de Saltos do FO
x11()
ggplot(quantidade %>% filter(mudanca != 0), aes(reorder(genotipo, mudanca), mudanca, ymin = 0, ymax = mudanca)) + geom_point() + geom_pointrange() + 
theme_bw() + coord_flip() + labs(x = "Genótipo", y = "Porcentagem de vezes que houve salto", title = "Salto devido a efeito climático nas doenças FO") + 
geom_text(aes(label = round(mudanca, 2), hjust = -0.5, size = 2.5))



### Gráficos de Não Saltos FO
ggplot(quantidade %>% filter(nao_mudanca != 0), aes(reorder(genotipo, nao_mudanca), nao_mudanca, ymin = 0, ymax = nao_mudanca)) + geom_point() + geom_pointrange() + 
  theme_bw() + coord_flip() + labs(x = "Genótipo", y = "Porcentagem de vezes que não houve salto", title = "Não Salto devido a efeito climático nas doenças FO") + 
  geom_text(aes(label = round(nao_mudanca, 2), hjust = -0.5, size = 2.5))







### FS

### Criação da tabela com os grupos preditos por genótipo
quantidade1 <- wdata %>% group_by(ano_colheita, genotipo, ONI_C, fs_grupo) %>% summarize(quantidade = n()) %>%
  group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade)) %>% filter(quantidade == maximo) %>%
  distinct(ano_colheita, genotipo, .keep_all = TRUE) %>% arrange(genotipo)

### Criação dos índices
for (indice in 1:nrow(quantidade1)){
  ano <- quantidade1[indice, "ano_colheita"]
  ano_anterior <- quantidade1[indice - 1, "ano_colheita"]
  genotipo <- quantidade1[indice, "genotipo"]
  genotipo_anterior <- quantidade1[indice - 1, "genotipo"]
  efeito_climatico <- quantidade1[indice, "ONI_C"]
  efeito_climatico_anterior <- quantidade1[indice - 1, "ONI_C"]
  grupo <- quantidade1[indice, "fs_grupo"]
  grupo_anterior <- quantidade1[indice - 1, "fs_grupo"]
  if (indice != 1){
    if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo != grupo_anterior) & (efeito_climatico_anterior != efeito_climatico)){
      quantidade1[indice, "mudanca"] <- 1
      quantidade1[indice, "nao_mudanca"] <- 0
    }else if(genotipo != genotipo_anterior){
      quantidade1[indice, "mudanca"] <- 0
      quantidade1[indice, "nao_mudanca"] <- 0
    }else if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo != grupo_anterior) & (efeito_climatico_anterior == efeito_climatico)){
      quantidade1[indice, "mudanca"] <- 0
      quantidade1[indice, "nao_mudanca"] <- 1
    }else if((ano != ano_anterior) & (genotipo == genotipo_anterior) & (grupo == grupo_anterior)){
      quantidade1[indice, "mudanca"] <- 0
      quantidade1[indice, "nao_mudanca"] <- 1
    }
  }else{
    quantidade1[indice, "mudanca"] <- 0
    quantidade1[indice, "nao_mudanca"] <- 0
  }
}

### Criação das porcentagens dos indices
quantidade1 <- quantidade1 %>% group_by(genotipo) %>% mutate(mudanca = mudanca / (n() - 1),
                                                           nao_mudanca = nao_mudanca / (n() - 1)) %>%
  group_by(genotipo) %>% summarize(mudanca = sum(mudanca), 
                                   nao_mudanca = sum(nao_mudanca)) %>% filter(!is.na(mudanca)) %>% mutate(media = (mudanca + nao_mudanca) / 2)


### Saltos FS
x11()
ggplot(quantidade1 %>% filter(mudanca != 0), aes(reorder(genotipo, mudanca), mudanca, ymin = 0, ymax = mudanca)) + geom_point() + geom_pointrange() + 
  theme_bw() + coord_flip() + labs(x = "Genótipo", y = "Porcentagem de vezes que houve salto", title = "Salto devido a efeito climático na doença FS") + 
  geom_text(aes(label = round(mudanca, 2), hjust = -0.5, size = 2.5))



x11()
ggplot(quantidade1 %>% filter(nao_mudanca != 0), aes(reorder(genotipo, nao_mudanca), nao_mudanca, ymin = 0, ymax = nao_mudanca)) + geom_point() + geom_pointrange() + 
  theme_bw() + coord_flip() + labs(x = "Genótipo", y = "Porcentagem de vezes que não houve salto", title = "Não Salto devido a efeito climático na doença FS") + 
  geom_text(aes(label = round(nao_mudanca, 2), hjust = -0.5, size = 2.5))




### Calculo das Médias

media_mudanca_fo <- data.frame(nome = "fo_mudanca", valor = mean(quantidade$mudanca))
media_nao_mudanca_fo <- data.frame(nome = "fo_nao_mudanca", valor = mean(quantidade$nao_mudanca))

media_mudanca_fs <- data.frame(nome = "fs_mudanca", valor = mean(quantidade1$mudanca))
media_nao_mudanca_fs <- data.frame(nome = "fs_nao_mudanca", valor = mean(quantidade1$nao_mudanca))

tudo_junto <- rbind(media_mudanca_fo, media_nao_mudanca_fo, media_mudanca_fs, media_nao_mudanca_fs)


ggplot(tudo_junto, aes(reorder(nome, valor), valor, fill = nome)) + geom_col() + theme_bw() + 
geom_text(aes(label = round(valor, 2)))
