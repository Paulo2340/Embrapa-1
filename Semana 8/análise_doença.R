set.seed(123)
library(tidyverse)
library(lubridate)
library(kpodclustr)
library(tidytext) 
library(tidyverse)
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/banco_com_efeitos2.csv")


### FO

###Criação da tabela da moda do FO 
wdata_fo <- wdata[,3:50] %>% mutate(fo_grupo = as.character(fo_grupo)) %>% group_by(genotipo, fo_grupo) %>%
summarize(quantidade = n()) %>% group_by(genotipo) %>% mutate(soma = sum(quantidade), maximo = max(quantidade)) %>%
filter(quantidade == maximo) %>% mutate(porcentagem = maximo / soma) %>% dplyr::select(genotipo, fo_grupo, porcentagem) %>%
distinct(genotipo, .keep_all = TRUE)
### Criação do gráfico da Moda para FO
x11()
ggplot(wdata_fo, aes(reorder(genotipo, porcentagem), porcentagem, color = fo_grupo, ymin = 0, ymax = porcentagem)) + geom_point() + 
geom_pointrange() + theme_bw() + labs(x = "Genótipos", y = "Porcentagem de repetição", title = "Modas de grupos de doenças FO para cada genótipo") + 
coord_flip() + geom_text(aes(label = round(porcentagem, 2)), hjust = -0.5, size = 2.5)
wdata %>% select(fo_grupo) %>% unique()

### Criação da tabela para avaliar a mudança das doenças
tabela_ano_fo <- wdata[,3:50] %>% mutate(fo_grupo = as.character(fo_grupo), data_semeadura = lubridate::as_datetime(data_semeadura), ano_colheita = year(data_semeadura)) %>%
group_by(ano_colheita, genotipo, fo_grupo) %>% summarize(quantidade = n()) %>% group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade), soma = sum(quantidade), porcentagem = quantidade / soma) %>%
filter(quantidade == maximo) %>% distinct(ano_colheita, quantidade, .keep_all = TRUE) %>% arrange(genotipo)

### Criação dos índices
for (indice in 1:nrow(tabela_ano_fo)){
  condicao1 <- indice != 1
  if (condicao1){
    condicao2 <- tabela_ano_fo[indice, "ano_colheita"] != tabela_ano_fo[indice - 1, "ano_colheita"] 
    condicao3 <- tabela_ano_fo[indice, "genotipo"] == tabela_ano_fo[indice - 1, "genotipo"]
    condicao4 <- tabela_ano_fo[indice, "fo_grupo"] != tabela_ano_fo[indice - 1, "fo_grupo"]
    if(condicao3){
      if (condicao2 & condicao4){
        tabela_ano_fo[indice, "mudanca_predito"] <- 1
        tabela_ano_fo[indice, "nao_mudanca_predito"] <- 0
      }else{
        tabela_ano_fo[indice, "mudanca_predito"] <- 0
        tabela_ano_fo[indice, "nao_mudanca_predito"] <- 1
      }
    }else{
      tabela_ano_fo[indice, "mudanca_predito"] <- 0
      tabela_ano_fo[indice, "nao_mudanca_predito"] <- 0
    }
  }else{
    tabela_ano_fo[indice, "mudanca_predito"] <- 0
    tabela_ano_fo[indice, "nao_mudanca_predito"] <- 0
  }
}

### Genótipos que aparecem mais que 3 vezes no FO
teste <- tabela_ano_fo  %>% group_by(genotipo) %>% summarize(quantidade = n()) %>% filter(quantidade >= 3)
### Calculo exato dos índices
tabela_fo_mudancas <- tabela_ano_fo %>% filter(genotipo %in% teste$genotipo) %>% group_by(genotipo) %>% summarize(mudanca_total = sum(mudanca_predito) / (n() - 1), permanencia_total = sum(nao_mudanca_predito) / (n() - 1)) %>%
mutate(mudanca_total = case_when(is.na(mudanca_total) ~ 0,
                                 !is.na(mudanca_total) ~ mudanca_total),
                                 permanencia_total =  case_when(is.na(permanencia_total) ~ 0,
                                                                !is.na(permanencia_total) ~ permanencia_total))
### Criação do gráfico de mudança para FO
x11()
ggplot(tabela_fo_mudancas, aes(reorder(genotipo, mudanca_total), mudanca_total, ymin = 0, ymax = mudanca_total)) + geom_point() + geom_pointrange() + 
theme_bw() + labs(x = "Genótipos", y = "Quantidade de Saltos Por Genótipo", title = "Saltos de Cada genótipo de ano para ano, para doença FO") + coord_flip() +
geom_text(aes(label = round(mudanca_total, 2)), hjust = -0.5, size = 2.5)


### Criação do gráfico de não mudança Para FO
ggplot(tabela_fo_mudancas, aes(reorder(genotipo, permanencia_total), permanencia_total, ymin = 0, ymax = permanencia_total)) + geom_point() + geom_pointrange() + 
  theme_bw() + labs(x = "Genótipos", y = "Quantidade de Não Saltos Por Genótipo", title = "Não saltos de Cada genótipo de ano para ano, para doença FO") + coord_flip() +
geom_text(aes(label = round(permanencia_total, 2)), hjust = -0.5, size = 2.5)
### FS


### Banco que mostra a porcentagem de aparição do FS
wdata_fs <- wdata[,3:50] %>% mutate(fs_grupo = as.character(fs_grupo)) %>% group_by(genotipo, fs_grupo) %>%
  summarize(quantidade = n()) %>% group_by(genotipo) %>% mutate(soma = sum(quantidade), maximo = max(quantidade)) %>%
  filter(quantidade == maximo) %>% mutate(porcentagem = maximo / soma) %>% dplyr::select(genotipo, fs_grupo, porcentagem) %>%
  distinct(genotipo, .keep_all = TRUE)

#### Moda do FS

x11()
ggplot(wdata_fs, aes(reorder(genotipo, porcentagem), porcentagem, color = fs_grupo, ymin = 0, ymax = porcentagem)) + geom_point() + 
  geom_pointrange() + theme_bw() + labs(x = "Genótipos", y = "Porcentagem de repetição", title = "Modas de grupos de doenças FS para cada genótipo") + 
  coord_flip() + geom_text(aes(label = round(porcentagem, 2)), hjust = -0.5, size = 2.5)



tabela_ano_fs <- wdata[,3:50] %>% mutate(fs_grupo = as.character(fs_grupo), data_semeadura = lubridate::as_datetime(data_semeadura), ano_colheita = year(data_semeadura)) %>%
  group_by(ano_colheita, genotipo, fs_grupo) %>% summarize(quantidade = n()) %>% group_by(ano_colheita, genotipo) %>% mutate(maximo = max(quantidade), soma = sum(quantidade), porcentagem = quantidade / soma) %>%
  filter(quantidade == maximo) %>% distinct(ano_colheita, quantidade, .keep_all = TRUE) %>% arrange(genotipo)


for (indice in 1:nrow(tabela_ano_fs)){
  condicao1 <- indice != 1
  if (condicao1){
    condicao2 <- tabela_ano_fs[indice, "ano_colheita"] != tabela_ano_fs[indice - 1, "ano_colheita"] 
    condicao3 <- tabela_ano_fs[indice, "genotipo"] == tabela_ano_fs[indice - 1, "genotipo"]
    condicao4 <- tabela_ano_fs[indice, "fs_grupo"] != tabela_ano_fs[indice - 1, "fs_grupo"]
    if(condicao3){
      if (condicao2 & condicao4){
        tabela_ano_fs[indice, "mudanca_predito"] <- 1
        tabela_ano_fs[indice, "nao_mudanca_predito"] <- 0
      }else{
        tabela_ano_fs[indice, "mudanca_predito"] <- 0
        tabela_ano_fs[indice, "nao_mudanca_predito"] <- 1
      }
    }else{
      tabela_ano_fs[indice, "mudanca_predito"] <- 0
      tabela_ano_fs[indice, "nao_mudanca_predito"] <- 0
    }
  }else{
    tabela_ano_fs[indice, "mudanca_predito"] <- 0
    tabela_ano_fs[indice, "nao_mudanca_predito"] <- 0
  }
}

tabela_fs_mudancas <- tabela_ano_fs %>% group_by(genotipo) %>% summarize(mudanca_total = sum(mudanca_predito) / (n() - 1), permanencia_total = sum(nao_mudanca_predito) / (n() - 1)) %>%
  mutate(mudanca_total = case_when(is.na(mudanca_total) ~ 0,
                                   !is.na(mudanca_total) ~ mudanca_total),
         permanencia_total =  case_when(is.na(permanencia_total) ~ 0,
                                        !is.na(permanencia_total) ~ permanencia_total))
### Saltos do FS
x11()
ggplot(tabela_fs_mudancas, aes(reorder(genotipo, mudanca_total), mudanca_total, ymin = 0, ymax = mudanca_total)) + geom_point() + geom_pointrange() + 
  theme_bw() + labs(x = "Genótipos", y = "Quantidade de Saltos Por Genótipo", title = "Saltos de Cada genótipo de ano para ano, para doença FS") + coord_flip() +
  geom_text(aes(label = round(mudanca_total, 2)), hjust = -0.5, size = 2.5)


### Não saltos FS
ggplot(tabela_fs_mudancas, aes(reorder(genotipo, permanencia_total), permanencia_total, ymin = 0, ymax = permanencia_total)) + geom_point() + geom_pointrange() + 
  theme_bw() + labs(x = "Genótipos", y = "Quantidade de Não Saltos Por Genótipo", title = "Não saltos de Cada genótipo de ano para ano, para doença FS") + coord_flip() +
  geom_text(aes(label = round(permanencia_total, 2)), hjust = -0.5, size = 2.5)
