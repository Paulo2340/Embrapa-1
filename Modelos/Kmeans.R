set.seed(12432)
library(tidyverse)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
wdata <- read.csv("C:/Users/paogr/Desktop/EMBRAPA2/input/doenca_v1.csv", sep = ";")
library(cluster)
library(stringr)
library(rpart)
library(fdm2id)
library(cluster)
### Importação das Bibliotecas


wdata <- wdata %>% filter(!is.na(FO), !is.na(FS), FO <= 10, FS <= 10) %>% mutate(FO = str_replace(FO, ",", "."),
                                                                                 FS = str_replace(FS, ",", ".")) %>% mutate(FO = as.numeric(FO), FS = as.numeric(FS)) %>% 
  mutate(genotipo = as.factor(genotipo))

### Conserto do banco de dados
agrupados <- wdata %>% group_by(genotipo, cidade, tipo_de_grao, safra) %>% summarize(Media_fs = mean(FS),
                                                                                     Media_fo = mean(FO),
                                                                                     quantidade = n(), 
                                                                                     cv_fs = (sd(FS, na.rm= TRUE) / Media_fs) * 100,
                                                                                     cv_fo = (sd(FO, na.rm = TRUE)/Media_fs) * 100)

                                                                                     
ggplot(agrupados, aes(cv_fs)) + geom_boxplot()                                                             
fs_cotovelo <- c()
fo_cotovelo <- c()
for (indice in 2:5){
  fs <- kmeans(agrupados$Media_fs, centers = indice)
  fo <- kmeans(agrupados$Media_fo, centers = indice)
  fs_cotovelo[indice - 1] <- fs$tot.withinss
  fo_cotovelo[indice - 1] <- fo$tot.withinss
}

resultado_cotovelo <- tibble(indice = 2:5,
                             cotovelo_fs = fs_cotovelo,
                             cotovelo_fo = fo_cotovelo) %>% pivot_longer(-indice, names_to = "Doenca", values_to = "Cotovelo")

ggplot(resultado_cotovelo, aes(indice, Cotovelo, color = Doenca)) + geom_point() + geom_line() + theme_bw() + 
labs(x = "Grupos K", y = "Cotovelo", title = "Gráfico de Cotovelo", colors = "Doenças") + facet_grid(vars(Doenca), space = "free", scale = "free")

### Criação dos Modelos


fo_modelo <- KMEANS(agrupados$Media_fo, k = 3)
fs_modelo <- KMEANS(agrupados$Media_fs, k = 3)


mudanca <- tibble(notas = seq(0, 10, 0.001), previsao_fs = as.factor(predict(fs_modelo, notas)), 
                  previsao_fo = as.factor(predict(fo_modelo, notas)))

fs_mudanca <- mudanca %>% group_by(previsao_fs) %>% summarize(maximo = max(notas))
fo_mudanca <- mudanca %>% group_by(previsao_fo) %>% summarize(maximo = max(notas))

ggplot(fs_mudanca, aes(reorder(previsao_fs, maximo), maximo, color = previsao_fs)) + geom_point() + theme_bw() + labs(x = "Grupos", y = "Notas", 
                                                                                                    title = "Ponto de Mudança FS", color = "Grupos") + 
  geom_text(aes(label = maximo), vjust = -0.5)

ggplot(fo_mudanca, aes(reorder(previsao_fo, maximo), maximo, color = previsao_fo)) + geom_point() + theme_bw() + labs(x = "Grupos", y = "Notas", 
                                                                                                                      title = "Ponto de Mudança FO", color = "Grupos") + 
  geom_text(aes(label = maximo), vjust = -0.5)


ordem_fs <- c("3", "2", "1")
ordem_fo <- c("1", "2", "3")
ordem <- c("Resistente", "Neutro", "Sensível")

wdata <- wdata %>% mutate(previsao_fs = factor(predict(fs_modelo, FS), levels = ordem_fs, label = ordem) , 
                          previsao_fo = factor(predict(fo_modelo, FO), levels = ordem_fo, label = ordem))


ggplot(wdata, aes(previsao_fs, FS, color = previsao_fs)) + geom_jitter() + theme_bw() + 
  labs(x = "Grupos", y = "Notas FS", title = "Distribuição do FS", color = "Grupos")

ggplot(wdata, aes(previsao_fo, FO, color = previsao_fo)) + geom_jitter() + theme_bw() + 
  labs(x = "Grupos", y = "Notas FO", title = "Distribuição do FO", color = "Grupos")


### Associação dos genótipos com os grupos

# Safras :  "15/16" "16/17" "17/18" "18/19" "19/20" "20/21"

gerador_graficos = function(banco, filtro){
informacao <- banco %>% filter(safra == filtro) %>% group_by(genotipo) %>%
summarize(Media_Fs = mean(Media_fs), Media_Fo = mean(Media_fo), predito_fs = 
            predict(fs_modelo, Media_Fs), predito_fo = predict(fo_modelo, Media_Fo)) %>% mutate(predito_fs = factor(predito_fs, levels = ordem_fs, label = ordem),
                                                                                                predito_fo = factor(predito_fo, levels = ordem_fo, label = ordem)) 

grafico1 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fo), Media_Fo, fill = predito_fo)) + geom_col() +
theme_bw() + 
labs(x = "Genótipos", y = "Nota Média", title = paste("Associação entre genótipos e grupos na Safra", filtro), color = "Grupos") +
coord_flip() + scale_fill_brewer(palette = "Dark2")

grafico2 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fs), Media_Fs, fill = predito_fs)) + geom_col() +
  theme_bw() + 
  labs(x = "Genótipos", y = "Nota Média", title = paste("Associação entre genótipos e grupos na Safra", filtro), color = "Grupos") +
  coord_flip() + scale_fill_brewer(palette = "Dark2")
library(cowplot)
print(plot_grid(grafico1, grafico2))}


### Safra 15/16
gerador_graficos(agrupados, "15/16")

### Safra 16/17

gerador_graficos(agrupados, "16/17")

### Safra 17/18

gerador_graficos(agrupados, "17/18")

### Safra 18/19

gerador_graficos(agrupados, "18/19")

### Safra 19/20

gerador_graficos(agrupados, "19/20")

### Safra 20/21

gerador_graficos(agrupados, "20/21")

###---------------------------------------------------------------------------Cidades

gerador_graficos_cidade = function(banco, filtro){
  informacao <- banco %>% filter(cidade == filtro) %>% group_by(genotipo) %>%
    summarize(Media_Fs = mean(Media_fs), Media_Fo = mean(Media_fo), predito_fs = 
                predict(fs_modelo, Media_Fs), predito_fo = predict(fo_modelo, Media_Fo)) %>% mutate(predito_fs = factor(predito_fs, levels = ordem_fs, label = ordem),
                                                                                                    predito_fo = factor(predito_fo, levels = ordem_fo, label = ordem))
  
  grafico1 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fo), Media_Fo, fill = predito_fo)) + geom_col() +
    theme_bw() + 
    labs(x = "Genótipos", y = "Nota Média", title = paste("Associação entre genótipos e grupos na Cidade", filtro), color = "Grupos") +
    coord_flip() + scale_fill_brewer(palette = "Dark2")
  
  grafico2 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fs), Media_Fs, fill = predito_fs)) + geom_col() +
    theme_bw() + 
    labs(x = "Genótipos", y = "Nota Média", title = paste("Associação entre genótipos e grupos na Cidade", filtro), color = "Grupos") +
    coord_flip() + scale_fill_brewer(palette = "Dark2")
  library(cowplot)
  print(plot_grid(grafico1, grafico2))
}
## Arapoti
gerador_graficos_cidade(agrupados, "Arapoti")

### Castro
gerador_graficos_cidade(agrupados, "Castro")


#### Consertar

grafico1 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fo), Media_Fo, fill = predito_fo)) + 
  geom_col() +

grafico2 <- ggplot(informacao, aes(reorder(genotipo, -Media_Fs), Media_Fs, fill = predito_fs)) + geom_col() +
  theme_bw() + 
  labs(x = "", y = "Valor Médio", title = paste("Safra", filtro), color = "Grupos",
       fill= "Fusarium solani") +
  coord_flip() + scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "top", 
        legend.title = element_text(colour="red", size=10,  face="bold"),
        axis.text =  element_text(face="bold" ))