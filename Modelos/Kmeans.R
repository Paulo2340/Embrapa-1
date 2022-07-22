set.seed(12432)
library(tidyverse)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
wdata <- read.csv("C:/Users/paogr/Desktop/EMBRAPA2/input/doenca_v1.csv", sep = ";")
library(cluster)
library(stringr)


library(fdm2id)
predict.kmeans {fdm2id}

wdata <- wdata %>% filter(!is.na(FO), !is.na(FS), FO <= 10, FS <= 10) %>% mutate(FO = str_replace(FO, ",", "."),
                                                             FS = str_replace(FS, ",", ".")) %>% mutate(FO = as.numeric(FO), FS = as.numeric(FS))

resultado <- c()
resultado1 <- c()
for (k in 2:5){
  resultado[k - 1] <- pam(wdata$FO, k)$silinfo$avg.width
  
  resultado1[k - 1] <- pam(wdata$FS, k)$silinfo$avg.width
}

tabela <- data.frame(k = 2:5,
                     FS = resultado,
                     FO = resultado1) %>% pivot_longer(c(FO, FS), names_to = "VariavelResposta", values_to = "Resultado")


ggplot(tabela, aes(k, Resultado, color = VariavelResposta)) + geom_line() + geom_point() + geom_text(aes(label = round(Resultado, 2)), vjust = -0.5) + theme_bw()


modelo_FO <- KMEANS(wdata$FO, k = 5)
modelo_FS <- KMEANS(wdata$FS, k = 5)


wdata <- wdata %>% mutate(grupo_fo = modelo_FO$cluster, grupo_fs = modelo_FS$cluster)
ggplot(wdata, aes(FO, grupo_fo, color = as.factor(grupo_fo))) + geom_jitter() + theme_bw() + labs(title = "Clusterização do FO", x = "Notas FO", y = "Grupos", cols = "Grupos") + theme(legend.position = "none")
ggplot(wdata, aes(FS, grupo_fs, color = as.factor(grupo_fs))) + geom_jitter() + theme_bw() + labs(title = "Clusterização do FS", x = "Notas FS", y = "Grupos", cols = "Grupos") + theme(legend.position = "none")

#####################################################################################################################################

predict(modelo_FO, seq(0.2, 0.3, 0.0005))

divisao <- tibble(nota_fo = seq(0, 10, 0.001), nota_fs = seq(0, 10, 0.001),
                  previsao_fo = predict(modelo_FO, nota_fo), previsao_fs = predict(modelo_FS, nota_fs))

grafico_fo <- divisao %>% group_by(as.factor(previsao_fo)) %>% summarize(valor_maximo_fo = max(nota_fo)) %>% rename(agrupamento = 'as.factor(previsao_fo)')
grafico_fs <- divisao %>% group_by(as.factor(previsao_fs)) %>% summarize(valor_maximo_fs = max(nota_fs)) %>% rename(agrupamento = 'as.factor(previsao_fs)') 

ggplot(grafico_fo, aes(reorder(agrupamento, valor_maximo_fo), valor_maximo_fo, color = agrupamento)) + geom_point() + theme_bw() + theme(legend.position = "none") + labs(x = "Grupos", y = "Notas do FO", title = "Extremos de cada ponto") + 
  geom_text(aes(label = valor_maximo_fo), vjust = -0.5)

ggplot(grafico_fs, aes(reorder(agrupamento, valor_maximo_fs), valor_maximo_fs, color = agrupamento)) + geom_point() + theme_bw() + theme(legend.position = "none") + labs(x = "Grupos", y = "Notas do FS", title = "Extremos de cada ponto") + 
  geom_text(aes(label = valor_maximo_fs), vjust = -0.5)
