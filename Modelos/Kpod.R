set.seed(123)
library(tidyverse)
library(kpodclustr)
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/EMBRAPA2/input/banco_ajustado.csv") %>%
  rename("index" = ...2) %>% select(-...1) %>% arrange(index)

fo_matriz <- data.matrix(wdata %>% select(FO1:FO9))
fs_matriz <- data.matrix(wdata %>% select(FS1:FS11))

### Para FO

modelo_fo_k_3 <- kpod(fo_matriz, 3)
modelo_fo_k_4 <- kpod(fo_matriz, 4)
modelo_fo_k_5 <- kpod(fo_matriz, 5)

pontos_resultados <- data.frame(k = seq(3, 5),
                                resultado = c(modelo_fo_k_3$fit, modelo_fo_k_4$fit, 
                                              modelo_fo_k_5$fit))

ggplot(pontos_resultados, aes(k, resultado)) + geom_point() + geom_line() + 
theme_bw() + labs(x = "Quantidade de Cluster", y = "Ajuste", title = "Gráfico de Ajuste")


wdata <- wdata %>% mutate(fo_grupo = modelo_fo_k_5$cluster)

banco <- data.frame(table(wdata$genotipo, wdata$fo_grupo)) %>% group_by(Var1) %>%
mutate(maximo = max(Freq)) %>% filter(Freq == maximo)

criar_grafico = function(banco, grupo){
  ggplot(banco %>% filter(Var2 == grupo), aes(reorder(Var1, Freq), Freq, fill = Var1)) + 
  geom_col() + theme_bw() + labs(x = "Genotipos", y = "Frequência", 
  title = paste("Genótipos que se repetiram mais no grupo ", grupo)) + 
  theme(legend.position = "none") + geom_text(aes(label = Freq)) + coord_flip()
}

criar_grafico(banco, "1")
criar_grafico(banco, "2")
criar_grafico(banco, "3")
criar_grafico(banco, "4")
criar_grafico(banco, "5")

table(wdata$fo_grupo)


#### FS

modelo_fs_k_3 <- kpod(fs_matriz, 3)
modelo_fs_k_4 <- kpod(fs_matriz, 4)
modelo_fs_k_5 <- kpod(fs_matriz, 5)

pontos_resultados <- data.frame(k = seq(3, 5),
                                resultado = c(modelo_fs_k_3$fit, modelo_fs_k_4$fit, 
                                              modelo_fs_k_5$fit))



ggplot(pontos_resultados, aes(k, resultado)) + geom_point() + geom_line() + 
  theme_bw() + labs(x = "Quantidade de Cluster", y = "Ajuste", title = "Gráfico de Ajuste")



wdata <- wdata %>% mutate(fs_grupo = modelo_fs_k_4$cluster)
banco1 <- data.frame(table(wdata$genotipo, wdata$fs_grupo)) %>% group_by(Var1) %>%
  mutate(maximo = max(Freq)) %>% filter(Freq == maximo)

criar_grafico(banco1, "1")
criar_grafico(banco1, "2")
criar_grafico(banco1, "3")
criar_grafico(banco1, "4")