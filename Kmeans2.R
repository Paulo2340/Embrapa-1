# Importações
source("C:/Users/paogr/Desktop/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
library(cluster)
library(stringr)
library(rpart)
library(fdm2id)
library(cluster)
set.seed(123)
banco <- read_csv("C:/Users/paogr/Documents/banco_agregado.csv") %>% select(-("...1"))

wdata <- banco %>% select(Local:safra, Maximo_Fs, Maximo_Fo) %>% 
group_by(Local, cidade, estado, fungicida, irrigacao, genotipo, tipo_de_grao, data_semeadura,
         id_ensaio, safra) %>% summarize(Media_Maximo_Fo = mean(Maximo_Fo, na.rm = TRUE),
                                                    Mediana_Maximo_Fo = median(Maximo_Fo, na.rm = TRUE),
                                                    Desvio_Maximo_Fo = sd(Maximo_Fo, na.rm = TRUE),
                                                    Cv_Maximo_Fo = Desvio_Maximo_Fo / Media_Maximo_Fo * 100,
                                                    Media_Maximo_Fs = mean(Maximo_Fs, na.rm = TRUE),
                                                    Mediana_Maximo_Fs = median(Maximo_Fs, na.rm = TRUE),
                                                    Desvio_Maximo_Fs = sd(Maximo_Fs, na.rm = TRUE),
                                                    Cv_Maximo_Fs = Desvio_Maximo_Fs / Mediana_Maximo_Fs * 100,
                                                    nan_obs_fs = is.na(banco))%>% 
  mutate(Local = str_remove(Local, " "))


### Criando os grupos

modelo_fo <- KMEANS(wdata$Media_Maximo_Fo, k = 4)
modelo_fs <- KMEANS(wdata$Media_Maximo_Fs, k = 4)

divisao <- tibble(notas = seq(0, 10, 0.01), predito_fo = predict(modelo_fo, notas),
                  predito_fs = predict(modelo_fs, notas)) %>% pivot_longer(c(predito_fo, predito_fs), 
                  names_to = "Doenca", values_to = "Grupos")


ggplot(divisao, aes(as.character(Grupos), notas, fill = as.character(Grupos))) + geom_boxplot() + theme_bw() + 
  labs(x = "Grupos", y = "Notas", title = "Distribuição dos Grupos por Nota", fill = "Grupos") + 
  facet_grid(vars(Doenca))

ordem_fo <- c("4", "1", "3", "2")
ordem_fs <- c("3", "1", "2", "4")
nome <- c("A", "B", "C", "D")

### Adaptando o nome dos Grupos
wdata <- wdata %>% mutate(Grupo_Fo = as.factor(predict(modelo_fo, Media_Maximo_Fo)),
                          Grupo_Fs = as.factor(predict(modelo_fs, Media_Maximo_Fs))) %>% 
  mutate(Grupo_Fo = factor(Grupo_Fo, levels = ordem_fo, labels = nome),
         Grupo_Fs = factor(Grupo_Fs, levels = ordem_fs, labels = nome))


ggplot(wdata, aes(Grupo_Fo, Media_Maximo_Fo, fill = Grupo_Fo)) + geom_boxplot() + 
  theme_bw() + labs(x = "Grupos", y = "Notas", title = "Distribuição das Notas FO por Grupo",
                    fill = "Grupo Fo")

ggplot(wdata, aes(Grupo_Fs, Media_Maximo_Fs, fill = Grupo_Fs)) + geom_boxplot() + 
  theme_bw() + labs(x = "Grupos", y = "Notas", title = "Distribuição das Notas FS por Grupo",
                    fill = "Grupo Fs")


### Criação da Relação entre os grupos e o desvio

fit.model_fo <- lm(Desvio_Maximo_Fo ~ Grupo_Fo, data = wdata)
fit.model_fs <- lm(Desvio_Maximo_Fs ~ Grupo_Fs, data = wdata)

summary(fit.model_fo)
summary(fit.model_fs)