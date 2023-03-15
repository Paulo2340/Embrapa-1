set.seed(123)
library(tidyverse)
library(lubridate)
library(kpodclustr)
library(tidytext) 
library(tidyverse)
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/CO/Seca/lm/bibliotecas.R")
source("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA/Modelos_por_safra/dados_e_etc/general_functions.R")
wdata <- read_csv("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/banco_com_efeitos.csv") %>% mutate(fo_grupo = as.character(fo_grupo),
                                                                                            fs_grupo = as.character(fs_grupo)) %>% mutate(predito = case_when(predito == 1 ~ "Neutro",
                                                                                                                                                              predito == 2 ~ "El Nino",
                                                                                                                                                              predito == 3 ~ "La Nina"))

### Gráfico de curvas para doença FO
fo <- wdata %>% select(FO1:FO9, fo_grupo) %>% pivot_longer(-fo_grupo, names_to = "Data", values_to = "Notas") %>%
group_by(fo_grupo, Data) %>% summarize(mediana = median(Notas, na.rm = TRUE)) %>% mutate(Data = 
  case_when(Data == "FO1" ~ 1,
            Data == "FO2" ~ 2,
            Data == "FO3" ~ 3,
            Data == "FO4" ~ 4,
            Data == "FO5" ~ 5,
            Data == "FO6" ~ 6,
            Data == "FO7" ~ 7,
            Data == "FO8" ~ 8,
            Data == "FO9" ~ 9))

ggplot(fo, aes(Data, mediana, color = fo_grupo)) + geom_point() + geom_line() + theme_bw() + labs(x = "Grupo", y = "Notas do FO", title = "Grafico de pontos da mediana de Cada grupo FO") + geom_text(aes(label = mediana), vjust = -0.5)


### Gráfico de Curvas para doença FS

fs <- wdata %>% select(FS1:FS11, fs_grupo) %>% pivot_longer(-fs_grupo, names_to = "Data", values_to = "Notas") %>%
  group_by(fs_grupo, Data) %>% summarize(mediana = median(Notas, na.rm = TRUE)) %>% mutate(Data = 
                                                                                             case_when(Data == "FS1" ~ 1,
                                                                                                       Data == "FS2" ~ 2,
                                                                                                       Data == "FS3" ~ 3,
                                                                                                       Data == "FS4" ~ 4,
                                                                                                       Data == "FS5" ~ 5,
                                                                                                       Data == "FS6" ~ 6,
                                                                                                       Data == "FS7" ~ 7,
                                                                                                       Data == "FS8" ~ 8,
                                                                                                       Data == "FS9" ~ 9,
                                                                                                       Data == "FS10" ~ 10,
                                                                                                       Data == "FS11" ~ 11,
                                                                                                       Data == "FS12" ~ 12))
ggplot(fs, aes(Data, mediana, color = fs_grupo)) + geom_point() + geom_line() + theme_bw() + labs(x = "Grupo", y = "Notas do Fs", title = "Grafico de pontos da mediana de Cada grupo FS") + geom_text(aes(label = mediana), vjust = -0.5)



### FO 3 - Muito Tolerante,1 - Tolerante, 5 - Mediamente Tolerante, 4 - Pouco tolerante, 2 - Muito Pouco Tolerante
### FS 1 - Muito Tolerante,4 - Tolerante, 3 - Pouco Tolerante, 2 - Muito Pouco Tolerante


wdata <- wdata %>% mutate(fo_grupo = case_when(fo_grupo == "3" ~ "Muito Tolerante",
                                               fo_grupo == "1" ~ "Tolerante",
                                               fo_grupo == "5" ~ "Mediamente Tolerante",
                                               fo_grupo == "4" ~ "Pouco Tolerante",
                                               fo_grupo == "2" ~ "Muito Pouco Tolerante"),
                          fs_grupo = case_when(fs_grupo == "1" ~ "Muito Tolerante",
                                               fs_grupo == "4" ~ "Tolerante",
                                               fs_grupo == "3" ~ "Pouco Tolerante",
                                               fs_grupo == "2" ~ "Muito Pouco Tolerante")) %>% select(-c("...1", "X", "...3"))


write.csv(wdata, "C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/banco_com_efeitos2.csv")
