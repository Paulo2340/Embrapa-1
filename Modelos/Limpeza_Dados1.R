library(tidyverse)
wdata <- read_csv("C:/Users/paogr/Documents/juntos_teste.csv") %>% 
select(-c(Media_Fo, Mediana_FO, Desvio_FO, Cv_FO, Media_Fs, Mediana_FS, Desvio_FS, Cv_FS)) %>%
mutate(FO1 = as.numeric(FO1), FO2 = as.numeric(FO2), FO6 = as.numeric(FO6), FO9 = as.numeric(FO9),
       FS1 = as.numeric(FS1), FS2 = as.numeric(FS2), FS5 = as.numeric(FS5), FS7 = as.numeric(FS7),
       FS8 = as.numeric(FS8), FS10 = as.numeric(FS10))


### Tratando os FOS
for (c in 1:10){
  fo_nulos_zero <- wdata %>% filter(is.na(FO1)|FO1 == 0) %>% 
  mutate(FO1 = FO2, FO2 = FO3, FO3 = FO4, FO4 = FO5, FO5 = FO6, FO6 = FO7, FO7 = FO8,
         FO8 = FO9, FO9 = NA)
  fo_nao_nulo_zero <- wdata %>% filter(!(is.na(FO1)|FO1 == 0))
  wdata <- rbind(fo_nulos_zero, fo_nao_nulo_zero)
}




for (c in 1:11){
  fs_nulos_zero <- wdata %>% filter(is.na(FS1)|FS1 == 0) %>% 
    mutate(FS1 = FS2, FS2 = FS3, FS3 = FS4, FS4 = FS5, FS5 = FS6, FS6 = FS7, FS7 = FS8,
           FS8 = FS9, FS9 = FS10, FS10 = FS11, FS11 = NA)
  fs_nao_nulo_zero <- wdata %>% filter(!(is.na(FS1)|FS1 == 0))
  wdata <- rbind(fs_nulos_zero, fs_nao_nulo_zero)
}


wdata <- wdata %>% filter(!(...1 %in% c(614, 309, 626, 625, 673, 618, 617)))
write.csv(wdata, "C:/Users/paogr/Desktop/EMBRAPA2/input/banco_ajustado.csv")