library(tidyverse)
library(lubridate)
banco <- read.csv("C:/Users/paogr/Desktop/EMBRAPA2/input/V29.csv", sep = ";") %>%
  select(-X)
onii <- read.csv("C:/Users/paogr/Downloads/oni.ascii.txt") %>% select(SEAS, YR,
                                                                      TOTAL, ANOM)


banco <- banco %>% mutate(data_semeadura = lubridate::date(data_semeadura),
                          data_colheita = lubridate::date(data_colheita),
                          semeadura = month(data_semeadura, label = TRUE),
                          ano_semeadura = year(data_semeadura),
                          colheita = month(data_colheita, label = TRUE),
                          ano_colheita = year(data_colheita),
                          semeadura = str_to_upper(str_sub(semeadura,1,1)),
                          colheita = str_to_upper(str_sub(colheita, 1,1)))
  


anomalia <- matrix(nrow = 1417, ncol = 6)
for (c in  1:1417){
  observacao <- banco[c, c(37, 38)]
  observacao1 <- banco[c, c(39, 40)]
  filtrado <- onii %>% 
  filter(YR == observacao$ano_semeadura, str_detect(SEAS, observacao$semeadura))
  filtrado1 <- onii %>% 
  filter(YR == observacao1$ano_colheita, str_detect(SEAS, observacao1$colheita))
  anomalia[c, 1] <- filtrado$ANOM[1]
  anomalia[c, 2] <- filtrado$ANOM[2]
  anomalia[c, 3] <- filtrado$ANOM[3]
  anomalia[c, 4] <- filtrado1$ANOM[1]
  anomalia[c, 5] <- filtrado1$ANOM[2]
  anomalia[c, 6] <- filtrado1$ANOM[3]
}


banco$anom <- anomalia


write.csv(banco, "C:/Users/paogr/Desktop/EMBRAPA2/input/todo_agrupado.csv")