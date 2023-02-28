library(dplyr)
library(ggplot2)
library(naniar)
library(tidyverse)
library(naniar)


wdata <- read_delim("C:/Users/paogr/Desktop/EMBRAPA2/input/doenca_v1.csv", delim = ";")
referencia <- read_csv("C:/Users/paogr/Desktop/EMBRAPA2/input/lista_genotipo.csv")
vis_miss(wdata)
wdata <- wdata %>% filter(!(genotipo %in% c("PR10-4-4/24", "TAACariocaL-013", "IAC97-2", "Marhe"))) %>% mutate(FO = as.numeric(str_replace_all(FO, ",", ".")),
                                                                                                               FS = as.numeric(str_replace_all(FS, ",", ".")))



wdata$fungicida %>% unique()
wdata <- wdata %>% mutate(Classe = case_when(str_starts(genotipo, "BRS") ~ "BRS",
                                             str_starts(genotipo, "BolaCheia") ~ "BolaCheia",
                                             str_starts(genotipo, "FTS") ~ "FTS",
                                             str_starts(genotipo,"GEN05TGE") ~ "GEN05TGE",
                                             str_starts(genotipo, "IAC") ~ "IAC",
                                             str_starts(genotipo, "IPR") ~ "IPR",
                                             str_starts(genotipo, "LP06") ~ "LP06",
                                             str_starts(genotipo,"OTG") ~ "OTG",
                                             str_starts(genotipo, "P5") ~ "P5",
                                             str_starts(genotipo, "Perola") ~ "Perola",
                                             str_starts(genotipo, "PR") ~ "PR",
                                             str_starts(genotipo, "Rubi") ~ "Rubi",
                                             str_starts(genotipo, "Selecao") ~ "Selecao",
                                             str_starts(genotipo, "TAAL") ~ "TAAL",
                                             str_starts(genotipo, "SCS") ~ "SCS",
                                             str_starts(genotipo, "GEN") ~ "GEN",
                                             str_starts(genotipo, "TAA") ~ "TAA",
                                             str_starts(genotipo, "ANF") ~ "ANF",
                                             str_starts(genotipo, "Imperador") ~ "Imperador",
                                             str_starts(genotipo, "UEM") ~ "UEM",
                                             str_starts(genotipo, "Linhagem") ~ "Linhagem",
                                             str_starts(genotipo, "C10") ~ "C10",
                                             str_starts(genotipo, "45") ~ "45",
                                             str_starts(genotipo, "49") ~ "49",
                                             str_starts(genotipo, "FAP") ~ "FAP",
                                             str_starts(genotipo, "ANfe") ~ "ANfe",
                                             str_starts(genotipo, "C-") ~ "C-",
                                             str_starts(genotipo, "C2") ~ "C2",
                                             str_starts(genotipo, "LPO") ~ "LPO",
                                             str_starts(genotipo, "Multiplicacao") ~ "Multiplicacao"))


### FS

ggplot(wdata, aes(genotipo, FS)) + geom_boxplot(aes(fill = genotipo)) + 
geom_jitter(aes(color = genotipo)) + coord_flip() + theme_bw() + theme(legend.position = "none") + 
labs(x = "Genótipo", y = "Notas do FS", title = "Relação de Genótipos com as Notas do FS") + 
  facet_wrap(vars(cidade))

ggplot(wdata, aes(genotipo, FS)) + geom_boxplot(aes(fill = genotipo)) + 
  geom_jitter(aes(color = genotipo)) + coord_flip() + theme_bw() + theme(legend.position = "none") + 
  labs(x = "Genótipo", y = "Notas do FS", title = "Relação de Genótipos com as Notas do FS") + 
  facet_wrap(vars(cidade, safra))


### FO/FS ~ genotipo + ea(cidade) + ea(safra) + ea(cidade|safra)
### FO/FS ~ genotipo + ea(cidade) + ea(safra) + ea(cidade|safra) + ea(genotipo|safra) + ea(cidade|safra|genotipo)



### FO



ggplot(wdata, aes(genotipo, FO)) + geom_boxplot(aes(fill = genotipo)) + 
  geom_jitter(aes(color = genotipo)) + coord_flip() + theme_bw() + theme(legend.position = "none") + 
  labs(x = "Genótipo", y = "Notas do FO", title = "Relação de Genótipos com as Notas do FO") + facet_wrap(vars(cidade))



ggplot(wdata, aes(genotipo, FO)) + geom_boxplot(aes(fill = genotipo)) + 
  geom_jitter(aes(color = genotipo)) + coord_flip() + theme_bw() + theme(legend.position = "none") + 
  labs(x = "Genótipo", y = "Notas do FO", title = "Relação de Genótipos com as Notas do FO") + facet_wrap(vars(cidade, safra))
