### Criação do seed e importação dos bancos
set.seed(123)
library(tidyverse)
library(fda.usc)
banco <- read.csv("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/wdata_atual.csv")


#### Criação do modelo e atribuição dos grupos
grupos <- fdata(as.matrix(banco[,43:48], 1417, 6))
kmeans1 <- kmeans.fd(grupos, 3)
banco$predito <- kmeans1$cluster


### Calculando a moda geral
tabela <- banco %>% group_by(genotipo, as.character(predito)) %>% summarize(quantidade = n()) %>%
  rename(grupo = "as.character(predito)")
tabela1 <- tabela %>% group_by(genotipo) %>% mutate(porcentagem = quantidade / sum(quantidade),
                                                       salto = n() - 1, maximo = max(quantidade)) %>% filter(quantidade == maximo) %>%
mutate(grupo = as.character(grupo)) %>% distinct(genotipo, .keep_all = TRUE)

###Criação do gráfico da moda
ggplot(tabela1, aes(reorder(genotipo, porcentagem), porcentagem, color = grupo, ymin = 0, ymax = porcentagem)) + geom_point() + 
coord_flip() + theme_bw() + labs(y = "Porcentagem de Frequência", x = "Genótipos", title = "Quantidade de genótipos mais frequentes no grupo") +
geom_text(aes(label = round(porcentagem, 2)), hjust = -1, size = 3) + ylim(0, 1.5) + geom_pointrange()

### Criação do gráfico de saltos

ggplot(tabela1, aes(reorder(genotipo, salto), salto, color = grupo, ymin = 0, ymax = salto)) +
geom_point() + coord_flip() + geom_pointrange() + theme_bw() + geom_text(aes(label = salto), hjust = -1, size = 3) + 
labs(x = "Saltos", y = "Genótipos", title = "Saltos por cada Genótipo")

### Criação da tabela levando em consideração a cidade

tabela_cidade <- banco %>% mutate(predito = as.character(predito)) %>% 
group_by(genotipo, predito, cidade) %>% summarize(quantidade = n()) %>%
group_by(genotipo, cidade) %>% mutate(maximo = max(quantidade), total = sum(quantidade), 
porcentagem = quantidade / total, salto = n() - 1) %>% filter(quantidade == maximo)

### Criação dos gráficos para Arapoti

tabela_cidade_arapoti <- tabela_cidade %>% filter(cidade == "Arapoti")
ggplot(tabela_cidade_arapoti, aes(reorder(genotipo, porcentagem), porcentagem, color = predito, ymin = 0, ymax = porcentagem)) + geom_point() + 
  coord_flip() + theme_bw() + labs(y = "Porcentagem de Frequência", x = "Genótipos", title = "Quantidade de genótipos mais frequentes no grupo, na cidade Arapoti") +
  geom_text(aes(label = round(porcentagem, 2)), hjust = -1, size = 3) + ylim(0, 1.5) + geom_pointrange()

ggplot(tabela_cidade_arapoti, aes(reorder(genotipo, salto), salto, color = predito, ymin = 0, ymax = salto)) +
  geom_point() + coord_flip() + geom_pointrange() + theme_bw() + geom_text(aes(label = salto), hjust = -1, size = 3) + 
  labs(x = "Saltos", y = "Genótipos", title = "Saltos por cada Genótipo na cidade Arapoti")


### Criação dos gráficos para Castro

tabela_cidade_castro <- tabela_cidade %>% filter(cidade == "Castro")
ggplot(tabela_cidade_castro, aes(reorder(genotipo, porcentagem), porcentagem, color = predito, ymin = 0, ymax = porcentagem)) + geom_point() + 
  coord_flip() + theme_bw() + labs(y = "Porcentagem de Frequência", x = "Genótipos", title = "Quantidade de genótipos mais frequentes no grupo, na cidade Castro") +
  geom_text(aes(label = round(porcentagem, 2)), hjust = -1, size = 3) + ylim(0, 1.5) + geom_pointrange()

ggplot(tabela_cidade_castro, aes(reorder(genotipo, salto), salto, color = predito, ymin = 0, ymax = salto)) +
  geom_point() + coord_flip() + geom_pointrange() + theme_bw() + geom_text(aes(label = salto), hjust = -1, size = 3) + 
  labs(x = "Saltos", y = "Genótipos", title = "Saltos por cada Genótipo na cidade Castro")


write.csv(banco, "banco_com_efeitos.csv")

