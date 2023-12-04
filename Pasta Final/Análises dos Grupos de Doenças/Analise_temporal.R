### Criação do seed e importação dos bancos
set.seed(123)
library(tidyverse)
library(fda.usc)
banco <- read.csv("C:/Users/paogr/Desktop/Arquivos Importantes/EMBRAPA2/input/todo_agrupado.csv")

par(mar = c(3, 3, 1, 1))
#### Criação do modelo e atribuição dos grupos
grupos <- fdata(as.matrix(banco[,42:47], 1417, 6))
kmeans1 <- kmeans.fd(grupos, 3)
banco$predito <- kmeans1$cluster

#### Criação dos índices de salto e não salto
tabela <- banco %>% arrange(genotipo, ano_colheita)
for (c in 1:dim(tabela)[1]){
  if (c != 1){
    genotipo_nome_posterior <- tabela[c, "genotipo"]
    genotipo_nome_anterior <- tabela[c - 1, "genotipo"]
    ano_posterior <- tabela[c, "ano_colheita"]
    ano_anterior <- tabela[c - 1, "ano_colheita"]
    if ((genotipo_nome_posterior == genotipo_nome_anterior) & (ano_posterior != ano_anterior)){
      if (tabela[c, "predito"] != tabela[c - 1, "predito"]){
        tabela[c, "teve_salto_mudanca"] <- 1
        tabela[c, "teve_salto_continua"] <- 0
      }else{
        tabela[c, "teve_salto_mudanca"] <- 0
        tabela[c, "teve_salto_continua"] <- 1
      }
    }else{
      tabela[c, "teve_salto_mudanca"] <- 0
      tabela[c, "teve_salto_continua"] <- 0
    }
  }else{
    tabela[c, "teve_salto_mudanca"] <- 0
    tabela[c, "teve_salto_continua"] <- 0
  }
}
### Tabela mostrando a quantidade de saltos e não saltos
quantidade_saltos <- tabela %>% group_by(genotipo) %>% summarize(quant_saltos = sum(teve_salto_mudanca))
quantidade_saltos2 <- tabela %>% group_by(genotipo) %>% summarize(quant_saltos = sum(teve_salto_continua))
### Criação dos gráficos de saltos para outros grupos
x11()
ggplot(quantidade_saltos, aes(reorder(genotipo, quant_saltos), quant_saltos, ymin = 0, ymax = quant_saltos)) + geom_point() + 
geom_pointrange() + theme_bw() + coord_flip() + labs(title = "Quantidade de saltos de cada genotipo para outros grupos", 
                                                     x = "Genótipos", y = "Quantidade de Saltos") + geom_text(aes(label = quant_saltos))

### Criação do gráfico de não saltos
ggplot(quantidade_saltos2, aes(reorder(genotipo, quant_saltos), quant_saltos, ymin = 0, ymax = quant_saltos)) + geom_point() + 
  geom_pointrange() + theme_bw() + coord_flip() + labs(title = "Quantidade de saltos de cada genotipo na qual permaneceram no mesmo grupo", 
                                                       x = "Genótipos", y = "Quantidade de Saltos")
