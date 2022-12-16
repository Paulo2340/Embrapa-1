#Pacotes
require(lme4)
require(tidyverse)
require(data.table)
library(stringr)
library(gl)
library(stringi)
library(naniar)

# Lendo dados
safra_20_21 = read.csv2('safra_20_21.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_20_21)
#test$data_semeadura
safra_20_21$safra = "20/21"

safra_20_21$genotipo = str_replace_all(safra_20_21$genotipo," ","")
safra_20_21$local = str_replace_all(safra_20_21$local," ","")
safra_20_21$genotipo = stri_trans_general(str = safra_20_21$genotipo, id = "Latin-ASCII")
safra_20_21$tipo_de_grao = str_replace_all(safra_20_21$tipo_de_grao," ","")
safra_20_21$local = stri_trans_general(str = safra_20_21$local, id = "Latin-ASCII")

hist(safra_20_21$FO)
hist(safra_20_21$FS)

sort(unique(safra_20_21$genotipo))

table(safra_20_21$local)
table(safra_20_21$cidade)
#-----------------------------------------------------------------------
# Lendo dados
safra_19_20 = read.csv2('safra_19_20.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_19_20)
#test$data_semeadura
safra_19_20$safra = "19/20"

safra_19_20$genotipo = str_replace_all(safra_19_20$genotipo," ","")
safra_19_20$local = str_replace_all(safra_19_20$local," ","")
safra_19_20$genotipo = stri_trans_general(str = safra_19_20$genotipo, id = "Latin-ASCII")
safra_19_20$tipo_de_grao = str_replace_all(safra_19_20$tipo_de_grao," ","")
safra_19_20$local = stri_trans_general(str = safra_19_20$local, id = "Latin-ASCII")

hist(safra_19_20$FO)
hist(safra_19_20$FS)

sort(unique(safra_19_20$genotipo))

table(safra_19_20$local)
table(safra_19_20$cidade)
#-----------------------------------------------------------------------
# Lendo dados
safra_18_19 = read.csv2('safra_18_19.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_18_19)
#test$data_semeadura
safra_18_19$safra = "18/19"

safra_18_19$genotipo = str_replace_all(safra_18_19$genotipo," ","")
safra_18_19$local = str_replace_all(safra_18_19$local," ","")
safra_18_19$genotipo = stri_trans_general(str = safra_18_19$genotipo, id = "Latin-ASCII")
safra_18_19$tipo_de_grao = str_replace_all(safra_18_19$tipo_de_grao," ","")
safra_18_19$local = stri_trans_general(str = safra_18_19$local, id = "Latin-ASCII")

hist(safra_18_19$FO)
hist(safra_18_19$FS)

sort(unique(safra_18_19$genotipo))

table(safra_18_19$local)
table(safra_18_19$cidade)

#-----------------------------------------------------------------------
# Lendo dados
safra_17_18 = read.csv2('safra_17_18.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_17_18)
#test$data_semeadura
safra_17_18$safra = "17/18"

safra_17_18$genotipo = str_replace_all(safra_17_18$genotipo," ","")
safra_17_18$local = str_replace_all(safra_17_18$local," ","")
safra_17_18$genotipo = stri_trans_general(str = safra_17_18$genotipo, id = "Latin-ASCII")
safra_17_18$tipo_de_grao = str_replace_all(safra_17_18$tipo_de_grao," ","")
safra_17_18$local = stri_trans_general(str = safra_17_18$local, id = "Latin-ASCII")

hist(safra_17_18$FO)
hist(safra_17_18$FS)

sort(unique(safra_17_18$genotipo))

table(safra_17_18$local)
table(safra_17_18$cidade)
#-----------------------------------------------------------------------
# Lendo dados
safra_16_17 = read.csv2('safra_16_17.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_16_17)
#test$data_semeadura
safra_16_17$safra = "16/17"

safra_16_17$genotipo = str_replace_all(safra_16_17$genotipo," ","")
safra_16_17$local = str_replace_all(safra_16_17$local," ","")
safra_16_17$genotipo = stri_trans_general(str = safra_16_17$genotipo, id = "Latin-ASCII")
safra_16_17$tipo_de_grao = str_replace_all(safra_16_17$tipo_de_grao," ","")
safra_16_17$local = stri_trans_general(str = safra_16_17$local, id = "Latin-ASCII")

hist(safra_16_17$FO)
hist(safra_16_17$FS)

sort(unique(safra_16_17$genotipo))

table(safra_16_17$local)
table(safra_16_17$cidade)
#-----------------------------------------------------------------------
# Lendo dados
safra_15_16 = read.csv2('safra_15_16.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_15_16)
#test$data_semeadura
safra_15_16$safra = "15/16"

safra_15_16$genotipo = str_replace_all(safra_15_16$genotipo," ","")
safra_15_16$local = str_replace_all(safra_15_16$local," ","")
safra_15_16$genotipo = stri_trans_general(str = safra_15_16$genotipo, id = "Latin-ASCII")
safra_15_16$tipo_de_grao = str_replace_all(safra_15_16$tipo_de_grao," ","")
safra_15_16$local = stri_trans_general(str = safra_15_16$local, id = "Latin-ASCII")

hist(safra_15_16$FO)
hist(safra_15_16$FS)

sort(unique(safra_15_16$genotipo))

table(safra_15_16$local)
table(safra_15_16$cidade)
#-----------------------------------------------------------------------
# Lendo dados
safra_14_15 = read.csv2('safra_14_15.csv', dec=",")
#test = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v5.csv")
head(safra_14_15)
#test$data_semeadura
safra_14_15$safra = "15/16"

safra_14_15$genotipo = str_replace_all(safra_14_15$genotipo," ","")
safra_14_15$local = str_replace_all(safra_14_15$local," ","")
safra_14_15$genotipo = stri_trans_general(str = safra_14_15$genotipo, id = "Latin-ASCII")
safra_14_15$tipo_de_grao = str_replace_all(safra_14_15$tipo_de_grao," ","")
safra_14_15$local = stri_trans_general(str = safra_14_15$local, id = "Latin-ASCII")

hist(safra_14_15$FO)
hist(safra_14_15$FS)

sort(unique(safra_14_15$genotipo))

table(safra_14_15$local)
table(safra_14_15$cidade)

#---------------------------------------------------
test = rbind.data.frame(safra_14_15,safra_15_16, safra_16_17, safra_17_18, safra_18_19,safra_19_20, safra_20_21 )

sort(unique(test$genotipo))
sort(unique(test$tipo_de_grao))

test = test %>%
  mutate(genotipo = case_when(
    genotipo == "BRSMadreperola"   ~ "BRSMGMadreperola",
    TRUE ~ as.character(genotipo)))

mes = month(as.Date(test$data_semeadura, format = "%d/%m/%Y"))
table(test$data_semeadura)

#cria o ID---------------------------------------------------------
test$id_ensaio = paste(
  test$cidade,
  test$local,
  test$safra,
  mes,
  test$irrigacao,
  test$fungicida,
  sep = "_")

unique(test$id_ensaio)

write.csv2(test,"doenca_v1.csv", row.names = F)





#Correçoes-------------------------------------
#--------------------------------------
test = read.csv2('doenca_v1.csv', dec=",")

sort(unique(test$genotipo))

test1 = v6 %>%
  filter( is.na(produtividade))

test1 = test %>%
  filter(safra == "10/11")

table(test$safra)

# s_10_11 = tabela %>%
#   mutate(cidade = case_when(
#     Local == "hi"   ~ "Itai",
#     Local== "i"   ~ "Taquarituba",
#     Local == "isa"   ~ "Itabera",
#     Local== "a"   ~ "Arapoti",
#     Local == "t"   ~ "Tibagi",
#     Local== "b"   ~ "PontaGrossa",
#     Local == "c"   ~ "Castro"))

# s_09_10 = s_09_10 %>%
#   mutate(local = case_when(
#     Local == "hi"   ~ "FazNossaSenhoradoCarmo",
#     Local== "i"   ~ "FazNossaSenhoraAparecida",
#     Local == "isa"   ~ "CDEItabera",
#     Local== "a"   ~ "CDEArapoti",
#     Local == "t"   ~ "CDETibagi",
#     Local== "b"   ~ "CDEPontaGrossa",
#     Local == "c"   ~ "CDECastro"))

s_10_11 = tabela %>%
  mutate(estado = case_when(
    cidade == "Taquarituba"   ~ "PR",
    cidade== "Itapeva"      ~ "SP",
    cidade =="Itaberá"       ~ "SP",
    cidade== "Arapoti"     ~ "PR",
    cidade =="Tibagi"      ~ "PR",
    cidade== "PontaGrossa"~ "PR",
    cidade =="Castro"     ~ "PR"))

tabela1 = tabela%>%
  mutate(repeticao = case_when(
    repeticao == "I"   ~ 1,
    repeticao== "II"      ~ 2,
    repeticao =="III"       ~ 3,
    repeticao== "IV"     ~ 4))


table(tabela1$repeticao)
#s_10_11$umidade = as.numeric(s_10_11$umidade)
names(tabela1)

# Produtividade = (Gramaspar/(NlinhasPutil x ClinhaPutil x ElinhasPutil))*10/((100 - Umidade)/(100 -13))

tabela2 = tabela %>%
   mutate(produtividade = round((Gramaspar/(NlinhasPutil*ClinhasPutil*ElinhasPutil))*10/((100 - Umidade)/(100 -13)),2))
hist(tabela2$produtividade) 

tabela2$safra = "17/18"  

# s_10_11_1 = s_10_11_1 %>%
#   mutate(irrigacao = case_when(
#     Local == "hi"   ~ "sim",
#     Local== "i"   ~ "nao",
#     Local == "isa"   ~ "nao",
#     Local== "a"   ~ "nao",
#     Local == "t"   ~ "nao",
#     Local== "b"   ~ "nao",
#     Local == "c"   ~ "nao"))

names(s_10_11)

tabela2$genotipo = str_replace_all(tabela2$genotipo," ","")
tabela2$local = str_replace_all(tabela2$local," ","")
tabela2$genotipo = stri_trans_general(str = tabela2$genotipo, id = "Latin-ASCII")
tabela2$tipo_de_grao = str_replace_all(tabela2$tipo_de_grao," ","")
tabela2$local = stri_trans_general(str = tabela2$local, id = "Latin-ASCII")

table(tabela2$cidade)
#hist(s_10_11_1$produtividade)

sort(unique(tabela2$genotipo))
sort(unique(tabela2$tipo_de_grao))

tabela2$mes = month(as.Date(tabela2$data_semeadura, format = "%d/%m/%Y"))
table(tabela2$data_semeadura)

tabela2$id_ensaio = paste(
  tabela2$cidade,
  tabela2$local,
  tabela2$safra,
  tabela2$mes,
  tabela2$irrigacao,
  tabela2$fungicida,
  sep = "_")

names(tabela2)

# s_10_11_1 =s_10_11_1 %>% 
#   rename(local     =     Local)



tabela3 = tabela2 %>%
#  rename_with(tolower)%>%
select(id_ensaio,estado,cidade,safra,local,irrigacao,fungicida,tipo_de_grao,genotipo,                 
       repeticao, produtividade,data_semeadura, data_emergencia, data_inicio_floracao,
       data_inicio_ponto_colheita, data_inicio_colheita)

test = filter(tabela3, produtividade < 100)

tabela3 = tabela3[-c(195),]

v10 = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Input/v9.csv")
names(v10)
names(tabela3)

v10 = filter(v10, safra != "17/18")
table(v10$safra)

v10 = rbind(v10,tabela3)

test = sum(is.na(tabela3$produtividade))
gen = unique(sort(v6$genotipo))

table(v7$irrigacao)

table(v10$cidade)

v10 = v10 %>%
  mutate(cidade = case_when(
    cidade == "PontaGrossa"   ~ "Ponta Grossa",
    TRUE ~ as.character(cidade)))

table(v10$local)

v10 = v10 %>%
  mutate(local = case_when(
    local == "FazendaRibeirãoBonito"   ~ "FazendaRibeiraoBonito",
    TRUE ~ as.character(local)))

v10 = v10 %>%
  mutate(genotipo = case_when(
    genotipo == "Gol"   ~ "TAAGol",
    TRUE ~ as.character(genotipo)))


sort(unique(v10$genotipo))
v10$genotipo = str_replace_all(v10$genotipo," ","")

write.csv2(v10, "v10.csv", row.names = F)
getwd()
sum(is.na(v6$produtividade))
write.csv2(gen, "lista_genotipo.csv", row.names = F)


#tabela$cidade = str_replace_all(tabela$cidade," ","")
tabela$id_ensaio = paste(
  tabela$cidade,
  tabela$local,
  tabela$lafra,
  tabela$mes,
  tabela$irrigacao,
  tabela$fungicida,
  sep = "_")
tabela$genotipo = stri_trans_general(str = tabela$genotipo, id = "Latin-ASCII")
hist(tabela$produtividade)
names(tabela)
str(tabela)

col_order <- c("id_ensaio", "estado", "cidade",  "safra", "local","irrigacao","fungicida","tipo_de_grao",
               "genotipo", "repeticao", "produtividade",  "data_semeadura", "data_emergencia", "data_inicio_floracao",
               "data_inicio_ponto_colheita",  "data_inicio_colheita", "mes" )                
my_data2 <- tabela[,..col_order]
names(my_data2)
names(v2)
str(v2)
my_data2 = my_data2[ ,-c(17)]

v3 = filter(v2, safra != "08/09")
table(v3$safra)

v4 = rbind(v3,my_data2)

table(v4$irrigacao)

v4 = v4 %>%
  mutate(irrigacao = case_when(
    irrigacao == "Nao"   ~ "nao",
    irrigacao == "Sim"   ~ "sim" ))

table(v4$cidade)

v5 = v4 %>%
  mutate(cidade = case_when(
    cidade == "PontaGrossa"   ~ "Ponta Grossa",
    TRUE ~ as.character(cidade)))

table(v5$cidade)
sum(is.na(V7$produtividade))
dim(v3)

v6 = v5 %>%
  filter(produtividade == 0)

V7 = v5 %>% replace_with_na(replace = list(produtividade = 0))

Ge = V7 %>%
  select(genotipo, tipo_de_grao) %>%
  arrange(genotipo) %>%
  

write.csv2(gen, "lista_genotipo.csv", row.names = F)

table(v6$cidade)

v7 = v6 %>%
  mutate(genotipo = case_when(
  genotipo == "Uirapuru"   ~ "IPRUirapuru",
  TRUE ~ as.character(genotipo)))%>%
mutate(cidade = case_when(
  cidade == "PontaGrossa"   ~ "Ponta Grossa",
  TRUE ~ as.character(cidade)))

table(v7$estado)
gen = unique(sort(v7$genotipo))

table(tabela$cidade)
teste = filter(tabela, Safra == "1011")
dim(teste)

 t1 = filter(tabela, Safra == "1011" & Local == "TaquaritubaIr")
names(t1)
 dim(t1)
t1 <- t1[,-(13),drop=FALSE]

t2 = fread('Taquaritubair.csv')
total1 <- left_join(t1, t2, by=c("Tratamento", "Repeticao")) #Taquaritubair
names(total1)
dim(total1)
hist(total1$produtividade)
unique(total1$Genotipo)

t3 = filter(tabela, Safra == "1011" & Local == "TakaokaIr")
names(t3)
t3 <- t3[-c(28, 37, 52,101), ]
t3 <- t3[,-(13),drop=FALSE]
t4 = fread('Taquaritubair.csv')
total2 <- left_join(t3, t4, by=c("Tratamento", "Repeticao")) #TakaokaIr
names(total2)
#tabela = read.csv('99_95_v5.csv') 
#tabela$Genotipo = str_replace_all(tabela$Genotipo," ","")
#T1 = read.csv2('Juncao_2000_2020.csv') 
names(tabela)
tabela$Safra
# tabela$produtividade = ((tabela$Gramas* 10)/3.2)/((100-tabela$Umidade)/(100-13))
#  hist(tabela$produtividade)

table(tabela$Data.de.emergencia)
hist(tabela$cidade)

t55 = filter(tabela, Safra == "1011") # & Local != c("TakaokaIr","TaquaritubaIr"))
t54 = filter(t55,Local != "TakaokaIr" )
t5 = filter(t54,Local != "TaquaritubaIr" )
table(t5$Local)
names(t5)
t5 <- t5[,-(13),drop=FALSE]

t6 = fread('total_outras.csv')
total3 <- left_join(t5, t6, by=c("Tratamento", "Repeticao", "Local", "Fungicida")) #
names(total3)

final = rbind.data.frame(total1,total2,total3)
dim(final)
table(final$Safra)
final$Safra 

final = final %>%
  mutate(Safra = "10/11")
  
table(final$Repeticao)
hist(final$produtividade) 

names(final)
T1 = final %>% 
  select(ID_ensaio, UF, cidade, Safra, Local,irrigacao, Fungicida, Genotipo, 
         Repeticao, produtividade, Data.de.plantio, Data.de.emergencia, Data.de.inicio.floracao,
         Data.de.ponto.colheita, Data.de.colheita) %>%                                         
  rename_with(tolower)
names(T1)

T1 =T1 %>% 
  rename(
    estado                         =     uf,
    data_semeadura                 =     data.de.plantio,
    data_emergencia                =     data.de.emergencia,
    data_inicio_floracao           =     data.de.inicio.floracao,
    data_inicio_ponto_colheita     =     data.de.ponto.colheita,
    data_inicio_colheita           =     data.de.colheita )

T1$tipo_de_grao = NA
T1$fungicida
names(T1)

T2 = read.csv2("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v1.csv")
table(T2$safra)
T3 = filter(T2, safra != "10/11")
table(T3$safra)
names(T3)
names(T1)

T4 = rbind(T3,T1)
table(T4$safra)
sum(is.na(T4$produtividade))

Teste = T4 %>% 
  select( id_ensaio,genotipo,produtividade ) %>% 
  filter(produtividade < 100)%>% 
  arrange(id_ensaio)

colSums(is.na(T4))

new_DF<-subset(T4, T4$produtividade == "is.na") 
Teste = T4[is.na(T4$produtividade), ]   

write.csv2(T4, "/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v2.csv", row.names = F)

#----------95/99--------------------------
names(tabela)
unique(tabela$Cultivar)



T1 = tabela %>% 
  select(ID_ensaio, UF, cidade, Safra, Local,irrigacao, Fungicida, Cultivar, 
         Repeticao, produtividade, Data.de.plantio, Data.de.emergencia, Data.de.inicio.floracao,
         Data.de.ponto.de.colheita, Data.de.colheita)  %>%                                                                                                                                                                                                                                                      
  rename_with(tolower)
names(T1)

T1 =T1 %>% 
  rename(
    genotipo                       =     cultivar,
    estado                         =     uf,
    data_semeadura                 =     data.de.plantio,
    data_emergencia                =     data.de.emergencia,
    data_inicio_floracao           =     data.de.inicio.floracao,
    data_inicio_ponto_colheita     =     data.de.ponto.de.colheita,
    data_inicio_colheita           =     data.de.colheita )

T1$tipo_de_grao = NA

#new_DF<-subset(T1, T1$produtividade == "is.na") 

names(T1)
which(is.na(T1$produtividade))
sum(is.na(T1$produtividade))
unique(T1$id_ensaio)

T1 = T1 %>%
  filter(safra != "95/96")
unique(T1$safra)

T1 = T1 %>%
  select(id_ensaio,estado,cidade,safra,local,irrigacao,fungicida,tipo_de_grao,genotipo,                 
          repeticao, produtividade,data_semeadura, data_emergencia, data_inicio_floracao,
         data_inicio_ponto_colheita, data_inicio_colheita)

write.csv(T1,"95_99_v6.csv", row.names = F)

T2 = fread("/media/alexandre/Compartilhado1/Barbara_mestrado_UFG/Projeto-Embrapa/v2.csv")
names(T2)
str(T2)
names(T1)
str(T1)
sum(is.na(T2$data_semeadura))
juncao = rbind(T2, T1)
table(T2$data_semeadura)

data = as.character(T1$data_semeadura)


tabela = tabela %>%
  mutate(ID_ensaio = case_when(
    ID_ensaio == "Castro_CDECastro_199798_12_Nao_com"   ~ "Castro_CDECastro_9798_12_Nao_com",
    ID_ensaio == "Castro_CDECastro_199798_12_Nao_sem"   ~ "Castro_CDECastro_9798_12_Nao_sem",
    TRUE ~ as.character(ID_ensaio)
    ))
names(t1)



tabela = tabela %>%
mutate(Safra = case_when(
  Safra == "199798"                 ~ "97/98",
  Safra == "9697"                   ~ "96/97",
  Safra == "9596"                   ~ "95/96",
  Safra == "9798"                   ~ "97/98",
  Safra == "9899"                   ~ "98/99"))


 
# tabela = tabela %>%
# mutate(Cultivar = case_when(
#   Cultivar == "APORE"                 ~ "Apore",
#   Cultivar == "D.Negro"               ~ "DiamanteNegro",
#   Cultivar == "DIAMANTENEGRO"         ~ "DiamanteNegro",
#   Cultivar == "FtBonito"              ~ "Bonito",
#   Cultivar == "FTBonito"              ~ "Bonito",
#   Cultivar == "FTBONITO"              ~ "Bonito",
#   Cultivar == "FtNobre"               ~ "Nobre",
#   Cultivar == "FTNobre"               ~ "Nobre",
#   Cultivar == "IAC-Carioca-Arua"      ~ "IACCariocaArua",
#   Cultivar == "IAC-Carioca-Pyata"     ~ "IACCariocaPyata",
#   Cultivar == "Iac-CariocaAkyta"      ~ "IACCariocaAkyta",
#   Cultivar == "IAC-CariocaAkyta"      ~ "IACCariocaAkyta",
#   Cultivar == "Iac-CariocaArua"       ~ "IACCariocaArua",
#   Cultivar == "Iac-CariocaPyata"      ~ "IACCariocaPyata",
#   Cultivar == "Iac-Una"               ~ "IACUna",
#   Cultivar == "IAC-Una"               ~ "IACUna",
#   Cultivar == "IAC-UNA"               ~ "IACUna",
#   Cultivar == "IACCARIOCAAKYTA"       ~ "IACCariocaAkyta",
#   Cultivar == "IACCARIOCAARUA"        ~ "IACCariocaArua",
#   Cultivar == "IACCARIOCAPYATA"       ~ "IACCariocaPyata",
#   Cultivar == "Iapar14"               ~ "IAPAR14",
#   Cultivar == "Iapar31"               ~ "IAPAR31",
#   Cultivar == "Iapar44"               ~ "IAPAR44",
#   Cultivar == "Iapar80"               ~ "IAPAR80",
#   Cultivar == "IaparMD841"            ~ "IAPARMD841",
#   Cultivar == "Lp91-1"                ~ "LP91-1",
#   Cultivar == "Lp91-117"              ~ "LP91-117",
#   Cultivar == "MD841"                 ~ "IAPARMD841",
#   TRUE ~ as.character(Cultivar)
# ))


#tabela$produtividade = as.numeric(tabela$produtividade)
names(tabela)

tabela$produtividade
t1 = filter(tabela, Genotipo == "Rud")

T1 = tabela %>% 
  select( ID_ensaio,Cultivar,produtividade ) %>% 
  filter(produtividade < 1000 )%>% 
  arrange(ID_ensaio)

tabela$produtividade = round(tabela$produtividade,0)

correto = fread("dados_safra_12_13_a_13_a_20_21.csv") %>% 
  select(genotipo,tipo_de_grao)  %>% 
 distinct(genotipo, .keep_all = TRUE)%>% 
  arrange(genotipo)

#write.csv2(t1, "99_95_v5.csv", row.names = F)

boxplot(tabela$produtividade)
names(tabela)
unique(tabela$Genotipo)

T1 = tabela %>%
  select(ID_ensaio,Cultivar)%>% 
 distinct(Cultivar, .keep_all = T)%>% 
  arrange(Cultivar)


write.csv(tabela, "99_95_v5.csv", row.names = F)

dataset = tabela %>% 
  filter(safra%in%c("20/21", "19/20") & irrigacao == "True" & fungicida == "True") %>% 
  select(produtividade, repeticao, local, genotipo, safra)

names(dataset) = c("trait","rep", "site", "gid", "year")

prod = tabela %>%
  select(ID_ensaio,Genotipo,tipo_de_grao, Repeticao,Tratamento, produtividade) %>%
 filter (produtividade < 500)

# Lendo dados
# tabela = read.csv('data.csv', stringsAsFactors = F)
# 
# # Filtro dados
# safra = '15/16'
# estado = 'SP'
# cidade = 'Campina do Monte Alegre'
# irrigacao = TRUE
# fungicida = TRUE
# tipoDeGrao = 'preto'

# Funcoes para extrair dados

convertModel = function(list_table, colNames) {
  
  rowValues = strsplit(row.names(list_table), split = ":")
  resposta = do.call(rbind, rowValues)
  
  resposta = data.frame(resposta)
  resposta$hat = list_table[,1]
  names(resposta) = colNames
  
  return(resposta)
}

# Comp.var = function(model,r,s,y) {
#   
#   vcor = VarCorr(model)
#   
#   var.total = sum(vcor$"gid"[1], vcor$"gid:year"[1], vcor$"gid:site"[1], vcor$"gid:site:year"[1],sigma(model)^2)
#   h2.plot = vcor$"gid"[1]/(vcor$"gid"[1]+vcor$"gid:year"[1] + vcor$"gid:site"[1]+vcor$"gid:site:year"[1]+sigma(model)^2)
#   
#   Cgy = vcor$"gid:year"[1]/var.total
#   Cgl = vcor$"gid:year"[1]/var.total
#   Cgly = vcor$"gid:site:year"[1]/var.total
#   
#   h2.mean = vcor$"gid"[1]/(vcor$"gid"[1]+(vcor$"gid:year"[1]/y) + (vcor$"gid:site"[1]/s)+(vcor$"gid:site:year"[1]/(y*s))+(sigma(model)^2)/(y*r*s))
#   return(list(h2.mean=h2.mean,h2.plot=h2.plot,Cgy=Cgy, Cgl = Cgl, Cgly = Cgly))
# }

# Convetendo dados
#tabela$produtividade = as.numeric(tabela$produtividade)

# Gerando o modelo
#dataset = tabela[,c('produtividade', 'repeticao', 'local', 'genotipo', 'safra')]
names(dataset) = c("trait","rep", "site", "gid", "year")
yearBackup = unique(dataset$year)
siteBackup = unique(dataset$site)

r = length(unique(dataset$rep))
y = length(unique(dataset$year))
s = length(unique(dataset$site))


mix.model.an = lmer(trait~site:year+(1|gid)+(1|gid:site) + (1|gid:year) + (1|gid:site:year),data= dataset)

#summary(mix.model.an)
    
rn = ranef(mix.model.an)
    
g = convertModel(rn$"gid", c("gid", "g.hat"))
gy = convertModel(rn$"gid:year", c("gid","year", "gy.hat"))
gl = convertModel(rn$"gid:site", c("gid","site", "gl.hat"))
gly = convertModel(rn$"gid:site:year", c("gid","site","year", "gly.hat"))
    
resposta = merge(g, gy, by = c("gid"))
resposta = merge(resposta, gl, by = c("gid"))
resposta = merge(resposta, gly, by = c("gid","site","year")) 


fn = fixef(mix.model.an)[1]
resposta$y = resposta$g.hat + resposta$gl.hat + resposta$gy.hat + resposta$gly.hat + fn #falta inserir os efeitos fixos a menos do intercept
resposta = resposta[,c('gid','site','year','g.hat','gy.hat','gl.hat','gly.hat','y')]

hat_table = resposta[,4:7]
resposta$y.cor = apply(hat_table, 1, function(x) {
  return(sum(x,na.rm=T) + fn)
})

resp_list = list()
resp_list$Adjusted.Means.df = resposta
resp_list$Mu = fn
#resp_list$comps = Comp.var(mix.model.an,r=r,s=s,y=y)



BRSCampeiroembrapa" ~ "BRSCampeiro"
Carioca75precoce" ~ "Carioca75Precoce"
CNFC7384Perola" ~ "Perola"
CNFE6878JaloPrecoce" ~ "JaloPrecoce"
CNFP7560Valente" ~ "BRSValente"
FTBionobre" ~ "Bionobre"
FTBonito" ~ "Bonito"
FTMagnifico" ~ "Magnifico"
FTNobre" ~ "Nobre"
FTPortoReal" ~ "PortoReal
FTSMagnifico" ~ "Magnifico
FTSNativo" ~ "Nativo
FTSoberano" ~ "Soberano
FTSSoberano" ~ "Soberano
IACCariocaTibata" ~ "IACCariocaTybata
IACFormoso=IACC2-1-3" ~ "IACFormoso
IAC-UNA" ~ "IACUna
Iapar81" ~ "IAPAR81
IPR88Uirapuru" ~ "Uirapuru
IPRCamposGeraisLP01-38" ~ "IPRCamposGerais
IPRUirapuru" ~ "Uirapuru
Selecao2" ~ "Selecao02
UTF2" ~ "UTF2Gaurama
Zonin" ~ "Zonim
