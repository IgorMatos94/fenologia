# 13 de julho 2023

# Hades, Carina I. Motta

#Fenologia - analyses of phenology in naturally recovering secondary forest
#fragments in the Corumbataí River Basin, São Paulo based on dispersion and 
#pollination syndromes as well as deciduousness

#TAREFAS:
  #1 colocar pra que serve cada pacote e porque a gente tá usando 
  #2 annotar cada linha
  #3 repetir as analises para flor e leaf fall 

#1 CARREGAR PACOTES----------------------------------------------------------------

# a vector listing package names needed for importing the DNA sequences,
#calculating genetic distance, calculated geographic distance, and performing
#a Mantel test

package.list <- c("activity", #usei esse pacote...
                  "circular", 
                  "phenology", 
                  "lubridate",
                  "ggplot2",
                  "fastDummies",
                  "circular",
                  'CircStats',
                  "phenocamr",
                  "lubridate",
                  "bpnreg",
                  "here",
                  "synchrony",
                  "pheatmap",
                  "tidyr",
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "stringr" #data cleaning
                  )

#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}


#2 LOAD DATA--------------------------------------------------------------------
#nomeando e fornecendo localização do arquivo a ser aberto
feno <- readr::read_csv2(here::here("dados", "dados.csv"))

#nomeando e fornecendo localização do arquivo a ser aberto. Aqui é utilizado read_csv2 pois o arquivo CSV fica separado por vírgula
traits <- readr::read_csv(here::here("dados", "disp-poll_new.csv"))

#3 DATA CLEANING AND ORGANIZING-------------------------------------------------

#selecionando colunas do arquivo original que serão utilizadas
feno <- feno %>% select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

#Seleciona colunas do arquivo original que serão utilizadas
traits <- traits %>% select(3, 4, 5, 6, 7, 8, 9, 10, 11)

#junta as tabelas feno e traits, organizando por espécie
master <- merge(x=feno, y=traits,
                              by="Species", all.x =T)

#Transforma NA=0
master[is.na(master)] <- 0

#Transforma células vazias em unknown
#master$Dispersion[master$Dispersion == 0] <- "unknown"

#Transforma células vazias em unknown
#master$Pollination[master$Pollination == 0] <- "unknown"

#Transforma células vazias em unknown
#master$Deciduousness[master$Deciduousness == 0] <- "unknown"

#4 CALCULATE DAY OF YEAR AND TRANSFORM TO DEGREE--------------------------------
class(master$DATE)

?as.Date

master$DATE <- as.Date(master$DATE, "%d/%m/%Y")

summary(master)

as.tibble(master)

master$year = format(as.Date(master$DATE), "%Y")

master$month = format(as.Date(master$DATE), "%m")

master$days = lubridate::yday(master$DATE)

master$daysangles = (master$days*360)/365

master <- master %>%
  filter(year == "2022" & month %in% c("07", "08", "09", "10", "11", "12") |
         year == "2023" & month %in% c("01", "02", "03", "04", "05", "06") )

#5 FRUITING PHENOLOGY AND DISPERSAL---------------------------------------------

#5A FILTER DATA
#rename columns because current name format causes error
colnames(master)[11] ="imfruit"

colnames(master)[12] ="mfruit"

class(master$imfruit)

fruit <- master %>%
  filter(imfruit == 1 | mfruit == 1)
  
#5B PLOT -  fruits on all the plots??
  feno.circ = circular(fruit$daysangles, units = "degrees", template = "none", 
                       modulo = "2pi")
  fruit$feno.circ = circular(fruit$daysangles, units = "degrees", 
                             template = "none", modulo = "2pi")
  
  plot(feno.circ, units = "radians",shrink = 1.5, stack = TRUE, pch = 16, 
       bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")
  
  circular::rose.diag(feno.circ, bins=16, col = "darkgrey", cex = 0.8, 
                      prop =1.3, add = TRUE,
                      zero = pi/2, rotation = "clock")

  
#5C STATISTICAL TEST BETWEEN GROUPS
#watson.williams.test(feno.circ~Dispersion, data=fruit)
  
#zoo
  
  zoo <- fruit %>%
    filter(Zoocoria == 1)
  
  feno.circ.zoo = circular(zoo$daysangles, units = "degrees", template = "none", 
                       modulo = "2pi")
  zoo$feno.circ.zoo = circular(zoo$daysangles, units = "degrees", 
                             template = "none", modulo = "2pi")
  
  plot(feno.circ.zoo, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.zoo, axes = FALSE, bins=12, col = "darkgrey", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")

  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))

  arrows.circular(mean(feno.circ.zoo), rho.circular(feno.circ.zoo), zero = pi/2, rotation = "clock", col = "black")

  #média
  mean.circular(feno.circ.zoo)
  
  #comp. do vetor médio
  rho.circular(feno.circ.zoo)
  
  #SD em graus
  sqrt(-2*log(rho.circular(feno.circ.zoo)))
  
  #O teste de Rayleigh assume que os dados têm distribuição normal. Não usado.
  rayleigh.test(feno.circ.zoo)
  
  #Variância circular
  var.circular(feno.circ.zoo)

#anemocoria
  anemo <- fruit %>%
    filter(Anemocoria == 1)
  
  feno.circ.anemo = circular(anemo$daysangles, units = "degrees", template = "none", 
                           modulo = "2pi")
  anemo$feno.circ.anemo = circular(anemo$daysangles, units = "degrees", 
                               template = "none", modulo = "2pi")
  
  plot(feno.circ.anemo, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.anemo, axes = FALSE, bins=12, col = "blue", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.anemo), rho.circular(feno.circ.anemo), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.anemo)
  rho.circular(feno.circ.anemo)
  sqrt(-2*log(rho.circular(feno.circ.anemo)))
  rayleigh.test(feno.circ.anemo)

  #autocoria
  auto <- fruit %>%
    filter(Autocoria == 1)
  
  feno.circ.auto = circular(auto$daysangles, units = "degrees", template = "none", 
                           modulo = "2pi")
  auto$feno.circ.auto = circular(auto$daysangles, units = "degrees", 
                               template = "none", modulo = "2pi")
  
  plot(feno.circ.auto, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.auto, axes = FALSE, bins=12, col = "yellow", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.auto), rho.circular(feno.circ.auto), zero = pi/2, rotation = "clock", col = "black") 
  
  mean.circular(feno.circ.auto)
  rho.circular(feno.circ.auto)
  sqrt(-2*log(rho.circular(feno.circ.auto)))
  rayleigh.test(feno.circ.auto)
  
  watson.williams.test(list(rad(feno.circ.zoo), rad(feno.circ.anemo), rad(feno.circ.auto)))

  #6 Filter Data
  colnames(master)[9] ="BU"
  
  colnames(master)[10] ="FL"
  
  class(master$BU)
  
  flower <- master %>%
    filter(BU == 1 | FL == 1)
  
  #7 FLOWERING PHENOLOGY AND POLLINATION------------------------------------------
  #zoofilic
  
  zoofilia <- flower %>%
    filter(Zoofilia == 1)
  
  feno.circ.zoofilia = circular(zoofilia$daysangles, units = "degrees", template = "none", 
                           modulo = "2pi")
  zoofilia$feno.circ.zoofilia = circular(zoofilia$daysangles, units = "degrees", 
                               template = "none", modulo = "2pi")
  
  plot(feno.circ.zoofilia, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.zoofilia, axes = FALSE, bins=12, col = "darkgrey", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  
  arrows.circular(mean(feno.circ.zoofilia), rho.circular(feno.circ.zoofilia), zero = pi/2, rotation = "clock")
  
  mean.circular(feno.circ.zoofilia)
  rho.circular(feno.circ.zoofilia)
  sqrt(-2*log(rho.circular(feno.circ.zoofilia)))
  rayleigh.test(feno.circ.zoofilia)
  var.circular(feno.circ.zoofilia)
  
  #anemophilic
  anemofilia <- flower %>%
    filter(Anemofilia == 1)
  
  feno.circ.anemofilia = circular(anemofilia$daysangles, units = "degrees", template = "none", 
                             modulo = "2pi")
  anemofilia$feno.circ.anemofilia = circular(anemofilia$daysangles, units = "degrees", 
                                   template = "none", modulo = "2pi")
  
  plot(feno.circ.anemofilia, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.anemofilia, axes = FALSE, bins=12, col = "blue", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.anemofilia), rho.circular(feno.circ.anemofilia), zero = pi/2, rotation = "clock")
  
  
  mean.circular(feno.circ.anemofilia)
  rho.circular(feno.circ.anemofilia)
  sqrt(-2*log(rho.circular(feno.circ.anemofilia)))
  rayleigh.test(feno.circ.anemofilia)
  watson.williams.test(list(rad(feno.circ.zoofilia), rad(feno.circ.anemofilia)))

  #8 Filter Data
  colnames(master)[7] ="Leaffall"
  
  colnames(master)[8] = "Sprouting"
  
  colnames(master)[19] ="Sempreverde"
  
  colnames(master)[20] ="Semidecidua"
  
  colnames(master)[21] ="Decidua"
  
  class(master$leaffall)
  
  leaffall <- master %>%
    filter(Leaffall == 1)
  
#9 LEAF LOSS AND DECIDUOSNESS------------------------------------
  #leaf fall sempre-verde
  sempreverde <- leaffall %>%
    filter(Sempreverde == 1)
  
  feno.circ.sempreverde = circular(sempreverde$daysangles, units = "degrees", template = "none", 
                                  modulo = "2pi")
  sempreverde$feno.circ.sempreverde = circular(sempreverde$daysangles, units = "degrees", 
                                        template = "none", modulo = "2pi")
  
  plot(feno.circ.sempreverde, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.sempreverde, axes = FALSE, bins=12, col ="green", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.sempreverde), rho.circular(feno.circ.sempreverde), zero = pi/2, rotation = "clock")
  
  mean.circular(feno.circ.sempreverde)
  rho.circular(feno.circ.sempreverde)
  sqrt(-2*log(rho.circular(feno.circ.sempreverde)))
  rayleigh.test(feno.circ.sempreverde)
  
  #leaf fall semi-decidua
  semidecidua <- leaffall %>%
    filter(Semidecidua == 1)
  
  feno.circ.semidecidua = circular(semidecidua$daysangles, units = "degrees", template = "none", 
                                   modulo = "2pi")
  semidecidua$feno.circ.semidecidua = circular(semidecidua$daysangles, units = "degrees", 
                                        template = "none", modulo = "2pi")
  
  plot(feno.circ.semidecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.semidecidua, axes = FALSE, bins=12, col = "yellow", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.semidecidua), rho.circular(feno.circ.semidecidua), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.semidecidua)
  rho.circular(feno.circ.semidecidua)
  sqrt(-2*log(rho.circular(feno.circ.semidecidua)))
  rayleigh.test(feno.circ.semidecidua)
  
  #leaffall decidua
  decidua <- leaffall %>%
    filter(Decidua == 1)
  
  feno.circ.decidua = circular(decidua$daysangles, units = "degrees", template = "none", 
                                   modulo = "2pi")
  decidua$feno.circ.decidua = circular(decidua$daysangles, units = "degrees", 
                                               template = "none", modulo = "2pi")
  
  plot(feno.circ.decidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.decidua, axes = FALSE, bins=12, col = "brown", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.decidua), rho.circular(feno.circ.decidua), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.decidua)
  rho.circular(feno.circ.decidua)
  sqrt(-2*log(rho.circular(feno.circ.decidua)))
  rayleigh.test(feno.circ.decidua)
  
  watson.williams.test(list(rad(feno.circ.sempreverde), rad(feno.circ.semidecidua), rad(feno.circ.decidua)))

  
  #10 SPROUTING
  #Filter DAta
  
  sprouting <- master %>%
    filter(Sprouting == 1)
  
  #sprouting sempreverde
  ssempreverde <- sprouting %>%
    filter(Sempreverde == 1)
  
  feno.circ.ssempreverde = circular(ssempreverde$daysangles, units = "degrees", template = "none", 
                                   modulo = "2pi")
  feno.circ.ssempreverde = circular(ssempreverde$daysangles, units = "degrees", template = "none", 
                                  modulo = "2pi")
  ssempreverde$feno.circ.ssempreverde = circular(ssempreverde$daysangles, units = "degrees", 
                                               template = "none", modulo = "2pi")
  feno.circ.ssempreverde = circular(ssempreverde$daysangles, units = "degrees", template = "none", 
                                  modulo = "2pi")
  plot(feno.circ.ssempreverde, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.ssempreverde, axes = FALSE, bins=12, col = "green", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.ssempreverde), rho.circular(feno.circ.sempreverde), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.ssempreverde)
  rho.circular(feno.circ.ssempreverde)
  sqrt(-2*log(rho.circular(feno.circ.ssempreverde)))
  rayleigh.test(feno.circ.ssempreverde)
  
  #sprouting semidecidua
  ssemidecidua <- sprouting %>%
    filter(Semidecidua == 1)
  
  feno.circ.ssemidecidua = circular(ssemidecidua$daysangles, units = "degrees", template = "none", 
                                   modulo = "2pi")
  ssemidecidua$feno.circ.ssemidecidua = circular(ssemidecidua$daysangles, units = "degrees", 
                                               template = "none", modulo = "2pi")
  
  plot(feno.circ.ssemidecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.ssemidecidua, axes = FALSE, bins=12, col = "yellow", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.ssemidecidua), rho.circular(feno.circ.semidecidua), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.ssemidecidua)
  rho.circular(feno.circ.ssemidecidua)
  sqrt(-2*log(rho.circular(feno.circ.ssemidecidua)))
  rayleigh.test(feno.circ.ssemidecidua)
  
  #sprouting decidua
  sdecidua <- sprouting %>%
    filter(Decidua == 1)
  
  feno.circ.sdecidua = circular(sdecidua$daysangles, units = "degrees", template = "none", 
                               modulo = "2pi")
  sdecidua$feno.circ.sdecidua = circular(sdecidua$daysangles, units = "degrees", 
                                       template = "none", modulo = "2pi")
  
  plot(feno.circ.sdecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.sdecidua, axes = FALSE, bins=12, col = "brown", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.sdecidua), rho.circular(feno.circ.decidua), zero = pi/2, rotation = "clock", col = "black")
  
  mean.circular(feno.circ.sdecidua)
  rho.circular(feno.circ.sdecidua)
  sqrt(-2*log(rho.circular(feno.circ.sdecidua)))
  rayleigh.test(feno.circ.sdecidua)
  
  watson.williams.test(list(rad(feno.circ.ssempreverde), rad(feno.circ.ssemidecidua), rad(feno.circ.sdecidua)))

  #duração
  #fruits
  fruit_zoo <- fruit %>%
    filter(Zoocoria == 1)
  
  fruit_zoo1 <- fruit_zoo %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  fruit_zoo2 <- fruit_zoo %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  #duração por espécie
  mean(fruit_zoo1$months)
  
  #duração por indivíduo
  mean(fruit_zoo2$months)
  
  fruit_auto <- fruit %>%
    filter(Autocoria == 1)
  
  fruit_auto1 <- fruit_auto %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  fruit_auto2 <- fruit_auto %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(fruit_auto1$months)
  mean(fruit_auto2$months)
  
  fruit_anemo <- fruit %>%
    filter(Anemocoria == 1)
  
  fruit_anemo1 <- fruit_anemo %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  fruit_anemo2 <- fruit_anemo %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(fruit_anemo1$months)
  mean(fruit_anemo2$months)
  
  
  #flower
  flower_zoo <- flower %>%
    filter(Zoofilia == 1)
  
  flower_zoo1 <- flower_zoo %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  flower_zoo2 <- flower_zoo %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(flower_zoo1$months)
  mean(flower_zoo2$months)

  flower_anemo <- flower %>%
    filter(Anemofilia == 1)
  
  flower_anemo1 <- flower_anemo %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  flower_anemo2 <- flower_anemo %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(flower_anemo1$months)
  mean(flower_anemo2$months)  
  
  #leaffall
  leaffall_sempreverde <- leaffall %>%
    filter(Sempreverde == 1)
  
  leaffall_sempreverde1 <- leaffall_sempreverde %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  leaffall_sempreverde2 <- leaffall_sempreverde %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(leaffall_sempreverde1$months)
  mean(leaffall_sempreverde2$months)  

  
  leaffall_semidecidua <- leaffall %>%
    filter(Semidecidua == 1)
  
  leaffall_semidecidua1 <- leaffall_semidecidua%>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  leaffall_semidecidua2 <- leaffall_semidecidua %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(leaffall_semidecidua1$months)
  mean(leaffall_semidecidua2$months)  

  
  leaffall_decidua <- leaffall %>%
    filter(Decidua == 1)
  
  leaffall_decidua1 <- leaffall_decidua %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  leaffall_decidua2 <- leaffall_decidua %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(leaffall_decidua1$months)
  mean(leaffall_decidua2$months)  

  #  sprouting
  sprouting_sempreverde <- sprouting %>%
    filter(Sempreverde == 1)
  
  sprouting_sempreverde1 <- sprouting_sempreverde %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  sprouting_sempreverde2 <- sprouting_sempreverde %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(sprouting_sempreverde1$months)
  mean(sprouting_sempreverde2$months)  

  
  sprouting_semidecidua <- leaffall %>%
    filter(Semidecidua == 1)
  
  sprouting_semidecidua1 <- sprouting_semidecidua %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  sprouting_semidecidua2 <- sprouting_semidecidua %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(sprouting_semidecidua1$months)
  mean(sprouting_semidecidua2$months)  

  
  sprouting_decidua <- leaffall %>%
    filter(Decidua == 1)
  
  sprouting_decidua1 <- sprouting_decidua %>%
    group_by(Species) %>%
    summarise(months = n_distinct(month))
  
  sprouting_decidua2 <- sprouting_decidua %>%
    group_by(Tag) %>%
    summarise(months = n_distinct(month))
  
  mean(sprouting_decidua1$months)
  mean(sprouting_decidua2$months)  

  #filter master spreadsheet to only include the species that have more than 3
  #individuals
  
  gr8r_3 <- master %>% 
    group_by(Species) %>% 
    filter(n_distinct(Tag) >= 3)
  
  #sync
  #filter again to only include fruting individuals (with immature or mature 
  #fruits)
  fruit <- gr8r_3 %>%
    filter(imfruit == 1 | mfruit == 1)
  
  #filter to only include zoochorous species 
  fruit_zooc <- fruit %>%
    filter(Zoocoria == 1)
  
  #calculate the number of fruiting individuals per species, per month
  fruit_zooc_new <- fruit_zooc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  zooc <- gr8r_3 %>%
    filter(Zoocoria == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  fruit_zooc_new2 <- zooc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  fruit_zooc_new3 <- fruit_zooc_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  fruit_zooc_new4 <- fruit_zooc_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(fruit_zooc_new4$Species)
  
  Species <- c( "Cupania vernalis Cambess.", 
                "Cupania vernalis Cambess.",
                "Cupania vernalis Cambess.",
                "Andira fraxinifolia Benth.",
                "Andira fraxinifolia Benth.",
                "Ocotea puberula (Rich.) Nees", 
                "Ocotea puberula (Rich.) Nees",                             
                "Eugenia pyriformis Cambess.",                              
                "Lafoensia pacari A.St.-Hil.",                             
                "Lithraea molleoides (Vell.) Engl.",                       
                "Myrcia tomentosa (Aubl.) DC.",                             
                "Tapirira guianensis Aubl.",                                
                "Zanthoxylum monogynum A.St.-Hil.",                         
                "Zanthoxylum riedelianum Engl.")
  
  month <- c("02", "05", "10", "02", "12", "03", "05", "05", 
             "12", "05", "12", "12", "12", "05")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(fruit_zooc_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=fruit_zooc_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_zooc <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_zooc <- as.numeric(merge1$prop_zooc)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_zooc <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_zooc) 
  
  #excluding columns with no activity
  matrix_zooc_n <- matrix_zooc[, -c(3, 4, 7, 9, 10, 11, 13, 18, 19, 20,
                                    21, 23, 27)]
  
  
  #make the first column the row names
  matrix_zooc_df <- matrix_zooc_n[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_zooc_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_zooc_matrix <- cor(matrix_zooc_df, method = "pearson")
  
  heatmap(correlation_zooc_matrix)
  
  #filter to only include anemochorous species 
  fruit_anemoc <- fruit %>%
    filter(Anemocoria == 1)
  
  #calculate the number of fruiting individuals per species, per month
  fruit_anemoc_new <- fruit_anemoc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include anemochorous tree species 
  anemoc <- gr8r_3 %>%
    filter(Anemocoria == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  fruit_anemoc_new2 <- anemoc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  fruit_anemoc_new3 <- fruit_anemoc_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  fruit_anemoc_new4 <- fruit_anemoc_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(fruit_anemoc_new4$Species)
  
  Species <- c( "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
                "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
                "Cariniana estrellensis (Raddi) Kuntze"                        ,
                "Centrolobium tomentosum Guillem. ex Benth."                   ,
                "Eucalyptus spp."                                              ,
                "Lafoensia pacari A.St.-Hil."                                  ,
                "Lonchocarpus cultratus (Vell.) A.M.G.Azevedo & H.C.Lima"      ,
                "Machaerium stipitatum Vogel"                                  ,
                "Myracrodruon urundeuva Allemao")
  
  month <- c("03", "05", "05", "05", "12", "12", "05", "05", "05")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(fruit_anemoc_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=fruit_anemoc_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_anemoc <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_anemoc <- as.numeric(merge1$prop_anemoc)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_anemoc <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_anemoc) 
  
  #excluding columns with no activity
  matrix_anemoc_n <- matrix_anemoc[, -c(2, 3, 4, 6, 9, 11, 14)]
  
  
  #make the first column the row names
  rownames(matrix_zooc_n) <- matrix_anemoc$month #doesn't work
  matrix_anemoc_df <- matrix_anemoc_n[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_anemoc_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_anemoc_matrix <- cor(matrix_anemoc_df, method = "pearson")

  
    
  #filter to only include autocorous species 
  fruit_autoc <- fruit %>%
    filter(Autocoria == 1)
  
  #calculate the number of fruiting individuals per species, per month
  fruit_autoc_new <- fruit_autoc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include anemochorous tree species 
  autoc <- gr8r_3 %>%
    filter(Autocoria == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  fruit_autoc_new2 <- autoc %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  fruit_autoc_new3 <- fruit_autoc_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  fruit_autoc_new4 <- fruit_autoc_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(fruit_autoc_new4$Species)
  
  Species <- c( "Albizia niopoides (Spruce ex Benth.) Burkart"     ,      
                "Albizia niopoides (Spruce ex Benth.) Burkart" ,
                "Cariniana estrellensis (Raddi) Kuntze"         ,         
                "Lafoensia pacari A.St.-Hil."                    ,        
                "Lonchocarpus cultratus (Vell.) A.M.G.Azevedo & H.C.Lima",
                "Machaerium stipitatum Vogel"                            ,
                "Sebastiania brasiliensis Spreng.")
  
  month <- c("03", "05", "05", "12", "05", "05", "12")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(fruit_autoc_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=fruit_autoc_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_autoc <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_autoc <- as.numeric(merge1$prop_autoc)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_autoc <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_autoc) 
  
  #excluding columns with no activity
  matrix_autoc_n <- matrix_autoc[, -c(2, 4, 6, 9, 13)]
  
  
  #make the first column the row names
  rownames(matrix_autoc_n) <- matrix_autoc$month #doesn't work
  matrix_autoc_df <- matrix_autoc_n[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_autoc_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_autoc_matrix <- cor(matrix_autoc_df, method = "pearson")

    
  #filter again to only include flowering individuals (with immature or mature 
  #fruits)
  flower <- gr8r_3 %>%
    filter(BU == 1 | FL == 1)
  
  #filter to only include zoophilic species 
  flower_zoof <- flower %>%
    filter(Zoofilia == 1)
  
  #calculate the number of fruiting individuals per species, per month
  flower_zoof_new <- flower_zoof %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoophilic tree species 
  zoof <- gr8r_3 %>%
    filter(Zoofilia == 1)
  
  #calculate total number of zoophilic fruiting individuals 
  flower_zoof_new2 <- zoof %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  flower_zoof_new3 <- flower_zoof_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  flower_zoof_new4 <- flower_zoof_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(flower_zoof_new4$Species)
  
  Species <- c( "Cupania vernalis Cambess."                                ,
                "Cupania vernalis Cambess."                                ,
                "Cupania vernalis Cambess."                                ,
                "Albizia niopoides (Spruce ex Benth.) Burkart"              ,
                "Albizia niopoides (Spruce ex Benth.) Burkart"              ,
                "Andira fraxinifolia Benth."                                 ,
                "Andira fraxinifolia Benth."                                 ,
                "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
                "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
                "Ocotea puberula (Rich.) Nees"                                 ,
                "Ocotea puberula (Rich.) Nees"                                 ,
                "Cariniana estrellensis (Raddi) Kuntze"                        ,
                "Centrolobium tomentosum Guillem. ex Benth."                   ,
                "Eucalyptus spp."                                              ,
                "Eugenia pyriformis Cambess."                                  ,
                "Lafoensia pacari A.St.-Hil."                                  ,
                "Lithraea molleoides (Vell.) Engl."                            ,
                "Lonchocarpus cultratus (Vell.) A.M.G.Azevedo & H.C.Lima"      ,
                "Machaerium stipitatum Vogel"                                  ,
                "Myracrodruon urundeuva Allemao"                               ,
                "Myrcia tomentosa (Aubl.) DC."                                 ,
                "Sebastiania brasiliensis Spreng."                             ,
                "Tapirira guianensis Aubl."                                    ,
                "Zanthoxylum monogynum A.St.-Hil."                             ,
                "Zanthoxylum riedelianum Engl.")
  
  month <- c("02", "05", "10", "03", "05", "02", "12", "03", "05",
             "03", "05", "05", "05", "12", "05", "12", "05", "05",
             "05", "05", "12", "12", "12", "12", "05")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(flower_zoof_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=flower_zoof_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_zoof <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_zoof <- as.numeric(merge1$prop_zoof)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_zoof <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_zoof) 
  
  #excluding columns with no activity
  matrix_zoof_n <- matrix_zoof[, -c(4, 13, 17, 21, 26, 29, 32, 34, 38, 40, 48)]
  
  
  #make the first column the row names
  rownames(matrix_zoof_n) <- matrix_zoof$month
  matrix_zoof_df <- matrix_zoof_n[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_zoof_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_zoof_matrix <- cor(matrix_zoof_df, method = "pearson")
  
  
  
  #filter again to only include flowering individuals (with immature or mature 
  #fruits)
  flower <- gr8r_3 %>%
    filter(BU == 1 | FL == 1)
  
  #filter to only include anemophilic species 
  flower_anemof <- flower %>%
    filter(Anemofilia == 1)
  
  #calculate the number of fruiting individuals per species, per month
  flower_anemof_new <- flower_anemof %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  anemof <- gr8r_3 %>%
    filter(Anemofilia == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  flower_anemof_new2 <- anemof %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  flower_anemof_new3 <- flower_anemof_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  flower_anemof_new4 <- flower_anemof_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(flower_anemof_new4$Species)
  
  Species <- c("Sebastiania brasiliensis Spreng.")
  
  month <- c("12")
  
  
  ind <- c(0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(flower_anemof_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=flower_anemof_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_anemof <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_anemof <- as.numeric(merge1$prop_anemof)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_anemof <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_anemof) 
  
  #excluding columns with no activity
  matrix_anemof_n <- matrix_anemof[, -c(2)]
  
  
  #make the first column the row names
  rownames(matrix_anemof_n) <- matrix_anemof$month
  matrix_anemof_df <- matrix_anemof_n[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_anemof_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_anemof_matrix <- cor(matrix_anemof_df, method = "pearson")

  
  
    
  #filter again to only include sprouting individuals
  sprouting <- gr8r_3 %>%
    filter(Sprouting == 1)
  
  #filter to only include sprouting species 
  sprouting_sempreverde <- sprouting %>%
    filter(Sempreverde == 1)
  
  #calculate the number of fruiting individuals per species, per month
  sprouting_sempreverde_new <- sprouting_sempreverde %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  sempreverde <- gr8r_3 %>%
    filter(Sempreverde == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  sprouting_sempreverde_new2 <- sempreverde %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  sprouting_sempreverde_new3 <- sprouting_sempreverde_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  sprouting_sempreverde_new4 <- sprouting_sempreverde_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(sprouting_sempreverde_new4$Species)
  
  Species <- c("Cupania vernalis Cambess."                  ,
               "Cupania vernalis Cambess."                  ,
               "Cupania vernalis Cambess."                  ,
               "Andira fraxinifolia Benth."                 ,
               "Andira fraxinifolia Benth."                 ,
               "Ocotea puberula (Rich.) Nees"               ,
               "Ocotea puberula (Rich.) Nees"               ,
               "Lithraea molleoides (Vell.) Engl."          ,
               "Machaerium stipitatum Vogel"                ,
               "Tapirira guianensis Aubl."                  ,
               "Zanthoxylum monogynum A.St.-Hil.")
  
  month <- c("02", "05", "10", "02", "12", "03", "05", "05", "05", "12",
             "12")

  
  ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(sprouting_sempreverde_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=sprouting_sempreverde_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_sprouting_sempreverde <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_sprouting_sempreverde <- as.numeric(merge1$prop_sprouting_sempreverde)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_sprouting_sempreverde <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_sprouting_sempreverde) 
  
  #excluding columns with no activity
  matrix_sprouting_sempreverde_n <- matrix_sprouting_sempreverde[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_sprouting_sempreverde_n) <- matrix_sprouting_sempreverde$month
  matrix_sprouting_sempreverde_df <- matrix_sprouting_sempreverde[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_sempreverde_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_sprouting_sempreverde_matrix <- cor(matrix_sprouting_sempreverde_df, method = "pearson")
  
  
  
  #filter again to only include sprouting individuals
  sprouting <- gr8r_3 %>%
    filter(Sprouting == 1)
  
  #filter to only include sprouting species 
  sprouting_semidecidua <- sprouting %>%
    filter(Semidecidua == 1)
  
  #calculate the number of fruiting individuals per species, per month
  sprouting_semidecidua_new <- sprouting_semidecidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  semidecidua <- gr8r_3 %>%
    filter(Semidecidua == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  sprouting_semidecidua_new2 <- semidecidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  sprouting_semidecidua_new3 <- sprouting_semidecidua_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  sprouting_semidecidua_new4 <- sprouting_semidecidua_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(sprouting_semidecidua_new4$Species)
  
  Species <- c("Albizia niopoides (Spruce ex Benth.) Burkart"           ,
               "Albizia niopoides (Spruce ex Benth.) Burkart"           ,
               "Cariniana estrellensis (Raddi) Kuntze"                  ,
               "Eugenia pyriformis Cambess."                            ,
               "Lafoensia pacari A.St.-Hil."                            ,
               "Lonchocarpus cultratus (Vell.) A.M.G.Azevedo & H.C.Lima",
               "Sebastiania brasiliensis Spreng.")
  
  month <- c("03", "05", "05", "05", "12", "05", "12")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(sprouting_semidecidua_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=sprouting_semidecidua_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_sprouting_semidecidua <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_sprouting_semidecidua <- as.numeric(merge1$prop_sprouting_semidecidua)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_sprouting_semidecidua <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_sprouting_semidecidua) 
  
  #excluding columns with no activity
  matrix_sprouting_semidecidua_n <- matrix_sprouting_semidecidua[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_sprouting_semidecidua_n) <- matrix_sprouting_semidecidua$month
  matrix_sprouting_semidecidua_df <- matrix_sprouting_semidecidua[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_semidecidua_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_sprouting_semidecidua_matrix <- cor(matrix_sprouting_semidecidua_df, method = "pearson")
  
  
  
  #filter again to only include sprouting individuals
  sprouting <- gr8r_3 %>%
    filter(Sprouting == 1)
  
  #filter to only include sprouting species 
  sprouting_decidua <- sprouting %>%
    filter(Decidua == 1)
  
  #calculate the number of fruiting individuals per species, per month
  sprouting_decidua_new <- sprouting_decidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  decidua <- gr8r_3 %>%
    filter(Decidua == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  sprouting_decidua_new2 <- decidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  sprouting_decidua_new3 <- sprouting_decidua_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  sprouting_decidua_new4 <- sprouting_decidua_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(sprouting_decidua_new4$Species)
  
  Species <- c("Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
               "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
               "Centrolobium tomentosum Guillem. ex Benth."                   ,
               "Myracrodruon urundeuva Allemao"                               ,
               "Myrcia tomentosa (Aubl.) DC."                                 ,
               "Zanthoxylum riedelianum Engl.")
  
  month <- c("03", "05", "05", "05", "12", "05")
  
  
  ind <- c(0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(sprouting_decidua_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=sprouting_decidua_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_sprouting_decidua <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_sprouting_decidua <- as.numeric(merge1$prop_sprouting_decidua)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_sprouting_decidua <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_sprouting_decidua) 
  
  #excluding columns with no activity
  matrix_sprouting_semidecidua_n <- matrix_sprouting_semidecidua[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_sprouting_decidua) <- matrix_sprouting_decidua$month
  matrix_sprouting_decidua_df <- matrix_sprouting_decidua[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_decidua_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_sprouting_decidua_matrix <- cor(matrix_sprouting_decidua_df, method = "pearson")
  
  
  
  #filter again to only include leaffall individuals
  leaffall <- gr8r_3 %>%
    filter(Leaffall == 1)
  
  #filter to only include leaffalling species 
  leaffall_sempreverde <- leaffall %>%
    filter(Sempreverde == 1)
  
  #calculate the number of individuals with leaffall per species, per month
  leaffall_sempreverde_new <- leaffall_sempreverde %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include tree species with leaffall
  sempreverde <- gr8r_3 %>%
    filter(Sempreverde == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  leaffall_sempreverde_new2 <- sempreverde %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  leaffall_sempreverde_new3 <- leaffall_sempreverde_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  leaffall_sempreverde_new4 <- leaffall_sempreverde_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(leaffall_sempreverde_new4$Species)
  
  Species <- c("Cupania vernalis Cambess."                  ,
               "Cupania vernalis Cambess."                  ,
               "Cupania vernalis Cambess."                  ,
               "Andira fraxinifolia Benth."                 ,
               "Andira fraxinifolia Benth."                 ,
               "Ocotea puberula (Rich.) Nees"               ,
               "Ocotea puberula (Rich.) Nees"               ,
               "Lithraea molleoides (Vell.) Engl."          ,
               "Machaerium stipitatum Vogel"                ,
               "Tapirira guianensis Aubl."                  ,
               "Zanthoxylum monogynum A.St.-Hil.")
  
  month <- c("02", "05", "10", "02", "12", "03", "05", "05", "05", "12", "12")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(leaffall_sempreverde_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=leaffall_sempreverde_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_leaffall_sempreverde <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_leaffall_sempreverde <- as.numeric(merge1$prop_leaffall_sempreverde)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_leaffall_sempreverde <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_leaffall_sempreverde) 
  
  #excluding columns with no activity
  matrix_sprouting_sempreverde_n <- matrix_sprouting_sempreverde[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_leaffall_sempreverde) <- matrix_leaffall_sempreverde$month
  matrix_leaffall_sempreverde_df <- matrix_leaffall_sempreverde[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_sempreverde_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_leaffall_sempreverde_matrix <- cor(matrix_leaffall_sempreverde_df, method = "pearson")
  
  
  
  #filter again to only include sprouting individuals
  leaffall <- gr8r_3 %>%
    filter(Leaffall == 1)
  
  #filter to only include sprouting species 
  leaffall_semidecidua <- leaffall %>%
    filter(Semidecidua == 1)
  
  #calculate the number of fruiting individuals per species, per month
  leaffall_semidecidua_new <- leaffall_semidecidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  semidecidua <- gr8r_3 %>%
    filter(Semidecidua == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  leaffall_semidecidua_new2 <- semidecidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  leaffall_semidecidua_new3 <- leaffall_semidecidua_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  leaffall_semidecidua_new4 <- leaffall_semidecidua_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(leaffall_semidecidua_new4$Species)
  
  Species <- c("Albizia niopoides (Spruce ex Benth.) Burkart"           ,
               "Albizia niopoides (Spruce ex Benth.) Burkart"           ,
               "Cariniana estrellensis (Raddi) Kuntze"                  ,
               "Eugenia pyriformis Cambess."                            ,
               "Lafoensia pacari A.St.-Hil."                            ,
               "Lonchocarpus cultratus (Vell.) A.M.G.Azevedo & H.C.Lima",
               "Sebastiania brasiliensis Spreng.")
  
  month <- c("03", "05", "05", "05", "12", "05", "12")
  
  
  ind <- c(0, 0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(leaffall_semidecidua_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=leaffall_semidecidua_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_leaffall_semidecidua <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_leaffall_semidecidua <- as.numeric(merge1$prop_leaffall_semidecidua)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_leaffall_semidecidua <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_leaffall_semidecidua) 
  
  #excluding columns with no activity
  matrix_sprouting_semidecidua_n <- matrix_sprouting_semidecidua[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_leaffall_semidecidua) <- matrix_leaffall_semidecidua$month
  matrix_leaffall_semidecidua_df <- matrix_leaffall_semidecidua[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_semidecidua_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_leaffall_semidecidua_matrix <- cor(matrix_leaffall_semidecidua_df, method = "pearson")
  
  
  
  #filter again to only include sprouting individuals
  leaffall <- gr8r_3 %>%
    filter(Leaffall == 1)
  
  #filter to only include sprouting species 
  leaffall_decidua <- leaffall %>%
    filter(Decidua == 1)
  
  #calculate the number of fruiting individuals per species, per month
  leaffall_decidua_new <- leaffall_decidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #filter to only include zoochorous tree species 
  decidua <- gr8r_3 %>%
    filter(Decidua == 1)
  
  #calculate total number of zoochorous fruiting individuals 
  leaffall_decidua_new2 <- decidua %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  #see which months we didn't survey any individuals of that species, we need 
  #to have the number of indiviudals for every month, even if it's 0
  leaffall_decidua_new3 <- leaffall_decidua_new2 %>%
    group_by(Species) %>%
    summarise(month = n_distinct(month))
  
  #arrange sheet so its easier to see which species need fixing 
  leaffall_decidua_new4 <- leaffall_decidua_new3 %>%
    arrange(month)
  
  #create a new dataframe with the missing data 
  list(leaffall_decidua_new4$Species)
  
  Species <- c("Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
               "Dahlstedtia muehlbergiana (Hassl.) M.J.Silva & A.M.G. Azevedo",
               "Centrolobium tomentosum Guillem. ex Benth."                   ,
               "Myracrodruon urundeuva Allemao"                               ,
               "Myrcia tomentosa (Aubl.) DC."                                 ,
               "Zanthoxylum riedelianum Engl.")
  
  month <- c("03", "05", "05", "05", "12", "05")
  
  
  ind <- c(0, 0, 0, 0, 0, 0)
  
  missing_data <- data.frame(Species, month, ind)
  
  
  #bind missing data to original data set 
  full.data <- bind_rows(leaffall_decidua_new2, missing_data)
  
  #merge the spreadsheet with the total number of individuals and the number of
  #fruiting individuals 
  merge <- merge(x=full.data, y=leaffall_decidua_new,
                 by=c("Species", "month"), all.x =T)
  
  #calculate the proportion of fruiting individuals 
  merge$prop_leaffall_decidua <- (merge$ind.y/merge$ind.x)
  
  #fill the NAs with 0
  merge[is.na(merge)] <- 0
  
  
  #select the three columns that we want to transform into a matrix 
  merge1 <- merge %>% select(1, 2, 5)
  
  merge1$prop_leaffall_decidua <- as.numeric(merge1$prop_leaffall_decidua)
  
  
  #create the matrix where each column is a species and each row is a month
  matrix_leaffall_decidua <- merge1 %>%
    pivot_wider(names_from = Species,
                values_from = prop_leaffall_decidua) 
  
  #excluding columns with no activity
  matrix_leaffall_decidua_n <- matrix_leaffall_decidua[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_leaffall_decidua) <- matrix_leaffall_decidua$month
  matrix_leaffall_decidua_df <- matrix_leaffall_decidua[, -1]
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_decidua_df, nrands = 100)
  
  #create a correlation matrix --> but how to we deal with the NAs?? Why are 
  #there so many NAs??
  correlation_leaffall_decidua_matrix <- cor(matrix_leaffall_decidua_df, method = "pearson")
  
  
  citation()
  