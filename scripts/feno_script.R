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
                  "circular",
                  'CircStats',
                  "phenocamr",
                  "lubridate",
                  "bpnreg",
                  "here",
                  "synchrony",
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

  mean.circular(feno.circ.zoo)
  rho.circular(feno.circ.zoo)
  sqrt(-2*log(rho.circular(feno.circ.zoo)))
  rayleigh.test(feno.circ.zoo)
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
  
  mean(fruit_zoo1$months)
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

  #synch
  #fruit
  fruit_zoo3 <- fruit_zoo %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_zoochory <- master %>%
    filter(Zoocoria == 1)
  
  fruit_zoo4 <- master_zoochory %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  zoochory <- (fruit_zoo3$ind/fruit_zoo4$ind)
  
  
  fruit_auto3 <- fruit_auto %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_autochory <- master %>%
    filter(Autocoria == 1)
  
  fruit_auto4 <- master_autochory %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  autochory <- (fruit_auto3$ind/fruit_auto4$ind)
  
  
  fruit_anemo3 <- fruit_anemo %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  class(fruit_anemo3$month)
  class(fruit_anemo3$ind)
  
  fruit_anemo3[nrow(fruit_anemo3) + 1,] <- list("12", 0)
  
  master_anemochory <- master %>%
    filter(Anemocoria == 1)
  
  fruit_anemo4 <- master_anemochory %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  anemochory <- (fruit_anemo3$ind/fruit_anemo4$ind)
  
  #join all into one dataframe
  sync_fruit <- data.frame(zoochory, autochory, anemochory)
 
  community.sync(sync_fruit, nrands=100)
  
  #flower
  flower_zoo3 <- flower_zoo %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_zoophilia <- master %>%
    filter(Zoofilia == 1)
  
  flower_zoo4 <- master_zoophilia %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  zoophilia <- (flower_zoo3$ind/flower_zoo4$ind)

  
  flower_anemo3 <- fruit_anemo %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  flower_anemo3[nrow(flower_anemo3) + 1,] <- list("12", 0)
  
  master_anemophilia <- master %>%
    filter(Anemofilia == 1)
  
  flower_anemo4 <- master_anemophilia %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  anemophilia <- (flower_anemo3$ind/flower_anemo4$ind)  
  
  #join all into one dataframe
  sync_flower <- data.frame(zoophilia, anemophilia)
  
  community.sync(sync_flower, nrands=100)
  
  #leaffall
  leaffall_sempreverde3 <- leaffall_sempreverde %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_lfsempreverde <- master %>%
    filter(Sempreverde == 1)
  
  leaffall_sempreverde4 <- master_lfsempreverde %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  lfsempreverde <- (leaffall_sempreverde3$ind/leaffall_sempreverde4$ind)
  
  
  leaffall_semidecidua3 <- leaffall_semidecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_lfsemidecidua <- master %>%
    filter(Semidecidua == 1)
  
  leaffall_semidecidua4 <- master_lfsemidecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  lfsemidecidua <- (leaffall_semidecidua3$ind/leaffall_semidecidua4$ind)

  
  leaffall_decidua3 <- leaffall_decidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  leaffall_decidua3[nrow(leaffall_decidua3) + 1,] <- list("12", 0)
  
  master_lfdecidua <- master %>%
    filter(Decidua == 1)
  
  leaffall_decidua4 <- master_lfdecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  lfdecidua <- (leaffall_decidua3$ind/leaffall_decidua4$ind)  

  #join all into one dataframe
  sync_leaffall <- data.frame(lfsempreverde, lfsemidecidua, lfdecidua)
  
  community.sync(sync_leaffall, nrands=100)
  
  #sprouting
  sprouting_sempreverde3 <- sprouting_sempreverde %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_ssempreverde <- master %>%
    filter(Sempreverde == 1)
  
  sprouting_sempreverde4 <- master_ssempreverde %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  ssempreverde <- (sprouting_sempreverde3$ind/sprouting_sempreverde4$ind)

  
  sprouting_semidecidua3 <- sprouting_semidecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  master_ssemidecidua <- master %>%
    filter(Semidecidua == 1)
  
  sprouting_semidecidua4 <- master_ssemidecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  ssemidecidua <- (sprouting_semidecidua3$ind/sprouting_semidecidua4$ind)
  
  
  sprouting_decidua3 <- sprouting_decidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  sprouting_decidua3[nrow(sprouting_decidua3) + 1,] <- list("12", 0)
  
  master_sdecidua <- master %>%
    filter(Decidua == 1)
  
  sprouting_decidua4 <- master_sdecidua %>%
    group_by(month) %>%
    summarise(ind = n_distinct(Tag))
  
  sdecidua <- (sprouting_decidua3$ind/sprouting_decidua4$ind)  
  
  #join all into one dataframe
  sync_sprouting <- data.frame(ssempreverde, ssemidecidua, sdecidua)
  
  community.sync(sync_sprouting, nrands=100)
  