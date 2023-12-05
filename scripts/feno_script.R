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

  arrows.circular(mean(feno.circ.zoo))

  mean.circular(feno.circ.zoo)
  rho.circular(feno.circ.zoo)
  sqrt(-2*log(rho.circular(feno.circ.zoo)))
  
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
  arrows.circular(mean(feno.circ.anemo))
  
  mean.circular(feno.circ.anemo)
  rho.circular(feno.circ.anemo)
  sqrt(-2*log(rho.circular(feno.circ.anemo)))
  
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
  arrows.circular(mean(feno.circ.auto)) 
  
  mean.circular(feno.circ.auto)
  rho.circular(feno.circ.auto)
  sqrt(-2*log(rho.circular(feno.circ.auto)))
  
  
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
  
  arrows.circular(mean(feno.circ.zoofilia))
  
  mean.circular(feno.circ.zoofilia)
  rho.circular(feno.circ.zoofilia)
  sqrt(-2*log(rho.circular(feno.circ.zoofilia)))
  
  var.circular(feno.circ.zoofilia)
  
  #anemophilic
  anemofilia <- fruit %>%
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
  arrows.circular(mean(feno.circ.anemofilia))
  
  mean.circular(feno.circ.anemofilia)
  rho.circular(feno.circ.anemofilia)
  sqrt(-2*log(rho.circular(feno.circ.anemofilia)))
  
  
  #8 Filter Data
  colnames(master)[7] ="leaffall"
  
  colnames(master)[19] ="sempreverde"
  
  colnames(master)[20] ="semidecidua"
  
  class(master$leaffall)
  
  leaffall <- master %>%
    filter(leaffall == 1)
  
#7 LEAF LOSS AND DECIDUOSNESS------------------------------------
  #leaf fall sempre-verde
  sempreverde <- leaffall %>%
    filter(sempreverde == 1)
  
  feno.circ.sempreverde = circular(sempreverde$daysangles, units = "degrees", template = "none", 
                                  modulo = "2pi")
  sempreverde$feno.circ.sempreverde = circular(sempreverde$daysangles, units = "degrees", 
                                        template = "none", modulo = "2pi")
  
  plot(feno.circ.sempreverde, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.sempreverde, axes = FALSE, bins=12, col = "blue", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.sempreverde))
  
  mean.circular(feno.circ.sempreverde)
  rho.circular(feno.circ.sempreverde)
  sqrt(-2*log(rho.circular(feno.circ.sempreverde)))
  
  #leaf fall semi-decidua
  semidecidua <- leaffall %>%
    filter(semidecidua == 1)
  
  feno.circ.semidecidua = circular(semidecidua$daysangles, units = "degrees", template = "none", 
                                   modulo = "2pi")
  semidecidua$feno.circ.semidecidua = circular(semidecidua$daysangles, units = "degrees", 
                                        template = "none", modulo = "2pi")
  
  plot(feno.circ.semidecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, 
       bins = 365, cex = 0.0, rotation = "clock")
  
  circular::rose.diag(feno.circ.semidecidua, axes = FALSE, bins=12, col = "blue", cex = 0.8, 
                      prop =1.3, add = TRUE, rotation = "clock")
  
  axis.circular(at=circular(sort(seq(0, 11/6*pi, pi/6), decreasing = T)), c(labels = c("M", "J",
                                                                                       "J", "A", "S", "O", "N", "D", "J", "F", "M","A")))
  arrows.circular(mean(feno.circ.semidecidua))
  
  mean.circular(feno.circ.semidecidua)
  rho.circular(feno.circ.semidecidua)
  sqrt(-2*log(rho.circular(feno.circ.semidecidua)))
  
  
  
#8 SPROUTING
  