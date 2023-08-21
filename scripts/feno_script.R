# 13 de julho 2023

# Hades, Carina I. Motta

#Fenologia - analyses of phenology in naturally recovering secondary forest
#fragments in the Corumbataí River Basin, São Paulo based on dispersion and 
#pollination syndromes as well as deciduousness



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


##LOAD DATA###
#nomeando e fornecendo localização do arquivo a ser aberto
feno <- readr::read_csv(here::here("dados", "feno_new.csv"))

#selecionando colunas do arquivo original que serão utilizadas
feno <- feno %>% select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

#nomeando e fornecendo localização do arquivo a ser aberto. Aqui é utilizado read_csv2 pois o arquivo CSV fica separado por vírgula
traits <- readr::read_csv2(here::here("dados", "disp-poll.csv"))

#Seleciona colunas do arquivo original que serão utilizadas
traits <- traits %>% select(3, 4, 5, 6)

#junta as tabelas feno e traits, organizando por espécie
master <- merge(x=feno, y=traits,
                              by="Species", all.x =T)

#Transforma NA=0
master[is.na(master)] <- 0

#Transforma células vazias em unknown
master$Dispersion[master$Dispersion == 0] <- "unknown"

#Transforma células vazias em unknown
master$Pollination[master$Pollination == 0] <- "unknown"

#Transforma células vazias em unknown
master$Deciduousness[master$Deciduousness == 0] <- "unknown"

class(master$DATE)

?as.Date

master$DATE <- as.Date(master$DATE, "%m/%d/%y")

summary(master)

as.tibble(master)

master$year = format(as.Date(master$DATE), "%Y")

master$month = format(as.Date(master$DATE), "%m")

master$days = lubridate::yday(master$DATE)

master$daysangles = (master$days*360)/365

colnames(master)[11] ="imfruit"

colnames(master)[12] ="mfruit"

class(master$imfruit)

fruit <- master %>%
  filter(imfruit == 1 | mfruit == 1)

feno.circ = circular(fruit$daysangles, units = "degrees", template = "none", modulo = "2pi")
fruit$feno.circ = circular(fruit$daysangles, units = "degrees", template = "none", modulo = "2pi")

plot(feno.circ, units = "radians",shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")

circular::rose.diag(feno.circ, bins=16, col = "darkgrey", cex = 0.8, prop =1.3, add = TRUE,
                    zero = pi/2, rotation = "clock")


watson.williams.test(feno.circ~Dispersion, data=fruit)
