# 13 de julho 2023

# Hades, Carina I. Motta

#Fenologia - analyses of phenology in naturally recovering secondary forest
#fragments in the Corumbataí River Basin, São Paulo based on dispersion and 
#pollination syndromes as well as deciduousness




# annotate each line of script, fix latin names bug 




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
feno <- readr::read_csv(here::here("dados", "feno.csv"))

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


