# 11 de dezembro de 2024

# Hades I. M. Matos, Carina I. Motta

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
                  "ggtext",
                  "extrafont",
                  "fastDummies",
                  "circular",
                  'CircStats',
                  "phenocamr",
                  "lubridate",
                  "bpnreg",
                  "corrplot",
                  "RColorBrewer",
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
colnames(master)[7] = "Leaffall"
colnames(master)[8] = "Sprouting"
colnames(master)[9] = "BU"
colnames(master)[10] = "FL"
colnames(master)[11] ="imfruit"
colnames(master)[12] ="mfruit"
colnames(master)[19] = "Sempreverde"
colnames(master)[20] = "Semidecidua"
colnames(master)[21] = "Decidua"

class(master$imfruit)

fruit <- master %>%
  filter(imfruit == 1 | mfruit == 1)

#5B PLOT -  fruits on all the plots??
feno.circ = circular(fruit$daysangles, units = "degrees", template = "none", 
                     modulo = "2pi")
fruit$feno.circ = circular(fruit$daysangles, units = "degrees", 
                           template = "none", modulo = "2pi")

plot(feno.circ, units = "radians", shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")

circular::rose.diag(feno.circ, bins=16, col = "#F19E14", cex = 0.0, 
                    prop =1.3, add = TRUE,
                    zero = pi/2, rotation = "clock")


#5C STATISTICAL TEST BETWEEN GROUPS
#watson.williams.test(feno.circ~Dispersion, data=fruit)

#zoo

# Filtrar os dados para Zoocoria igual a 1
zoo <- fruit %>% filter(Zoocoria == 1)

# Converter os ângulos em dados circulares
feno.circ.zoo <- circular(zoo$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.zoo, units = "radians", axes = FALSE, shrink = 1, stack = TRUE, pch = 16, bins = 365, cex = 0, rotation = "clock", zero = pi/2)

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.zoo, axes = FALSE, bins = 12, col = "#F19E14", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho aumentado
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.zoo), rho.circular(feno.circ.zoo), zero = pi/2, rotation = "clock", col = "black")


# Teste de Rayleigh para avaliar a uniformidade da distribuição angular
rayleigh.test(feno.circ.zoo)

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.zoo)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.zoo)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.zoo)))

# Calcular a variância dos ângulos circulares
var.circular(feno.circ.zoo)



####################ANEMOCORIA################
# Filtrar os dados para Anemocoria igual a 1
anemo <- fruit %>%
  filter(Anemocoria == 1)

# Converter os ângulos em dados circulares
feno.circ.anemo <- circular(anemo$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.anemo, units = "radians", axes = FALSE, shrink = 1, stack = TRUE, pch = 20, bins = 365, cex = 0, rotation = "clock", zero = pi/2)

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.anemo, axes = FALSE, bins = 12, col = "#5B7C91", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho aumentado
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.anemo), rho.circular(feno.circ.anemo), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.anemo)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.anemo)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.anemo)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.anemo)



##################AUTOCORIA#################################
# Filtrar os dados para Autocoria igual a 1
auto <- fruit %>%
  filter(Autocoria == 1)

# Filtrar os dados para Autocoria igual a 1
auto <- fruit %>%
  filter(Autocoria == 1)

# Converter os ângulos em dados circulares
feno.circ.auto <- circular(auto$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.auto, units = "radians", axes = FALSE, shrink = 1, stack = TRUE, pch = 16, bins = 365, cex = 0, rotation = "clock", zero = pi/2)

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.auto, axes = FALSE, bins = 12, col = "#9F4147", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.auto), rho.circular(feno.circ.auto), zero = pi/2, rotation = "clock", col = "black") 

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.auto)

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.auto)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.auto)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.auto)))


#####################################FLORES#####################
###############################################################


# Verificar a classe da coluna BU
class(master$BU)

# Filtrar os dados para BU igual a 1 ou FL igual a 1
flower <- master %>%
  filter(BU == 1 | FL == 1)



##################ZOOFILIA################
# Filtrar os dados para Zoofilia igual a 1
zoofilia <- flower %>%
  filter(Zoofilia == 1)

# Converter os ângulos em dados circulares
feno.circ.zoofilia <- circular(zoofilia$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.zoofilia, units = "radians", axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.zoofilia, axes = FALSE, bins = 12, col = "#E86652", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.zoofilia), rho.circular(feno.circ.zoofilia), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.zoofilia)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.zoofilia)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.zoofilia)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.zoofilia)

# Calcular a variância dos ângulos circulares
var.circular(feno.circ.zoofilia)


#############################ANEMOFILIA##########################
# Filtrar os dados para Anemofilia igual a 1
anemofilia <- flower %>%
  filter(Anemofilia == 1)

# Converter os ângulos em dados circulares
feno.circ.anemofilia <- circular(anemofilia$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.anemofilia, units = "radians", axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.anemofilia, axes = FALSE, bins = 12, col = "#F8AF77", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.anemofilia), rho.circular(feno.circ.anemofilia), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.anemofilia)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.anemofilia)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.anemofilia)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.anemofilia)

# O teste de Watson-Williams compara duas amostras circulares
watson.williams.test(list(rad(feno.circ.zoofilia), rad(feno.circ.anemofilia)))

##################################################
####################LEAFFALL######################
###################################################


# Verificar a classe da coluna Leaffall
class(master$Leaffall)

# Filtrar os dados para Leaffall igual a 1
leaffall <- master %>%
  filter(Leaffall == 1)

# Filtrar os dados para Sempreverde igual a 1
sempreverde <- leaffall %>%
  filter(Sempreverde == 1)

# Converter os ângulos em dados circulares
feno.circ.sempreverde <- circular(sempreverde$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.sempreverde, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.sempreverde, axes = FALSE, bins = 12, col = "#C3E747", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.sempreverde), rho.circular(feno.circ.sempreverde), zero = pi/2, rotation = "clock")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.sempreverde)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.sempreverde)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.sempreverde)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.sempreverde)


###########################SEMIDECIDUA###############################
# Filtrar os dados para Semidecidua igual a 1
semidecidua <- leaffall %>%
  filter(Semidecidua == 1)

# Converter os ângulos em dados circulares
feno.circ.semidecidua <- circular(semidecidua$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.semidecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.semidecidua, axes = FALSE, bins = 12, col = "#71B481", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.semidecidua), rho.circular(feno.circ.semidecidua), zero = pi/2, rotation = "clock", col = "black")


# Calcular a média dos ângulos circulares
mean.circular(feno.circ.semidecidua)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.semidecidua)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.semidecidua)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.semidecidua)

###########################DECIDUA###################################
# Filtrar os dados para Decidua igual a 1
decidua <- leaffall %>%
  filter(Decidua == 1)

# Converter os ângulos em dados circulares
feno.circ.decidua <- circular(decidua$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.decidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.decidua, axes = FALSE, bins = 12, col = "#75774E", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.decidua), rho.circular(feno.circ.decidua), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.decidua)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.decidua)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.decidua)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.decidua)

# O teste de Watson-Williams compara três amostras circulares
watson.williams.test(list(rad(feno.circ.sempreverde), rad(feno.circ.semidecidua), rad(feno.circ.decidua)))


###########################################################################
########################BROTAMENTO#########################################
############################################################################
# Filtrar os dados para Sprouting igual a 1
sprouting <- master %>%
  filter(Sprouting == 1)

# Filtrar os dados para Sprouting e Sempreverde igual a 1
ssempreverde <- sprouting %>%
  filter(Sempreverde == 1)

# Converter os ângulos em dados circulares
feno.circ.ssempreverde <- circular(ssempreverde$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.ssempreverde, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.ssempreverde, axes = FALSE, bins = 12, col = "#C3E747", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.ssempreverde), rho.circular(feno.circ.sempreverde), zero = pi/2, rotation = "clock", col = "black")

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.ssempreverde)

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.ssempreverde)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.ssempreverde)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.ssempreverde)))




###########################SEMIDECIDUA###################################
# Filtrar os dados para Sprouting e Semidecidua igual a 1
ssemidecidua <- sprouting %>%
  filter(Semidecidua == 1)

# Converter os ângulos em dados circulares
feno.circ.ssemidecidua <- circular(ssemidecidua$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.ssemidecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.ssemidecidua, axes = FALSE, bins = 12, col = "#71B481", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.ssemidecidua), rho.circular(feno.circ.semidecidua), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.ssemidecidua)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.ssemidecidua)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.ssemidecidua)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.ssemidecidua)

################################DECIDUA###############################
# Filtrar os dados para Sprouting e Decidua igual a 1
sdecidua <- sprouting %>%
  filter(Decidua == 1)

# Converter os ângulos em dados circulares
feno.circ.sdecidua <- circular(sdecidua$daysangles, units = "degrees", template = "none", modulo = "2pi")

# Plotar o gráfico circular
plot(feno.circ.sdecidua, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")

# Plotar a rosa dos ventos
circular::rose.diag(feno.circ.sdecidua, axes = FALSE, bins = 12, col = "#75774E", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")

# Adicionar rótulos aos eixos com tamanho 2.0
axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
              c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
              cex = 2.0)

# Adicionar setas para indicar a direção do vento médio
arrows.circular(mean(feno.circ.sdecidua), rho.circular(feno.circ.decidua), zero = pi/2, rotation = "clock", col = "black")

# Calcular a média dos ângulos circulares
mean.circular(feno.circ.sdecidua)

# Calcular a magnitude média dos ângulos circulares
rho.circular(feno.circ.sdecidua)

# Calcular o desvio padrão dos ângulos circulares
sqrt(-2*log(rho.circular(feno.circ.sdecidua)))

# O teste de Rayleigh avalia se os dados têm distribuição uniforme
rayleigh.test(feno.circ.sdecidua)

# O teste de Watson-Williams compara três amostras circulares
watson.williams.test(list(rad(feno.circ.ssempreverde), rad(feno.circ.ssemidecidua), rad(feno.circ.sdecidua)))

