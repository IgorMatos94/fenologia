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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_zooc.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
  # Plotar o gráfico circular
  plot(feno.circ.zoo, units = "radians", axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")
  
  # Plotar a rosa dos ventos
  circular::rose.diag(feno.circ.zoo, axes = FALSE, bins = 12, col = "#F19E14", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")
  
  # Adicionar rótulos aos eixos com tamanho aumentado
  axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
                c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
                cex = 2.0)
  
  # Adicionar setas para indicar a direção do vento médio
  arrows.circular(mean(feno.circ.zoo), rho.circular(feno.circ.zoo), zero = pi/2, rotation = "clock", col = "black")
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_anemoc.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
  # Plotar o gráfico circular
  plot(feno.circ.anemo, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")
  
  # Plotar a rosa dos ventos
  circular::rose.diag(feno.circ.anemo, axes = FALSE, bins = 12, col = "#5B7C91", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")
  
  # Adicionar rótulos aos eixos com tamanho aumentado
  axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
                c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
                cex = 2.0)
  
  # Adicionar setas para indicar a direção do vento médio
  arrows.circular(mean(feno.circ.anemo), rho.circular(feno.circ.anemo), zero = pi/2, rotation = "clock", col = "black")
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_autoc.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
  # Plotar o gráfico circular
  plot(feno.circ.auto, axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")
  
  # Plotar a rosa dos ventos
  circular::rose.diag(feno.circ.auto, axes = FALSE, bins = 12, col = "#9F4147", cex = 0.0, prop =1.3, add = TRUE, zero = pi/2, rotation = "clock")
  
  # Adicionar rótulos aos eixos com tamanho 2.0
  axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
                c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
                cex = 2.0)
  
  # Adicionar setas para indicar a direção do vento médio
  arrows.circular(mean(feno.circ.auto), rho.circular(feno.circ.auto), zero = pi/2, rotation = "clock", col = "black") 
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  # Renomear colunas
  colnames(master)[9] = "BU"
  colnames(master)[10] = "FL"
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_zoofilia.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
  # Plotar o gráfico circular
  plot(feno.circ.zoofilia, units = "radians", axes = FALSE, shrink = , stack = TRUE, pch = 16, bins = 365, cex = 0.0, rotation = "clock")
  
  # Plotar a rosa dos ventos
  circular::rose.diag(feno.circ.zoofilia, axes = FALSE, bins = 12, col = "#E86652", cex = 0.0, prop = 1.3, add = TRUE, zero = pi/2, rotation = "clock")
  
  # Adicionar rótulos aos eixos com tamanho 2.0
  axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
                c(labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A")), 
                cex = 2.0)
  
  # Adicionar setas para indicar a direção do vento médio
  arrows.circular(mean(feno.circ.zoofilia), rho.circular(feno.circ.zoofilia), zero = pi/2, rotation = "clock", col = "black")
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_anemofilia.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
    # Renomear colunas
  colnames(master)[7] = "Leaffall"
  colnames(master)[8] = "Sprouting"
  colnames(master)[19] = "Sempreverde"
  colnames(master)[20] = "Semidecidua"
  colnames(master)[21] = "Decidua"
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_sempreverde.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_semidecidua.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_decidua.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_ssempreverde.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_ssemidecidua.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  # Salvar o gráfico em um arquivo JPEG com alta resolução
  file_name <- "saz_sdecidua.jpg"
  jpeg(file_name, width = 1200, height = 800, quality = 100)
  
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
  
  # Finalizar o dispositivo gráfico JPEG
  dev.off()
  
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
  
  
  
  
  #########################################################################
  ####################duração#############################################
  ##################################
  
  ###########FRUITS#########################
  
  fruit_ <- fruit %>%
    filter(Zoocoria == 1 | Anemocoria == 1 | Autocoria == 1)
  
  fruit_ <- fruit_%>% select(1, 5, 23)
  
  fruit_new <- fruit_ %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  gg_duracao_frutos <- ggplot(duracao_frutos, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Dispersão") +
    scale_color_manual(values = c("1" = "#F19E14", "2" = "#5B7C91", "3" = "#9F4147", "4" = "#EF8F81", "5" = "#71B481"),
                       labels = c("Anemocoria", "Anemo+Auto", "Autocoria", "Zoo+Auto", "Zoocoria")) +  
    scale_x_continuous(breaks = seq(min(duracao$Month) - (min(duracao$Month) %% 2),
                                    max(duracao$Month) + 2 - (max(duracao$Month) %% 2), by = 2)) +
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave("duracao_frutos.jpg", plot = gg_duracao_frutos, width = 12, height = 8, dpi = 300)
  
  ####################FLOWERS##############
  flower_ <- flower %>%
    filter(Anemofilia == 1 | Zoofilia == 1)
  
  flower_ <- flower_%>% select(1, 5, 23)
  
  flower_new <- flower_ %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  gg_duracao_flores <- ggplot(duracao_flores, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Polinização") +
    scale_color_manual(values = c("1" = "#F19E14", "2" = "#9F4147"),
                       labels = c("Anemofilia+Zoo", "Zoofilia")) +  
    scale_x_continuous(breaks = seq(min(duracao$Month) - (min(duracao$Month) %% 2),
                                    max(duracao$Month) + 2 - (max(duracao$Month) %% 2), by = 2)) +
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave("duracao_flores.jpg", plot = gg_duracao_flores, width = 12, height = 8, dpi = 300)
  
  ####################LEAFFALL##############
  
  leaffall_ <- leaffall %>%
    filter(Decidua == 1 | Semidecidua == 1 | Sempreverde == 1)
  
  leaffall_ <- leaffall_%>% select(1, 5, 23)
  
  leaffall_new <- leaffall_ %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  gg_duracao_queda_foliar1 <- ggplot(duracao_queda_foliar1, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Deciduidade") +
    theme(axis.text.y = element_text(size = 5)) +
    scale_color_manual(values = c("1" = "#9F4147", "2" = "#F19E14"),
                       labels = c("Decídua", "Semi-decídua")) +  
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave("duracao_queda_foliar1.jpg", plot = gg_duracao_queda_foliar1, width = 12, height = 8, dpi = 300)
  
  # Seu código para criar o gráfico
  gg_duracao_queda_foliar2 <- ggplot(duracao_queda_foliar2, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Deciduidade") +
    theme(axis.text.y = element_text(size = 5)) +
    scale_color_manual(values = c("3" = "#71B481", "4" = "#5B7C91"),
                       labels = c("Perenifólia+\nSemi-decídua", "Perenifólia")) +  
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Salvar o gráfico em um arquivo JPEG
  ggsave("duracao_queda_foliar2.jpg", plot = gg_duracao_queda_foliar2, width = 12, height = 8, dpi = 300)
  
  ####################SPROUTING##############
  
  sprouting_ <- sprouting %>%
    filter(Decidua == 1 | Semidecidua == 1 | Sempreverde == 1)
  
  sprouting_ <- sprouting_%>% select(1, 5, 23)
  
  sprouting_new <- sprouting_ %>%
    group_by(Species, month) %>%
    summarise(ind = n_distinct(Tag))
  
  # Seu código para criar o gráfico
  gg_duracao_brotamento1 <- ggplot(duracao_brotamento1, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Deciduidade") +
    theme(axis.text.y = element_text(size = 5)) +
    scale_color_manual(values = c("1" = "#9F4147", "2" = "#F19E14"),
                       labels = c("Decídua", "Semi-decídua")) +  
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Salvar o gráfico em um arquivo JPEG
  ggsave("duracao_brotamento1.jpg", plot = gg_duracao_brotamento1, width = 12, height = 8, dpi = 300)
  
  gg_duracao_brotamento2 <- ggplot(duracao_brotamento2, aes(x = IN, y = Species, xend = FI, yend = reorder(Species, Dispersion), group = Dispersion, color = factor(Dispersion))) +
    geom_segment(aes(group = Species)) +
    geom_point(aes(x = IN, y = Species, color = reorder(Species, Dispersion))) +  # Adiciona pontos em cada valor de dados
    geom_point(aes(x = FI, y = Species, color = reorder(Species, Dispersion))) +
    labs(x = "Meses", y = "Espécies", color = "Deciduidade") +
    theme(axis.text.y = element_text(size = 5)) +
    scale_color_manual(values = c("3" = "#71B481", "4" = "#5B7C91"),
                       labels = c("Perenifólia+\nSemi-decídua", "Perenifólia")) +  
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6), 
                       labels = c("J", "A", "S\n                         2022", "O", "N", "D", "J", "F", "M\n                         2023", "A", "M", "J")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Salvar o gráfico em um arquivo JPEG
  ggsave("duracao_brotamento2.jpg", plot = gg_duracao_brotamento2, width = 12, height = 8, dpi = 300)
  
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_zooc_df)
  
  # Definir novos títulos para as colunas
  novos_titulos1 <- c("Alchornea glandulosa", "Casearia sylvestris",
                     "Chrysophyllum marginatum", "Copaifera langsdorffii",
                     "Eugenia florida", "Guarea macrophylla", "Guazuma ulmifolia",
                     "Lafoensia pacari", "Lithraea molleoides",
                     "Syagrus romanzoffiana", "Trichilia clausseni",
                     "Trichilia pallida", "Zanthoxylum monogynum",
                     "Zanthoxylum riedelianum")
  
  colnames(matrix_zooc_df) <- novos_titulos1
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_zooc_df, nrands = 100)
  
  head(matrix_zooc_df)
  
  M<-cor(matrix_zooc_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_zooc_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvando o gráfico como JPEG
  jpeg("sinc_zooc.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  sinc_zooc <- corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)

 dev.off()
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_anemoc_df)
  
  # Definir novos títulos para as colunas
  novos_titulos2 <- c("Centrolobium tomentosum",  "Eucalyptus spp.",
                      "Lafoensia pacari", "Luehea candicans",
                      "Machaerium brasiliense", "Machaerium hirtum",
                      "Machaerium stipitatum", "Machaerium villosum",
                      "Moquiniastrum polymorphum", "Myracrodruon urundeuva",
                      "Platypodium elegans")
  
  colnames(matrix_anemoc_df) <- novos_titulos2
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_anemoc_df, nrands = 100)
  
  head(matrix_anemoc_df)
  
  M<-cor(matrix_anemoc_df)
  head(round(M,2))
  
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_anemoc_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvando o gráfico como JPEG
  jpeg("sinc_anemoc.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  
  dev.off()
  
    
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_autoc_df)
  
  # Definir novos títulos para as colunas
  novos_titulos3 <- c("Alchornea glandulosa", "Croton floribundus", "Guarea macrophylla",
                      "Lafoensia pacari", "Luehea candicans", "Machaerium stipitatum",
                      "Sebastiania brasiliensis")
  colnames(matrix_autoc_df) <- novos_titulos3
  
    #run communitmatrix_n#run community sync test
  community.sync(matrix_autoc_df, nrands = 100)
  
  head(matrix_autoc_df)
  
  M<-cor(matrix_autoc_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_autoc_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_autoc.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  dev.off()
  

    
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_zoof_df)
  
  # Definir novos títulos para as colunas
  novos_titulos4 <- c("Albizia niopoides", "Alchornea glandulosa",
                      "Aloysia virgata", "Andira fraxinifolia",
                      "Aspidosperma cylindrocarpon",
                      "Cariniana estrellensis", "Casearia sylvestris",
                      "Centrolobium tomentosum",
                      "Chrysophyllum marginatum", "Citharexylum myrianthum",
                      "Croton floribundus", "Cupania vernalis",
                      "Dahlstedtia muehlbergiana",
                      "Enterolobium contortisiliquum", "Eucalyptus spp.",
                      "Eugenia florida", "Guarea macrophylla", "Guazuma ulmifolia",
                      "Lafoensia pacari", "Lithraea molleoides",
                      "Luehea candicans", "Luehea grandiflora",
                      "Machaerium hirtum", "Machaerium nyctitans",
                      "Machaerium villosum", "Moquiniastrum polymorphum",
                      "Myracrodruon urundeuva", "Myrcia tomentosa",
                      "Platypodium elegans", "Sebastiania brasiliensis",
                      "Syagrus romanzoffiana", "Tapirira guianensis",
                      "Trichilia clausseni", "Trichilia pallida",
                      "Zanthoxylum monogynum", "Zanthoxylum rhoifolium")
  colnames(matrix_zoof_df) <- novos_titulos4
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_zoof_df, nrands = 100)
  
  head(matrix_zoof_df)
  
  M<-cor(matrix_zoof_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_zoof_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_zoof.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.0)
  
  dev.off()
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_anemof_df)
  
  # Definir novos títulos para as colunas
  novos_titulos5 <- c("Sebastiania brasiliensis")
  colnames(matrix_anemof_df) <- novos_titulos5
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_anemof_df, nrands = 100)
  
  head(matrix_anemof_df)
  
  M<-cor(matrix_anemof_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_anemof_df)
  #NÃO HÁ RESULTADO POIS APENAS UMA ESPÉCIE ATENDENDE O REQUISITO DE 3+ IND
  
  
  
    
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
  
  #make the first column the row names
  rownames(matrix_sprouting_sempreverde_n) <- matrix_sprouting_sempreverde$month
  matrix_sprouting_sempreverde_df <- matrix_sprouting_sempreverde[, -1]
  
  # Exibir os títulos originais das colunas
  colnames(matrix_sprouting_sempreverde_df)
  
  # Definir novos títulos para as colunas
  novos_titulos6 <- c("Alchornea glandulosa", "Andira fraxinifolia",
                      "Casearia sylvestris", "Cupania vernalis",
                      "Diospyros inconstans", "Eugenia florida",
                      "Guarea macrophylla", "Guazuma ulmifolia",
                      "Lithraea molleoides", "Machaerium stipitatum",
                      "Machaerium villosum", "Moquiniastrum polymorphum",
                      "Ocotea puberula", "Syagrus romanzoffiana",
                      "Tapirira guianensis", "Zanthoxylum monogynum")
  colnames(matrix_sprouting_sempreverde_df) <- novos_titulos6
  
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_sempreverde_df, nrands = 100)
  
  # Calcule a matriz de correlação
  matriz_cor <- cor(matrix_sprouting_sempreverde_df)
  
  # Exiba a matriz de correlação
  print("Matriz de correlação:")
  print(matriz_cor)
  
  # Calcule a média dos valores de r
  media_r <- mean(matriz_cor[upper.tri(matriz_cor)])  # A função upper.tri() seleciona apenas a parte superior da matriz de correlação
  print(paste("Média do valor de r:", media_r))
  
  head(matrix_sprouting_sempreverde_df)
  
  M<-cor(matrix_sprouting_sempreverde_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_sprouting_sempreverde_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_ssempreverde.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  dev.off()
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_sprouting_semidecidua_df)
  
  # Definir novos títulos para as colunas
  novos_titulos7 <- c("Albizia niopoides", "Cariniana estrellensis",
                      "Chrysophyllum marginatum", "Copaifera langsdorffii",
                      "Croton floribundus", "Eugenia pyriformis",
                      "Lafoensia pacari", "Lonchocarpus cultratus",
                      "Luehea candicans", "Luehea grandiflora",
                      "Machaerium nyctitans", "Maclura tinctoria",
                      "Platypodium elegans", "Psidium guajava",
                      "Sebastiania brasiliensis", "Trichilia clausseni",
                      "Trichilia pallida", "Zanthoxylum rhoifolium")
  colnames(matrix_sprouting_semidecidua_df) <- novos_titulos7
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_semidecidua_df, nrands = 100)
  
  # Calcule a matriz de correlação
  matriz_cor <- cor(matrix_sprouting_semidecidua_df)
  
  # Exiba a matriz de correlação
  print("Matriz de correlação:")
  print(matriz_cor)
  
  # Calcule a média dos valores de r
  media_r <- mean(matriz_cor[upper.tri(matriz_cor)])  # A função upper.tri() seleciona apenas a parte superior da matriz de correlação
  print(paste("Média do valor de r:", media_r))
  
  head(matrix_sprouting_semidecidua_df)
  
  M<-cor(matrix_sprouting_semidecidua_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_sprouting_semidecidua_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_ssemidecidua.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  dev.off()
  
  
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_sprouting_decidua_df)
  
  # Definir novos títulos para as colunas
  novos_titulos8 <- c("Allophylus edulis", "Aloysia virgata",
                      "Aspidosperma cylindrocarpon", "Centrolobium tomentosum",
                      "Citharexylum myrianthum", "Dahlstedtia muehlbergiana",
                      "Enterolobium contortisiliquum", "Machaerium brasiliense",
                      "Machaerium hirtum", "Myracrodruon urundeuva",
                      "Myrcia tomentosa", "Zanthoxylum riedelianum")
  colnames(matrix_sprouting_decidua_df) <- novos_titulos8
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_sprouting_decidua_df, nrands = 100)
  
  
  head(matrix_sprouting_decidua_df)
  
  M<-cor(matrix_sprouting_decidua_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_sprouting_decidua_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_sdecidua.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  dev.off()
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_leaffall_sempreverde_df)
  
  # Definir novos títulos para as colunas
  novos_titulos9 <- c("Alchornea glandulosa", "Andira fraxinifolia",
                      "Casearia sylvestris", "Cupania vernalis",
                      "Diospyros inconstans", "Eugenia florida",
                      "Guarea macrophylla", "Guazuma ulmifolia",
                      "Lithraea molleoides", "Machaerium stipitatum",
                      "Machaerium villosum", "Moquiniastrum polymorphum",
                      "Ocotea puberula", "Syagrus romanzoffiana",
                      "Tapirira guianensis", "Zanthoxylum monogynum")
  colnames(matrix_leaffall_sempreverde_df) <- novos_titulos9
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_sempreverde_df, nrands = 100)
  
  
  head(matrix_leaffall_sempreverde_df)
  
  M<-cor(matrix_leaffall_sempreverde_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_leaffall_sempreverde_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_sempreverde.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  
  dev.off()
  
  
  
  
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
  matrix_leaffall_semidecidua_n <- matrix_leaffall_semidecidua[, -c()]
  
  
  #make the first column the row names
  rownames(matrix_leaffall_semidecidua) <- matrix_leaffall_semidecidua$month
  matrix_leaffall_semidecidua_df <- matrix_leaffall_semidecidua[, -1]
  
  # Exibir os títulos originais das colunas
  colnames(matrix_leaffall_semidecidua_df)
  
  # Definir novos títulos para as colunas
  novos_titulos10 <- c("Albizia niopoides", "Cariniana estrellensis",
                       "Chrysophyllum marginatum", "Copaifera langsdorffii",
                       "Croton floribundus", "Eugenia pyriformis",
                       "Lafoensia pacari", "Lonchocarpus cultratus",
                       "Luehea candicans", "Luehea grandiflora",
                       "Machaerium nyctitans", "Maclura tinctoria",
                       "Platypodium elegans", "Psidium guajava",
                       "Sebastiania brasiliensis", "Trichilia clausseni",
                       "Trichilia pallida", "Zanthoxylum rhoifolium")
  colnames(matrix_leaffall_semidecidua_df) <- novos_titulos10
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_semidecidua_df, nrands = 100)
  
  
  head(matrix_leaffall_semidecidua_df)
  
  M<-cor(matrix_leaffall_semidecidua_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_leaffall_semidecidua_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_semidecidua.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  dev.off()
  
  
  
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
  
  # Exibir os títulos originais das colunas
  colnames(matrix_leaffall_decidua_df)
  
  # Definir novos títulos para as colunas
  novos_titulos11 <- c("Allophylus edulis", "Aloysia virgata",
                       "Aspidosperma cylindrocarpon", "Centrolobium tomentosum",
                       "Citharexylum myrianthum", "Dahlstedtia muehlbergiana",
                       "Enterolobium contortisiliquum", "Machaerium brasiliense",
                       "Machaerium hirtum", "Myracrodruon urundeuva",
                       "Myrcia tomentosa", "Zanthoxylum riedelianum")
  colnames(matrix_leaffall_decidua_df) <- novos_titulos11
  
  #run communitmatrix_n#run community sync test
  community.sync(matrix_leaffall_decidua_df, nrands = 100)
  
  
  head(matrix_leaffall_decidua_df)
  
  M<-cor(matrix_leaffall_decidua_df)
  head(round(M,2))
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(matrix_leaffall_decidua_df)
  head(p.mat[, 1:5])
  
  {plot.new(); dev.off()}
  
  # Salvar o gráfico como JPEG
  jpeg("sinc_decidua.jpg", width = 1200, height = 800, quality = 100)
  
  # Leave blank on no significant coefficient
  corrplot(M, 
           type = "lower", 
           order = "hclust",  
           tl.col = "black",
           col = brewer.pal(n = 8, name = "RdYlGn"),
           p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "blank",
           tl.cex = 1.5)
  dev.off()
  
  
  citation()
  