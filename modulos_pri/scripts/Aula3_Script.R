####---------------------- Aula 3 ------------------------ ##########
##################### -- Sets --  #####################
rm(list=ls())
setwd("SET HERE")
getwd()
setwd("/Users/priscila/Desktop/2022.1/Pheno.22/Aula_3")
#save(burnt, unburnt, fruitdays, ceiba_botao, ceiba_final,pratica,
#     feno.traits, feno.data3, InicioCeiba, file="Aula3.RData")
load('Aula3.RData')

#load(file.choose())
#read.csv(file.choose(), header = T)

##################### --Pacotes--  #####################
install.packages(c("activity","circular", "circStats",
                   "phenology", "lubridate","ggplot2"))
library(circular)
library(CircStats)
library(ggplot2)
library(phenology)
library(phenocamr)
library(lubridate)
library(bpnreg)

##################### --Ex. 1 #####################
# fruitdays = c(5, 6, 350, 351, 330, 333, 40, 47, 20, 35, 345, 360, 359, 352,
#              10, 7, 15, 13, 310, 320)
mean(fruitdays)
?circular
##################### --CONVERTENDO EM ÂNGULO/RADIANO --  #####################
data.circular = circular(fruitdays, units = "degrees", template = "geographics", modulo = "2pi")

#‘units = “degrees”’ ajusta a unidade em graus, e pode ser ajustada em radianos
#‘template = “geographics”’ ajusta o zero como o topo do gráfico, e a direção horária

#Calculando a media angular
mean(data.circular)

##################### --GRÁFICOS CIRCULARES--  #####################
#Quando representamos dados angulares de forma linear, nao representamos a 
#periodicidade dos dados
str(fruitdays)
par(mar=c(1, 1, 1, 1))
plot(fruitdays)
plot(data.circular)
plot(data.circular, col="blue")
days = c(30, 365, 40, 180)
angulos = 360 * days /365
angulos
#Adicionando setas e barras no gráfico circular
arrows.circular(data.circular, col = "red")
circular::rose.diag(data.circular, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)

circular::rose.diag(data.circular, bins=12, col="darkgrey", cex=1.5, prop = 1.3, add=TRUE)

# Repeat the plot as before, but green lines instead of red arrows
plot(data.circular, col = "red")
arrows.circular(mean(data.circular))

# Testando a uniformidade 
?rayleigh.test
rayleigh.test(data.circular)

##################### --r - comprimento --  #####################
?rho.circular
rho.circular(data.circular)
??rho.
## Só apresentar o comprimento médio se houver pico ###
??circ.disp
circ.disp(data.circular)

##################### --Ex. 2: Uniformidade ou Unimodalidade #####################
### Criando dados para o exercicio e convertendo em ângulos: 
unburnt = c(213.8, 151.3, 195.4, 32.7, 307.7, 93.9, 187.8, 357.5, 217.6, 307, 196.6, 233.6, 97)
unburntc = circular(unburnt, units = "degrees", template = "geographics", modulo = "2pi")

#burnt = c(342.1, 64.6, 53.6, 0.4, 17.4, 350.8, 51.6, 157.6, 191, 118.2, 317.9, 70.1, 55.8, 73.4)
burntc = circular(burnt, units = "degrees", template = "geographics", modulo = "2pi")

# Plot Burnt group (black)
plot.circular(burntc)
##################### --Rayleigh Test--  #####################
rayleigh.test(burntc)
rho.circular(burntc)
## Só apresentar o r comprimento médio se houver pico ###
arrows.circular(mean(burntc))

# Plot Unburnt group (red)
plot.circular(unburntc, col = "red")
rayleigh.test(unburntc)
rho.circular(unburntc)
arrows.circular(mean(unburntc), col = "red")

##################### --Ex. 3: WATSON test - comparando dois grupos (teste t)--  #####################
watson.two.test(burntc, unburntc)
?watson.two.test

# OPCIONAL:  Plot dois grupos juntos
plot.circular(burntc)
arrows.circular(mean(burntc))
points(unburntc, col = "red")
arrows.circular(mean(unburntc), col = "red")
circular::rose.diag(burntc, bins=12, col="darkgrey", cex=1, prop = 1.5, add=TRUE)
lines(density.circular(burntc,bw = 40))

##################### --Ex. 4 Sincronia:   #####################
install.packages("synchrony")
library(synchrony)
?community.sync
community.sync(feno.traits, nrands=100)

######### Ex 5. Manipulando a planilha #########
library(dplyr) 
feno.data=c(pratica$Ind1, pratica$Ind2, pratica$Ind3, pratica$Ind4, pratica$Ind5, pratica$Ind6)

pratica$Year = format(as.Date(pratica$Date), "%Y")

pratica$Month = format(as.Date(pratica$Date), "%m")
pratica$days = lubridate::day(pratica$Date)

bla = rbind( pratica[,c(8:10)],pratica[,c(8:10)],pratica[,c(8:10)],
             pratica[,c(8:10)],pratica[,c(8:10)],pratica[,c(8:10)])
feno.data2 = cbind(bla, feno.data)

feno.data3 = dplyr::filter(feno.data2, feno.data != 0)

feno.data3 = na.omit(feno.data3)
feno.data3$feno.data==0

feno.data3$daysangles = (feno.data3$days*360)/365

feno.circ = circular(feno.data3$daysangles, units = "degrees", template = "none", modulo = "2pi")
feno.data3$feno.circ = circular(feno.data3$daysangles, units = "degrees", template = "none", modulo = "2pi")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(feno.circ, units = "radians",shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")

circular::rose.diag(feno.circ, bins=16, col = "darkgrey", cex = 0.8, prop =1.3, add = TRUE,
          zero = pi/2, rotation = "clock")

############## Ex. 6 Pratica com dados enviados ####### 

## Extraindo variáveis a partir das datas
ceiba_botao$Year = format(as.Date(ceiba_botao$Day), "%Y")
ceiba_botao$Month = format(as.Date(ceiba_botao$Day), "%m")
ceiba_botao$days = lubridate::day(ceiba_botao$Day)
## Verifique sua planilha final!
ceiba_botao

#convertendo os dias em angulos:
days = lubridate::day(ceiba_botao$Day)
ceiba_botao$daysangles = (ceiba_botao$days*360)/365

ceibacirc = circular(ceiba_botao$daysangles, units = "degrees", template = "none", modulo = "2pi")
ceiba_botao$ceibacirc = circular(ceiba_botao$daysangles, units = "degrees", template = "none", modulo = "2pi")
#par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(ceibacirc, units = "degrees",shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")

circular::rose.diag(ceibacirc, bins=16, col = "darkgrey", cex = 0.8, prop =1.3, add = TRUE,
          zero = pi/2, rotation = "clock")

##################### --Graphs - ggplot2--  #####################
bla = ggplot(ceiba_botao, aes(x = ceibacirc, fill = as.factor(FenoInt))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bla

ble = ggplot(ceiba_botao, aes(x = ceibacirc, fill = as.factor(Year))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar()+ 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))
ble

#### Criando dois grupos a partir dos dados ###
earlier =dplyr:: filter(ceiba_botao, Year == 2003| Year==2004|Year==2005)
later = dplyr:: filter(ceiba_botao, Year == 2012| Year==2013|Year==2014)

earcirc = circular(earlier$daysangles, units = "degrees", template = "none", modulo = "2pi")
latcirc = circular(later$daysangles, units = "degrees", template = "none", modulo = "2pi")

plot(earcirc, units = "degrees",shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")
points(latcirc, col = "red",
       stack = TRUE, zero = pi/2, rotation = "clock")
# OPCIONAL:  Plot dois grupos juntos
arrows.circular(mean(earcirc),zero = pi/2, rotation = "clock")
points(latcirc, col = "red",zero = pi/2, rotation = "clock")
arrows.circular(mean(latcirc), col = "red",,zero = pi/2, rotation = "clock")

#Comparando os dois grupos
watson.two.test(earcirc, latcirc)

##################### -- comparando três grupos --  #####################
#Criando o terceiro grupo
medium = dplyr::filter(ceiba_botao, Year == 2008| Year==2009|Year==2010)
medcirc = circular(medium$daysangles, units = "radians", template = "none", modulo = "2pi")

#ceiba_botao$cat = c(rep("Early", 235), rep("Med", 250), rep("Late", 222))
watson.williams.test(ceibacirc~cat, data=ceiba_final)

##################### --Circular Data --  #####################
ceiba_iniflo = circular(InicioCeiba$Floração, units="degrees", template = "clock12",
                        modulo = "2pi")

ceiba_frut = circular(InicioCeiba$Frutificação, units="degrees", template = "clock12",
                      modulo = "2pi")

plot(ceiba_frut, units = "degrees",shrink = 1.5, stack = TRUE, pch = 16, 
     bins = 365, cex = 0.8, zero = pi/2, rotation = "clock")
points(ceiba_iniflo, col = "red",
       stack = TRUE, zero = pi/2, rotation = "clock")
# OPCIONAL:  Plot dois grupos juntos
arrows.circular(mean(ceiba_frut),zero = pi/2, rotation = "clock")
points(ceiba_iniflo, col = "red",zero = pi/2, rotation = "clock")
arrows.circular(mean(ceiba_iniflo), col = "red",zero = pi/2, rotation = "clock")

##################### --Ex. 7 Correlação Circ-Lin --  #####################
#Funcao de Mardia
cor.c.l <- function(linear, ang){
  sina <- sin(rad(ang))
  cosa <- cos(rad(ang))
  soma.linear <- sum(linear)
  soma.linear2 <- sum(linear^2)
  soma.sin <- sum(sina)
  soma.sin2 <- sum(sina^2)
  soma.cos <- sum(cosa)
  soma.cos2 <- sum(cosa^2)
  
  ssX <- soma.linear2-soma.linear^2/length(linear)
  sssina <- soma.sin2-soma.sin^2/length(linear)
  sscos <- soma.cos2-soma.cos^2/length(linear)
  
  sxp.cos <- sum(linear*cosa)-((soma.linear*soma.cos)/length(linear))
  sxp.sin <- sum(linear*sina)-((soma.linear*soma.sin)/length(linear))
  sxp.sin.cos <- sum(cosa*sina)-((soma.cos*soma.sin)/length(linear))
  rxc <- cor(linear, cosa)
  rxs <- cor(linear, sina)
  rcs <- cor(cosa, sina)
  ral <- sqrt((rxc^2 + rxs^2 - 2*rxc*rxs*rcs)/(1-rcs^2))
  nral2 <- length(ang)*((rxc^2 + rxs^2 - 2*rxc*rxs*rcs)/(1-rcs^2))
  ral
}

cor.c.l(InicioCeiba$Tmax, ceiba_iniflo)


R2xtCorrCoeff <- function(lvar, cvar) {
  rxc <- cor(lvar, cos(cvar)) ; rxs <- cor(lvar, sin(cvar))
  rcs <- cor(cos(cvar), sin(cvar))
  R2xtVal <- ((rxc*rxc)+(rxs*rxs)-(2*rxc*rxs*rcs))/(1-rcs*rcs)
  return(R2xtVal)
}

R2xtIndTestRand <- function(lvar, cvar, NR) {
  R2xtObs <- R2xtCorrCoeff(lvar, cvar) ; nxtrm <- 1
  for (r in 1:NR) {
    lvarRand <- sample(lvar)
    R2xtRand <- R2xtCorrCoeff(lvarRand,cvar)
    if (R2xtRand >= R2xtObs) {nxtrm <- nxtrm+1} }
  pval <- nxtrm/(NR+1) ; return(c(R2xtObs, pval))
}

daysrad = ceiba_final$ceibacirc*2*pi/360
temp = c(rep(26,100),rep(27,100),rep(28,100),rep(29,100),
         rep(30,100),rep(31,100), rep(32,107))

R2xtCorrCoeff(daysrad, temp)
R2xtIndTestRand(daysrad, temp, 999)

##################### --Regressão Circular --  #####################
x <- cbind(rnorm(10), rep(1, 10))
y <- circular(2*atan(c(x%*%c(5,1))))+rvonmises(10, mu=circular(0), kappa=100)
lm.circular(y=y, x=x, init=c(5,1), type='c-l', verbose=TRUE)
?lm.circular

InicioCeiba$Floraçãocirc = circular (InicioCeiba$Floração)

# Ajuste um moodelo de regressão circular-circular
?lm.circular
circ.lm <- lm.circular(ceiba_frut, ceiba_iniflo, order=1)
circ.lm
print(circ.lm, digits = max(3, getOption("digits") - 3), 
      signif.stars= getOption("show.signif.stars"))

plot.default(ceiba_iniflo, ceiba_frut)

##################### --Outros Exemplos --  #####################
## Outros exemplos
#Representação circular (valores em ângulos, explorar #3
plot(fisherB1c, shrink = 1.2, stack = TRUE, pch = 16, bins = 720, cex = 1)
lines(density.circular(fisherB1c, bw=40), lwd=2)
circular::rose.diag(fisherB1c, bins=24, cex=1, prop=2.3, col="grey", add=TRUE)
rtest = rayleigh.test(fisherB1c) 
rtest$statistic
rtest$p

#Se quiser comparar se há uniformidade na distribuicao dos angulos
# vs uma alternativa 
kuiper.test(fisherB1c) 

# Ajuste um moodelo de regressão circular-circular
circ.lm <- lm.circular(ceiba_iniflo, ceiba_frut, order=1)
circ.lm
print(circ.lm, digits = max(3, getOption("digits") - 3), 
      signif.stars= getOption("show.signif.stars"))

plot.default(ceiba_frut, ceiba_iniflo)

flo_rad = circular(ceiba_iniflo, units = "radians", template = "none", modulo = "2pi")
frut_rad = circular(ceiba_frut, units = "radians", template = "none", modulo = "2pi")

library(bpnreg)
bla = as.data.frame(cbind(flo_rad, frut_rad))
bla = na.omit(bla)
mod = bpnr(frut_rad ~ flo_rad, data=bla, its = 100)
print(mod)
mod

mod2 = bpnme(frut_rad ~ Site + (1|ID) , Dados, its = 100)
print(mod2)


