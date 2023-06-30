
#Hades
#14/04/23

# Fenologia 
# 13 de junho 2023
# Hades & Carina

#1 LOAD PACKAGES----------------------------------------------------------------

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
                  "here"
)

#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}


##LOAD DATA###

load(here("modulos_pri", "dados", "Aula3 (1).RData"))


##EXERCICIOS####

#1 a
mean(fruitdays) #calculando média dos frutos
plot(fruitdays) #gráfico linear dos dados
#b conversão de dados em angulos
data.circular = circular(fruitdays, units = "degrees", template = "geographics", modulo = "2pi")
plot(data.circular, col="red")
#c média dos dados angulares
mean(data.circular)
#d
circular::rose.diag(data.circular, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(data.circular))

#2a = 0.9105 não há uniformidade pois resultado muito distante de 0
rayleigh.test(data.circular)
#2b
rho.circular(data.circular) #r=comprimento do angulo médio
#2c
plot(data.circular, col="red")
arrows.circular(mean(data.circular))


#3
unburnt = c(213.8, 151.3, 195.4, 32.7, 307.7, 93.9, 187.8, 357.5,
            217.6, 307, 196.6, 233.6, 97)
unburntc = circular(unburnt, units = "degrees", template = "geographics", modulo = "2pi")
plot(unburntc, col = "blue")
circular::rose.diag(unburntc, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(unburntc))

burnt = c(342.1, 64.6, 53.6, 0.4, 17.4, 350.8, 51.6, 157.6, 191,
          118.2, 317.9, 70.1, 55.8, 73.4)
burntc = circular(burnt, units = "degrees", template = "geographics", modulo = "2pi")
plot(burntc, col = "green")
circular::rose.diag(burntc, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(burntc))

install.packages(synchrony)
library(synchrony)

community.sync(feno.traits) #há sincronia intensa entre as espécies. Varia de -1 (fenofases opostas) até 1 (mesma fenofase). 0 indica ausencia de sincronia

git

#Ex 5
ceiba_botao = 