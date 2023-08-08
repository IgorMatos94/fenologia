
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
                  "here",
                  "dplyr"
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
plot(data.circular, col="red") # disposição dos pontos em gráfico circular
#c média dos dados angulares
mean(data.circular)
#d coloca as colunas de intensidade por cada mês (rose diagram). Circular indica o pacote
circular::rose.diag(data.circular, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(data.circular)) #indica o ângulo médio da distribuição dos pontos

#2a = teste de Rayleigh = verifica a distribuição das observações pela circunferência. QUanto mais distante de 0, menos uniforme é a amostra.
# 0.9105 não há uniformidade pois resultado muito distante de 0
rayleigh.test(data.circular)
#2b
rho.circular(data.circular) #r=comprimento do angulo médio = 0.9105146
#2c
plot(data.circular, col="red") #demonstrando novamente os pontos
arrows.circular(mean(data.circular)) #angulo médio da distribuição dos pontos


#3
unburnt = c(213.8, 151.3, 195.4, 32.7, 307.7, 93.9, 187.8, 357.5,
            217.6, 307, 196.6, 233.6, 97) #concatenando os dados de floração em dias já convertidos em ângulos
unburntc = circular(unburnt, units = "degrees", template = "geographics", modulo = "2pi")
plot(unburntc, col = "blue") #dispondo os pontos em angulos
# coloca as colunas de intensidade por cada mês (rose diagram). Circular indica o pacote
circular::rose.diag(unburntc, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(unburntc)) #angulo médio da distribuição dos pontos

burnt = c(342.1, 64.6, 53.6, 0.4, 17.4, 350.8, 51.6, 157.6, 191,
          118.2, 317.9, 70.1, 55.8, 73.4)#concatenando os dados de floração em dias já convertidos em ângulos
burntc = circular(burnt, units = "degrees", template = "geographics", modulo = "2pi")
plot(burntc, col = "green")#dispondo os pontos em angulos

# coloca as colunas de intensidade por cada mês (rose diagram). Circular indica o pacote
circular::rose.diag(burntc, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
arrows.circular(mean(burntc))#angulo médio da distribuição dos pontos

#instalando pacote
install.packages(synchrony)
library(synchrony)

community.sync(feno.traits) #há sincronia intensa entre as espécies. Varia de -1 (fenofases opostas) até 1 (mesma fenofase). 0 indica ausencia de sincronia

#Ex 5 - trabalhando com o arquivo ceiba_botao
ceiba_botao$Year = format (as.Date(ceiba_botao$Day), "%Y") #separando o ano em uma nova coluna
ceiba_botao$Month = format (as.Date(ceiba_botao$Day), "%m")#separando o mes em uma nova coluna
ceiba_botao$days =  yday (ceiba_botao$Day) #separando o dia em uma nova coluna. Aqui são classificados em dias corridos 1-365
ceiba_botao #chamando tabela

#5b
ceiba_botao$daysangles = (ceiba_botao$days*360) /365 #transformando os dias corridos em angulos de 0-360
ceiba_botao

mean(ceiba_botao$daysangles) #calculando média da data angular
# disposição dos pontos em gráfico circular
dc.cbda = circular (ceiba_botao$daysangles, units = "degrees", template = "geographics", modulo = "2pi")
mean (dc.cbda) #média dos dados angulares
plot(dc.cbda, col = "blue") #dispondo os pontos em angulos
# coloca as colunas de intensidade por cada mês (rose diagram). Circular indica o pacote
circular::rose.diag(dc.cbda, bins=12, col="darkgrey", cex=0.8, prop = 1.3, add=TRUE, zero = pi/2, rotation = "clock")
arrows.circular(mean(dc.cbda))#angulo médio da distribuição dos pontos

?rose.diag
plot(dc.cbda, units = "degrees", shrink = 1.5, stack = TRUE, pch = 16, bins = 365, cex = 0.8, zero = pi/2, rotation ="clock")
#cria um gráfico que eu não entendi muito bem

#classifica os dados de acordo com os anos, criando uma nova lista com os filtros definidos
earlier = filter(ceiba_botao, Year == 2003| Year == 2004 | Year == 2005)
medium = filter(ceiba_botao, Year == 2008| Year==2009|Year==2010)
later = filter(ceiba_botao, Year == 2012| Year==2013|Year==2014)

watson.williams.test(ceibacirc~cat, data = ceiba_final)
