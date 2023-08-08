####---------------------- DATA ANALYSIS ------------------------ ##########
##################### --SETS--  #####################
rm(list=ls())
setwd("/Users/priscila/Desktop/2023/Amanda")
save(dados, clima2, clima,dados.flor, inicio, frutos, duracao_antese_0404_clima_polli_sem0, frutos_duracao_clima_poll_sem0, file = "AmandaDados.RData")
load("AmandaDados.RData")
load("dados_novos.RData")
load("dados_duracao.RData")

##################### --Packages--  #####################
require(ggplot2)
require(jtools)
require(sjPlot)
require(nlme) 
require(MASS) 
require(car) 
require(dplyr) 
require(tidyverse) 
require(ggpubr) 
require(gplots) 
require(emmeans) 
require(bbmle) 
require(lme4) 
require(lmerTest) 
require(circular) 
require(CircStats) 
require(lubridate) 
require(devtools) 
#require() 
#devtools::install_github("dustinfife/flexplot")
library(flexplot)
##################### --Data inspection--  #####################
flexplot(duration~1, data=duracao_antese_0404_clima_polli_sem0)
flexplot(duration~year, data=duracao_antese_0404_clima_polli_sem0, method = "poisson")
flexplot(duration~year, data=duracao_antese_0404_clima_polli_sem0, method = "poisson")
### Zero Inflated (Poisson) Distribution

##################### --Q1 - FLOWER Duration ~ spp/Year--  #####################
### Poisson
mod0 = glmer(duration ~ 1 + (1|ind),
               data = duracao_antese_0404_clima_polli_sem0, family = "poisson")
summary(mod0)

mod1 = glmer(duration ~ scale(year) + (1|ind),
               data = duracao_antese_0404_clima_polli_sem0, family = "poisson")
summary(mod1)

aggregate(duration~spp,duracao_antese_0404_clima_polli_sem0 , FUN="mean")
bla = aggregate(duration~spp, duracao_antese_0404_clima_polli_sem0, FUN="sd")
max(bla$duration)
?aggregate
mod2 = glm(duration ~ spp*year+(1|as.numeric(Local)),
               data = duracao_antese_0404_clima_polli_sem0, family = "poisson")
summary(mod2)

plot_model(mod1, type = "pred")

mod3 = glm(duration ~ spp*year +(1|ind),
           data = duracao_antese_0404_clima_polli_sem0, family = "poisson")
summary(mod3)

####graph = filter(duracao_antese_0404_clima_polli_sem0, spp == "Banisteriopsis campestris"|
                 spp == "Bauhinia rufa"|
                 spp == "Byrsonima intermedia"|
                 spp == "Schefflera vinosa"|
                 spp ==  "Tocoyena formosa")

sp2 = filter(duracao_antese_0404_clima_polli_sem0, spp == "Byrsonima intermedia")
sp3 = filter(duracao_antese_0404_clima_polli_sem0, spp == "Miconia rubiginosa")
sp4 = filter(duracao_antese_0404_clima_polli_sem0, spp == "Schefflera vinosa")
sp5 = filter(duracao_antese_0404_clima_polli_sem0, spp == "Siparuna guianensis")
sp6 = filter(dados, spp == "Qualea grandiflora")

modam = glmer(duration ~ scale(year) +(1|ind),
               data = sp1, family = "poisson")
summary(modam)
am = flexplot(duration~year, data=duracao_antese_0404_clima_polli_sem0, method = "poisson")

modbau = glmer(Duration ~ scale(Year) +(1|ind),
              data = sp2, family = "poisson")
summary(modbau)
flexplot(Duration~Year, data=sp2, method = "poisson")

modmic = glmer(Duration ~ scale(Year) +(1|ind),
               data = sp3, family = "poisson")
summary(modmic)
flexplot(Duration~Year, data=sp3, method = "poisson")

modsche = glmer(Duration ~ scale(Year) +(1|ind),
               data = sp4, family = "poisson")
summary(modsche)
flexplot(Duration~Year, data=sp4, method = "poisson")

modsip = lm(Duration ~ (Year),
                data = sp5)
summary(modsip)
sip = flexplot(Duration~Year, data=sp5, method = "poisson")

modqua = glmer(Duration ~ scale(Year) +(1|ind),
               data = sp6, family = "poisson")
###summary(modqua)
qua = flexplot(Duration~Year, data=sp6, method = "poisson")
library(ggpubr)
###ggarrange(am,qua)
### Positive effects for certain species
###ggarrange(bau,sche,sip)

library(ggplot2)
ama=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, 
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
#  xlim(100, 4000)
 theme_classic()+theme(text=element_text(size=16))
  ama
plot_coefs(ama)

resultado<-ICtab(mod0, mod1, mod2,mod3,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(duracao_antese_0404_clima_polli_sem0))
resultado

library(lmtest)
lrtest(modam)
library(MuMIn)
r.squaredGLMM(modam)

mod.sp = glmer(Duration ~ scale(Year) +(1|ind),
             data = sp2, family = "poisson")
summary(mod.sp)
flexplot(Duration~Year, data=sp2, method = "poisson")
aggregate(Index~spp, data=dados, FUN=mean)

ana=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, 
              method.args = list(family="poisson")) + 
  ylab("Duration of Flowering") +xlab("Years")+
  #  xlim(100, 4000)
  theme_classic()+theme(text=element_text(size=16))
ana
plot_coefs(mod.sp)

sp1$Duration[sp1$Duration >1 ] <- 1

mod4 = glmer(Duration ~ scale(Year) +(1|ind),
             data = sp1, family = "binomial")

summary(mod4)
resultado<-ICtab(mod0, mod1, mod2, mod3,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(dados))
resultado

library(lmtest)
lrtest(mod4)
library(MuMIn)
r.squaredGLMM(mod4)

##################### --Q2 - Duration ~ spp*Cat--  #####################
earlier = filter(duracao_antese_0404_clima_polli_sem0, year == 2005| year==2006|year==2007)
earlier$CatTime = c(rep("early", 965))
later = filter(duracao_antese_0404_clima_polli_sem0, year == 2017| year==2018| year==2019)
later$CatTime = c(rep("late", 736))
dados2_sem0 = rbind(earlier, later)

mod0 = glmer(duration ~ CatTime + (1|ind), data = dados2_sem0, family = poisson())
summ(mod0)
mod1 = glm(duration ~ CatTime+spp, data = dados2_sem0, family = poisson())
summ(mod1)
mod2 = glm(duration ~ spp/CatTime, data = dados2_sem0, family = poisson())
summ(mod2)
## Positive effects: Amaioua; Mic rubi;
## Strongest negative effects: Bauhinia; Byrsonima; Rourea; Siparuna;
## General negative effect (-0.27).
########## Graphs ##########
dados3_sem0 = filter(dados2_sem0, Duration>0)

library(viridis)
fig1=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration, fill = spp)) + 
 # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
 #   xlim(2005, 2019)+
  theme_classic()+theme(text=element_text(size=12), legend.text = element_text(face="italic"))
fig1+scale_color_viridis()+labs(fill = "Species")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

ylim.prim <- c(0, 0.8)  
ylim.sec <- c(20, 23) 

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

graph2 = cbind(duracao_antese_0404_clima_polli_sem0, clima2$temp)
colnames(graph2) = c("Index","Local","spp",
                     "ind","Duration","Year","temp")

fig2=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
  stat_smooth(aes(y = a + temp*b), method = "glm", se = TRUE, fullrange = TRUE,
             size = 0.5, color = "red") +
  scale_y_continuous("Duration of Flowering", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Temperature",
                                         labels = scales::number_format(accuracy = 0.1))) +
  scale_x_continuous(breaks = c(2005,2010,2015, 2019))+
  theme_classic()+theme(text=element_text(size=16),
                        axis.line.y.right = element_line(color = "red"), 
                        axis.ticks.y.right = element_line(color = "red"),
                        axis.text.y.right = element_text(color = "red"), 
                        axis.title.y.right = element_text(color = "red"))
fig2

###commcat = ggplot(duracao_antese_0404_clima_polli_sem0, aes(x = CatTime, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Community Flowering Duration") + xlab("")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none")
#  annotate("text", x = c(1), y = 8, label = "a", size=6)+
#  annotate("text", x = c(2,3,4), y = 8, label = "b", size=6)+
#  annotate("text", x = 0.85, y = 1, label = "C", size=5)+
#  annotate("text", x = 1.1, y = 1, label = "NC", size=5)+
#  scale_fill_manual(values=c("white","black","white", "white", "white", "black","white", "black"))
commcat

###fig1 = ggplot(dados, aes(x = CatTime, y =Duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Community Flowering Duration") + xlab("")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none")
#  annotate("text", x = c(1), y = 8, label = "a", size=6)+
#  annotate("text", x = c(2,3,4), y = 8, label = "b", size=6)+
#  annotate("text", x = 0.85, y = 1, label = "C", size=5)+
#  annotate("text", x = 1.1, y = 1, label = "NC", size=5)+
#  scale_fill_manual(values=c("white","black","white", "white", "white", "black","white", "black"))
commcat

##################### --Q3 - Duration ~ Clima--  #####################
##################### --Climatic Variables--  #####################
###nao consegui rodar essa parte### at�...
#clima2 = clima[rep(seq_len(nrow(clima)), each = 953), ]
dados3 = cbind(duracao_antese_0404_clima_polli_sem0, temp, precip, pluvi)

cresc.cor=clima[,c(7:9)]
cor.test(cresc.cor$prec,cresc.cor$plu)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=2)
}
pairs(cresc.cor, upper.panel = panel.cor)
chart.Correlation(cresc.cor, histogram=TRUE, pch="+")
########....aqui -  n�o consegui rodar


##################### --Models--  #####################
mod6 = glm(duration ~ temp + (1|ind)+(1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod6)

mod7 = glm(duration ~ temp + pluvi + (1|ind)+(1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod7)

mod8 = glm(duration ~ temp + precip + (1|ind) + (1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod8)

mod9 = glm(duration ~ temp + pluvi + (1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod9)

mod10 = glm(duration ~ temp + pluvi + (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod10)

mod11 = glm(duration ~ spp/temp+ (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod11)

library(bbmle)
resultado<-ICtab(mod6, mod7, mod8, mod9, mod10, mod11,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(duracao_antese_0404_clima_polli_sem0))
resultado
compare.fits(duration ~ temp, data=duracao_antese_0404_clima_polli_sem0, mod6, mod7)
model.comparison(mod7, mod11)

flexplot(duration~temp,data=duracao_antese_0404_clima_polli_sem0, method = "poisson")
flexplot(duration~pluvi,data=duracao_antese_0404_clima_polli_sem0, method = "poisson")
flexplot(temp~year, data=duracao_antese_0404_clima_polli_sem0, method = "poisson")

persp = ggplot(duracao_antese_0404_clima_polli_sem0, aes(x = spp, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flowering Duration") + xlab("Species")+
  theme_classic()+theme(text=element_text(size=12), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp

library(viridis)

graph2 = cbind(frutos, clima2$temp)
colnames(graph2) = c("Index","Local","Fam","spp",
                     "Fruto","ind","Year","Duration","temp")

fig2=ggplot(graph2, aes(x=Year, y=Duration)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
  stat_smooth(aes(y = a + temp*b), method = "glm", se = TRUE, fullrange = TRUE,
              size = 0.5, color = "red") +
  scale_y_continuous("Duration of Flowering", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Temperature",
                                         labels = scales::number_format(accuracy = 0.1))) +
  scale_x_continuous(breaks = c(2005,2010,2015, 2019))+
  theme_classic()+theme(text=element_text(size=22),
                        axis.line.y.right = element_line(color = "red"), 
                        axis.ticks.y.right = element_line(color = "red"),
                        axis.text.y.right = element_text(color = "red"), 
                        axis.title.y.right = element_text(color = "red"))
fig2





################################ --Q4 - Grupos Reproducao --  #####################
###########para congresso AFRICA###########

autocomp = filter(duracao_antese_0404_clima_polli_sem0, spp == "Byrsonima basiloba" | spp == "Campomanesia pubescens" | 
                    spp == "Miconia albicans" | spp == "Bowdichia virgilioides" | 
                    spp == "Miconia stenostachya")
SistRep = c(rep("Autocompativel", 620))
autocomp = cbind(autocomp, SistRep)         

incompat = filter(duracao_antese_0404_clima_polli_sem0, spp == "Banisteriopsis campestris" | spp == "Byrsonima intermedia" | 
                    spp == "Bauhinia rufa" | spp == "Tocoyena formosa" )
SistRep = c(rep("Incompativel", 620))
incompat = cbind(incompat, SistRep)         
data270623 = rbind(autocomp, incompat)


library(clipr)
write_clip(data270623) 
#falta fazer a compara��o com o clima nos modelos 3 -  vou adicionar na tabela os dados de temp e precip
##################### --Models--  #####################
mod25 = lmer(duration ~ scale(year)/SistRep + (1|ind), data=data270623)
summary(mod25)
mod26 = glmer(duration ~ SistRep + (1|ind), data=data270623, family = poisson())
summary(mod26)

mod27 = glmer(duration ~ SistRep + scale(year) +(1|ind), data=data270623, family = poisson())
summary(mod27)

mod28 = glmer(duration ~ scale(year)/SistRep + (1|ind), data=data270623, family = poisson())
summary(mod28)
### Sistemas reprodutivos aninhados dentro do fator ano - não sao dois
### fatores independentes.

plot_model(mod3, type = 'est')
?ICtab
resultado<-ICtab(mod25, mod26, mod27, mod28,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(data270623))
resultado
 ################com o clima_para Africa
mod66 = glm(duration ~ temp + (1|ind)+(1|year), data = data270623, family = poisson())
summ(mod66)

mod67 = glm(duration ~ temp + pluvi + (1|ind)+(1|year), data = data270623, family = poisson())
summ(mod67)

mod68 = glm(duration ~ temp + precip + (1|ind) + (1|year), data = data270623, family = poisson())
summ(mod68)

mod69 = glm(duration ~ temp + pluvi + (1|year), data = data270623, family = poisson())
summ(mod69)

mod70 = glm(duration ~ temp + pluvi + (1|ind), data = data270623, family = poisson())
summ(mod70)

mod71 = glm(duration ~ spp/temp+ (1|ind), data = data270623, family = poisson())
summ(mod71)

library(bbmle)
resultado<-ICtab(mod66, mod67, mod68, mod69, mod70, mod71,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(duracao_antese_0404_clima_polli_sem0))
resultado
##################################################final_congresso_africa###

##################### --Models--  #####################
mod01 = lmer(duration ~ scale(year)/pollinator + (1|ind), data=duracao_antese_0404_clima_polli_sem0)
summary(mod01)
mod10 = glmer(duration ~ pollinator + (1|ind), data=duracao_antese_0404_clima_polli_sem0, family = poisson())
summary(mod10)

mod20 = glmer(duration ~ pollinator + scale(year) +(1|ind), data=duracao_antese_0404_clima_polli_sem0, family = poisson())
summary(mod20)

mod30 = glmer(duration ~ scale(year)/pollinator + (1|ind), data=duracao_antese_0404_clima_polli_sem0, family = poisson())
summary(mod30)
### Sistemas reprodutivos aninhados dentro do fator ano - não sao dois
### fatores independentes.

plot_model(mod3, type = 'est')

resultado<-ICtab(mod01, mod10, mod20, mod30,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(duracao_antese_0404_clima_polli_sem0))
resultado

############################clima_pollination _DURA��O#####


mod6 = glm(duration ~ temp + pollinator +(1|ind)+(1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod6)

mod7 = glm(duration ~ temp + pluvi + pollinator + (1|ind)+(1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod7)

mod8 = glm(duration ~ temp + precip + pollinator + (1|ind) + (1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod8)

mod13 = glm(duration ~ pollinator + temp + pluvi + precip + (1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod13)
mod9 = glm(duration ~ temp + pluvi + pollinator + (1|year), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod9)

mod10 = glm(duration ~ temp + pluvi + (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod10)

mod11 = glm(duration ~ spp/temp+ pollinator + (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod11)

mod12 = glm(duration ~ pollinator + temp + pluvi + precip + (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod12)

##melhor modelo##
mod14 = glm(duration ~ pollinator/temp+ spp + (1|ind), data = duracao_antese_0404_clima_polli_sem0, family = poisson())
summ(mod14)

library(bbmle)
resultado<-ICtab(mod6, mod7, mod8, mod9, mod10, mod11,mod12,mod13,mod14,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(duracao_antese_0404_clima_polli_sem0))
resultado

######################clima vs pollination _FIM##########
##################### --Graphs--  #####################
library(viridis)
fig5=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration, fill = pollinator)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Flowering Duration") +xlab("Years")+
  #     ylim(0, 2.5)+
  theme_classic()+theme(text=element_text(size=22))
fig5+scale_color_viridis()+labs(fill = "Pollination")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

#################### FRUTIFICAÇÃO - DURAÇÃO ###########################
frutos = frutificacao_limpos[,1:6]
a = frutificacao_limpos 
Year = c(rep(2005, 928),rep(2006, 928),rep(2007, 928),rep(2008, 928),rep(2009, 928), rep(2010, 928),
         rep(2011, 928),rep(2012, 928),rep(2013, 928),rep(2014, 928),rep(2015, 928),rep(2016, 928),
         rep(2017, 928),rep(2018, 928),rep(2019, 928),rep(2020, 928))
Duration = c(a$`2005`,a$`2006`,a$`2007`,a$`2008`,a$`2009`,a$`2010`,a$`2011`,
             a$`2012`,a$`2013`,a$`2014`,a$`2015`,a$`2016`,a$`2017`,a$`2018`,a$`2019`,a$`2020`)
frutos = data.frame(frutos, Year, Duration)
save(dados, clima2, clima, frutos, file = "AmandaDados.RData")
load("AmandaDados.RData")

flexplot(duration~1, data=frutos_duracao_clima_poll_sem0)
flexplot(duration~year, data=frutos_duracao_clima_poll_sem0, method = "poisson")

persp = ggplot(frutos_duracao_clima_poll_sem0, aes(x = spp, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Fruiting Duration") + xlab("Species")+ ylim(0,12)+
  theme_classic()+theme(text=element_text(size=16), legend.position = "none",
                        axis.text.x = element_text(angle = 90,face = "italic", vjust = 0.5, hjust=1))
persp

##################### --Q1 - Duration ~ spp/Year--  #####################
library(glmmTMB)
mod0 = glmmTMB(duration ~ 1,
               zi = ~ 1,
               data = frutos_duracao_clima_poll_sem0, family = "poisson")

summary(mod0)

mod1 = glmmTMB(duration ~ year,
               zi = ~ 1,
               data = frutos_duracao_clima_poll_sem0, family = "poisson")
summary(mod1)

mod2 = glmmTMB(duration ~ spp+year,
               zi = ~ 1,
               data = frutos_duracao_clima_poll_sem0, family = "poisson")
summary(mod2)
#frutos$Espécie = gsub("Diospyros hispida", "Dyospiros hispida", frutos$Espécie) 
#frutos$Espécie = gsub("Schefflera  vinosa", "Schefflera vinosa", frutos$Espécie) 

mod3 = glmmTMB(duration ~ year*spp,
               zi = ~ 1,
               data = frutos_duracao_clima_poll_sem0, family = "poisson")
summary(mod3)

mod4 = glmmTMB(duration ~ spp*year + 
                 (1|Parcela),
               zi = ~ 1,
               data = frutos_duracao_clima_poll_sem0)

mod5 = glm(duration ~ spp*year, 
           data = frutos_duracao_clima_poll_sem0, family="poisson")

summary(mod5)
resultado<-ICtab(mod0, mod1, mod2,mod3, mod5,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(frutos_duracao_clima_poll_sem0))
resultado
##Greatest effects for Bauhinia (-), Byrsonima intermedia (-), Campomanesia pubescens (-),
## E gracilipes, E cuneifolium, E bimarginata (-), Memora (-), Miconia albicans, stenostachya,
### Myrcia bella
### Positive effects only for Qualea grandi


mod1 = glmer(duration ~ scale(Year) + (1|Parcela),
             data = frutos, family = "poisson")
summary(mod1)

mod2 = glmer(Duration ~ Espécie+Year+(1|Parcela),
           data = frutos, family = "poisson")
summary(mod2)

plot_model(mod5, type = "pred")

##################### --Q2 - Duration ~ spp*Cat--  #####################
earlier = filter(frutos_duracao_clima_poll_sem0, year == 2005| year==2006|year==2007)
earlier$CatTime = c(rep("early", 1159))
later = filter(frutos_duracao_clima_poll_sem0, year == 2017| year==2018| year==2019)
later$CatTime = c(rep("late", 945))
dados_frutos_270323 = rbind(earlier, later)

mod0 = glmer(duration ~ CatTime + (1|ind), data = dados_frutos_270323, family = poisson())
summ(mod0)
mod1 = glm(duration ~ CatTime+spp, data = dados_frutos_270323, family = poisson())
summ(mod1)
mod2 = glm(duration ~ spp/CatTime, data = dados_frutos_270323, family = poisson())
summ(mod2)
## Positive effects: only  Qualea;
## Strongest negative effects
## General negative effect (-0.27).

#################### Graphs #####################
dados3 = filter(dados2, Duration>0)
graph = filter(dados2, Espécie == "Miconia stenostachya"|
                 Espécie == "Bauhinia rufa"|
                 Espécie == "Rourea induta"|
                 Espécie == "Schefflera vinosa"|
                 Espécie ==  "Stryphnodendron obovatum")

library(viridis)
fig1=ggplot(dados_frutos_270323, aes(x=year, y=duration, fill = spp)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Fruiting") +xlab("Years")+
  #   xlim(2005, 2019)+
  theme_classic()+theme(text=element_text(size=22), legend.text = element_text(face="italic"))
fig1+scale_color_viridis()+labs(fill = "Species")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

ylim.prim <- c(0, 0.8)  
ylim.sec <- c(20, 23) 

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

graph2 = cbind(dados, clima2$temp)
colnames(graph2) = c("Index","Local","spp",
                     "ind","Duration","Year","temp")

fig2=ggplot(dados_frutos_270323, aes(x=year, y=duration)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
  stat_smooth(aes(y = a + temp*b), method = "glm", se = TRUE, fullrange = TRUE,
              size = 0.5, color = "red") +
  scale_y_continuous("Duration of Fruiting", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Temperature",
                                         labels = scales::number_format(accuracy = 0.1))) +
  scale_x_continuous(breaks = c(2005,2010,2015, 2019))+
  theme_classic()+theme(text=element_text(size=22),
                        axis.line.y.right = element_line(color = "red"), 
                        axis.ticks.y.right = element_line(color = "red"),
                        axis.text.y.right = element_text(color = "red"), 
                        axis.title.y.right = element_text(color = "red"))
fig2

commcat = ggplot(dados_frutos_270323, aes(x = CatTime, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Community Fruiting Duration") + xlab("")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none")
#  annotate("text", x = c(1), y = 8, label = "a", size=6)+
#  annotate("text", x = c(2,3,4), y = 8, label = "b", size=6)+
#  annotate("text", x = 0.85, y = 1, label = "C", size=5)+
#  annotate("text", x = 1.1, y = 1, label = "NC", size=5)+
#  scale_fill_manual(values=c("white","black","white", "white", "white", "black","white", "black"))
commcat

fig1 = ggplot(dados_frutos_270323, aes(x = CatTime, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Community Flowering Duration") + xlab("")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none")
#  annotate("text", x = c(1), y = 8, label = "a", size=6)+
#  annotate("text", x = c(2,3,4), y = 8, label = "b", size=6)+
#  annotate("text", x = 0.85, y = 1, label = "C", size=5)+
#  annotate("text", x = 1.1, y = 1, label = "NC", size=5)+
#  scale_fill_manual(values=c("white","black","white", "white", "white", "black","white", "black"))
commcat

##################### --Q3 - Duration ~ Clima--  #####################
##################### --Climatic Variables--  #####################
clima2 = clima[rep(seq_len(nrow(clima)), each = 928), ]
dados3 = cbind(frutos, clima2)

cresc.cor=clima2[,c(2:4)]
cor.test(cresc.cor$prec,cresc.cor$plu)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=2)
}
pairs(cresc.cor, upper.panel = panel.cor)
chart.Correlation(cresc.cor, histogram=TRUE, pch="+")

##################### --Models--  #####################
mod6 = glm(duration ~ temp + (1|ind)+(1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod6)

mod7 = glm(duration ~ temp + pluvi + (1|ind)+(1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod7)

mod8 = glm(duration ~ temp + precip + (1|ind) + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod8)

mod9 = glm(duration ~ temp + pluvi + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod9)

mod10 = glm(duration ~ temp + pluvi + (1|ind), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod10)

mod11 = glmer(duration ~ spp/temp+ (1|ind), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod11)

library(bbmle)
resultado<-ICtab(mod6, mod7, mod8, mod9, mod10, mod11,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(frutos_duracao_clima_poll_sem0))
resultado
compare.fits(duration ~ temp, data=dados_frutos_270323, mod6, mod7)
model.comparison(mod7, mod11)
flexplot(duration~temp,data=frutos_duracao_clima_poll_sem0, method = "poisson")
flexplot(duration~pluvi,data=frutos_duracao_clima_poll_sem0, method = "poisson")
flexplot(temp~year, data=frutos_duracao_clima_poll_sem0, method = "poisson")

persp = ggplot(frutos_duracao_clima_poll_sem0, aes(x = spp, y =duration)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Fruiting Duration") + xlab("Species")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp

library(viridis)
fig2=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y = duration, fill = Espécie)) + 
  geom_point( alpha=.5) +
  stat_smooth(aes(y = Duration),method="glm", se=TRUE, fullrange=TRUE, color_palette("viridis"),
              method.args = list(family=poisson)) + 
  
  ylab("Duration of Flowering") +xlab("Years")+
  #  xlim(100, 4000)
  theme_classic()+theme(text=element_text(size=12), legend.text = element_text(face="italic"))
fig2+scale_color_viridis()

library(viridis)

graph2 = cbind(frutos, clima2$temp)
colnames(graph2) = c("Index","Local","Fam","spp",
                     "Fruto","ind","Year","Duration","temp")

fig2=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y=duration)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Fruiting") +xlab("Years")+
  stat_smooth(aes(y = a + temp*b), method = "glm", se = TRUE, fullrange = TRUE,
              size = 0.5, color = "red") +
  scale_y_continuous("Duration of Fruiting", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Temperature",
                                         labels = scales::number_format(accuracy = 0.1))) +
  scale_x_continuous(breaks = c(2005,2010,2015, 2019))+
  theme_classic()+theme(text=element_text(size=22),
                        axis.line.y.right = element_line(color = "red"), 
                        axis.ticks.y.right = element_line(color = "red"),
                        axis.text.y.right = element_text(color = "red"), 
                        axis.title.y.right = element_text(color = "red"))
fig2


##################### --Q4 - Grupos Reproducao --  #####################
autocomp = filter(frutos, Espécie == "Byrsonima basiloba" | Espécie == "Campomanesia pubescens" | 
                    Espécie == "Miconia albicans" | Espécie == "Bowdichia virgilioides" | 
                    Espécie == "Miconia stenostachya")
SistRep = c(rep("Autocompativel", 1760))
autocomp = cbind(autocomp, SistRep)         

incompat = filter(frutos, Espécie == "Banisteriopsis campestris" | Espécie == "Byrsonima intermedia" | 
                    Espécie == "Bauhinia rufa" | Espécie == "Tocoyena formosa" )
SistRep = c(rep("Incompativel", 1232))
incompat = cbind(incompat, SistRep)         
data2 = rbind(autocomp, incompat)

##################### --Models--  #####################
mod = lmer(duration ~ year/pollination + (1|ind), data=frutos_duracao_clima_poll_sem0)
summary(mod)
mod1 = glmer(duration ~ pollination + (1|ind), data=frutos_duracao_clima_poll_sem0, family = poisson())
summary(mod1)

mod2 = glmer(duration ~ pollination + scale(year) +(1|ind), data=frutos_duracao_clima_poll_sem0, family = poisson())
summary(mod2)

mod3 = glmer(duration ~ scale(year)/pollination + (1|ind), data=frutos_duracao_clima_poll_sem0, family = poisson())
summary(mod3)
### Sistemas reprodutivos aninhados dentro do fator ano - não sao dois
### fatores independentes.

plot_model(mod3, type = 'est')

resultado<-ICtab(mod, mod1, mod2, mod3,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(frutos_duracao_clima_poll_sem0))
resultado



############################clima_pollination _DURA��O_frutos#####


mod6 = glm(duration ~ temp + pollination +(1|ind)+(1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod6)

mod7 = glm(duration ~ temp + pluvi + pollination + (1|ind)+(1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod7)

mod8 = glm(duration ~ temp + precip + pollination + (1|ind) + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod8)

mod13 = glm(duration ~ pollination + temp + pluvi + precip + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod13)

mod9 = glm(duration ~ temp + pluvi + pollination + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod9)

mod10 = glm(duration ~ temp + pluvi + (1|ind), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod10)

mod11 = glm(duration ~ pollination/temp+ spp + (1|year), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod11)


mod12 = glm(duration ~ pollination + temp + pluvi + precip + (1|ind), data = frutos_duracao_clima_poll_sem0, family = poisson())
summ(mod12)

library(bbmle)
resultado<-ICtab(mod6, mod7, mod8, mod9, mod10, mod11,mod12,mod13,
                 type="AICc", sort=T, weights=T, base=T, nobs=nrow(frutos_duracao_clima_poll_sem0))
resultado

######################clima vs pollination _FIM##########
##################### --Graphs--  #####################

fig4=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y=duration, fill = spp+pollination)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duração da Frutificação") +xlab("Anos")+
#     ylim(0, 2.5)+
  theme_classic()+theme(text=element_text(size=22))
fig4+scale_color_viridis()+labs(fill = "Sist. Rep.")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

##################### --INICIOS: Data Management--  #####################
##################### --Angle conversion--  #####################
#save(dados_novos, file="dados_novos.RData")
#save(DurFlor,DurFrutos, file="dados_duracao.RData")
load("dados_novos.Rdata")
load("dados_duracao.RData")

dados_novos$inicio[dados_novos$inicio == 'set'] <- 9
dados_novos$inicio[dados_novos$inicio == 'ago'] <- 8
dados_novos$inicio[dados_novos$inicio == 'jul'] <- 7
dados_novos$inicio[dados_novos$inicio == 'jun'] <- 6
dados_novos$inicio[dados_novos$inicio == 'mai'] <- 5
dados_novos$inicio[dados_novos$inicio == 'abr'] <- 4
dados_novos$inicio[dados_novos$inicio == 'mar'] <- 3
dados_novos$inicio[dados_novos$inicio == 'fev'] <- 2
dados_novos$inicio[dados_novos$inicio == 'jan'] <- 1
dados_novos$inicio[dados_novos$inicio == 'out'] <- 10
dados_novos$inicio[dados_novos$inicio == 'nov'] <- 11
dados_novos$inicio[dados_novos$inicio == 'dez'] <- 12
dados_novos$inicio = as.numeric(dados_novos$inicio)
library(circular)
inicio.angulo = (360 * dados_novos$inicio /12)-15
inicio.circ = circular(inicio.angulo, units = "degrees", template = "clock12", modulo = "2pi")
inicio.rad = circular(inicio.angulo, units = "radians", template = "clock12", modulo = "2pi")
inicio = cbind(dados_novos, inicio.circ, inicio.rad)
#### Separando Fenofases
iniflor = subset(inicio, fenofase=="A" | fenofase == "BO")
inifruto = subset(dados_novos, fenofase == "FV" | fenofase == "FM")
#### Tirando NA's 
library(tidyr)
iniflorfull =  drop_na(iniflor)
inifrutofull = drop_na(inifruto)
par(mar=c(1, 1, 1, 1))

plot(iniflorfull$inicio)

##################### --UNIMODALITY test (Rayleigh)--  #####################
rayleigh.test(iniflorfull$inicio.circ)
rayleigh.test(inifrutofull$inicio.circ)
mean.circular(iniflorfull$inicio.circ)
##################### --Grupos: FLOR--  #####################
earlier.flor = filter(iniflorfull, ano == 2005 | ano == 2006| ano == 2007)
decade.flor = filter(iniflorfull, ano == 2015|ano==2016|ano == 2017)
later.flor = filter(iniflorfull, ano == 2017|ano==2018|ano == 2019)

rayleigh.test(earlier.flor$inicio.circ)
rayleigh.test(decade.flor$inicio.circ)
rayleigh.test(later.flor$inicio.circ)

plot(earlier.flor$inicio.circ)
plot(later.flor$inicio.circ)
arrows.circular(mean(earlier.flor$inicio.circ))
arrows.circular(mean(later.flor$inicio.circ), col = "red")
arrows.circular(mean(decade.flor$inicio.circ), col = "green")
### General advancement between earlier and later! Later in time, 
### it seems that the flower starts earlier in the year.

##################### --Grupos: Fruto--  #####################
earlier.fruto = filter(inifrutofull, ano == 2005 | ano == 2006| ano == 2007)
decade.fruto = filter(inifrutofull, ano == 2015|ano==2016|ano == 2017)
later.fruto = filter(inifrutofull, ano == 2017|ano==2018|ano == 2019)
plot(earlier.fruto$inicio.circ)
plot(later.fruto$inicio.circ)
arrows.circular(mean(earlier.fruto$inicio.circ))
arrows.circular(mean(later.fruto$inicio.circ), col = "red")
arrows.circular(mean(decade.fruto$inicio.circ), col = "green")

watson.two.test(earlier.flor$inicio.circ, later.flor$inicio.circ)
watson.two.test(earlier.flor$inicio.circ, decade.flor$inicio.circ)

watson.two.test(earlier.fruto$inicio.circ, later.fruto$inicio.circ)
watson.two.test(earlier.fruto$inicio.circ, earlier.fruto$inicio.circ)

circular::rose.diag(earlier.flor$inicio.circ, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
circular::rose.diag(later.flor$inicio.circ, bins=12, col="pink", cex=1, prop = 1.3, add=TRUE)

##################### --Graphs--  #####################
plot(iniflorfull$inicio.circ)
circular::rose.diag(iniflorfull$inicio.circ, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)

arrows.circular(mean(inicio.circ, na.rm=TRUE))
arrows.circular(mean(na.omit(inicio.circ, na.rm = TRUE)), col = "red")
lines(density.circular(na.omit(inicio.circ),bw = 40))

bla = ggplot(iniflorfull, aes(x = inicio.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Flowering")+
  xlab("")+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16))
bla

ble = ggplot(inifrutofull, aes(x = inicio.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Fruiting")+
  xlab("")+ 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16))
ble

require(gridExtra)
ggarrange(bla, ble, common.legend = TRUE, legend="right")
??ggarrange
##################### --Regressão CIEC: Botao--  #####################
require(bpnreg)
mod = bpnme(inicio.rad ~ ano + (1|parcela) , iniflorfull, its = 100)
print(mod)

mod1 = bpnme(inicio.rad ~ ano + spp+ (1|parcela) , iniflorfull, its = 100)
print(mod1)

### Identificando sp abundantes e raras:
bla = rep(1, 2783)
aggregate(bla~iniflorfull$spp, FUN="sum")

iniabund.flor = subset(iniflorfull, spp != "Aspidosperma tomentosum")
iniabund.flor = subset(iniabund.flor, spp != " Adenocalymma axillare")
iniabund.flor = subset(iniabund.flor, spp != " Bowdichia virgilioides")
iniabund.flor = subset(iniabund.flor, spp != "Connarus suberosus")
iniabund.flor = subset(iniabund.flor, spp != "Dalbergia miscolobium")
flexplot(inicio~1, data=iniabund.flor)
flexplot(inicio~ano, data=iniabund.flor)

mod1 = bpnme(inicio.rad ~ ano + spp+ (1|ind) , iniabund.flor, its = 100)
print(mod1)

mod2 = bpnme(inicio.rad ~ ano / spp+ (1|parcela) , iniabund.flor, its = 100)
print(mod2)
## there seems to be no relationship...

ble = ggplot(iniabund.flor, aes(x = inicio.circ, fill = as.factor(spp))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,12), breaks = seq(0, 12, by = 1))+theme_minimal()
ble

bli = ggplot(iniabund.flor, aes(x = inicio.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bli


bla = rep(1, 5432)
aggregate(bla~inifrutofull$spp, FUN="sum")

iniabund.fruto = subset(inifrutofull, spp != "Bowdichia virgilioides")
iniabund.fruto = subset(iniabund.fruto, spp != " Dalbergia miscolobium")
iniabund.fruto2 = iniabund.fruto[,-c(9:11)]

mod2 = bpnme(inicio.rad ~ ano / spp+ (1|parcela) , iniabund.fruto2, its = 100)
print(mod2)

## there seems to be a slight decrease along time
## no significant;
ble = ggplot(iniabund.fruto, aes(x = inicio.circ, fill = as.factor(spp))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
ble

bli = ggplot(iniabund.fruto, aes(x = inicio.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bli

##################### --Q3 - Inicio ~ Clima--  #####################
##################### --Climatic Variables--  #####################
bla = rep(1, 2776)
aggregate(bla~iniabund.flor$ano, FUN="sum")

clima2 <- rbind(as.data.frame(lapply(clima[1,], rep, 146)),
                as.data.frame(lapply(clima[2,], rep, 245)),
                as.data.frame(lapply(clima[3,], rep, 296)),
                as.data.frame(lapply(clima[4,], rep, 270)),
                as.data.frame(lapply(clima[5,], rep, 104)),
                as.data.frame(lapply(clima[6,], rep, 180)),
                as.data.frame(lapply(clima[7,], rep, 200)),
                as.data.frame(lapply(clima[8,], rep, 184)),
                as.data.frame(lapply(clima[9,], rep, 152)),
                as.data.frame(lapply(clima[10,], rep, 231)),
                as.data.frame(lapply(clima[11,], rep, 152)),
                as.data.frame(lapply(clima[12,], rep, 119)),
                as.data.frame(lapply(clima[13,], rep, 234)),
                as.data.frame(lapply(clima[14,], rep, 126)),
                as.data.frame(lapply(clima[15,], rep, 137))
)

dados.flor = cbind(iniabund.flor, clima2)

cresc.cor=clima[,c(2:4)]
cor.test(cresc.cor$prec,cresc.cor$plu)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=2)
}
pairs(cresc.cor, upper.panel = panel.cor)

##################### --Models--  #####################
flexplot(inicio~temp,data=dados.flor, method = "poisson")
flexplot(temp~ano,data=dados.flor, method = "poisson")
flexplot(plu~ano, data=dados.flor, method = "poisson")

persp = ggplot(dados.flor, aes(x = reorder(spp, inicio), y =inicio)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Start") + xlab("Species")+
  theme_classic()+theme(text=element_text(size=12), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp + facet_wrap(~year)

library(viridis)
fig2=ggplot(dados.flor, aes(x=ano, y = inicio, fill = spp)) + 
  geom_point( alpha=.5) +
  stat_smooth(aes(y = inicio),method="glm", se=TRUE, fullrange=TRUE, color_palette("viridis"),
              method.args = list(family=poisson)) + 
  
  ylab("Duration of Flowering") +xlab("Years")+
   ylim(0, 12)+
  theme_classic()+theme(text=element_text(size=12), legend.text = element_text(face="italic"))
fig2+scale_color_viridis()

library(viridis)

mod1 = bpnme(inicio.rad ~ temp/spp+ (1|ind) , dados.flor, its = 100)
print(mod1)

mod2 = bpnme(inicio.rad ~ plu / spp+ (1|parcela) , dados.flor, its = 100)
print(mod2)

##################### --Q4 - Grupos Reproducao --  #####################
colnames(dados.flor)[10] <- "Pollinator"
autocomp = filter(dados.flor, Pollinator == "no")

SistRep = c(rep("Autocompativel", 1329))
autocomp = cbind(autocomp, SistRep)         

incompat = filter(dados.flor, Pollinator == "yes" )
SistRep = c(rep("Incompativel", 1447))
incompat = cbind(incompat, SistRep)         
data2 = rbind(autocomp, incompat)

##################### --Models--  #####################
mod1 = bpnme(inicio.rad ~ temp/spp+ (1|ind) , incompat, its = 100)
print(mod1)

mod2 = bpnme(inicio.rad ~ plu / spp+ (1|parcela) , incompat, its = 100)
print(mod2)

flexplot(inicio.rad~temp|ano,data=incompat, method = "poisson")
flexplot(temp~ano,data=dados.flor, method = "poisson")
flexplot(plu~ano, data=dados.flor, method = "poisson")

persp = ggplot(incompat, aes(x = reorder(spp, inicio), y =inicio)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Start") + xlab("Species")+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp + facet_wrap(~ano)

##################### --GRUPOS de ESPÉCIES--  #####################
autocomp = filter(dados.flor, spp == "Byrsonima basiloba" | spp == "Campomanesia pubescens" | 
                    spp == "Miconia albicans" | spp == "Bowdichia virgilioides" | 
                    spp == "Miconia rubiginosa")
SistRep = c(rep("Autocompativel", 848))
autocomp = cbind(autocomp, SistRep)         

incompat = filter(dados.flor, spp == "Anadenanthera falcata" | spp == "Byrsonima intermedia" | 
                    spp == "Virola sebifera" | spp == "Myrcia splendens" | spp == "Virola sebifera" )
SistRep = c(rep("Incompativel", 294))
incompat = cbind(incompat, SistRep)         
data3 = rbind(autocomp, incompat)

mod1 = bpnme(inicio.rad ~ temp/SistRep+ (1|ind) , data3, its = 100)
print(mod1)

mod2 = bpnme(inicio.rad ~ plu / SistRep+ (1|parcela) , data3, its = 100)
print(mod2)

flexplot(inicio.rad~temp|ano,data=data3, method = "poisson")

persp = ggplot(data3, aes(x = reorder(SistRep, inicio), y =inicio)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Start") + xlab("Rep System")+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
persp + facet_wrap(~ano)

##################### --PICOS: Data Management--  #####################
par(mar=c(1, 1, 1, 1))

dados_novos$pico[dados_novos$pico == 'set'] <- 9
dados_novos$pico[dados_novos$pico == 'ago'] <- 8
dados_novos$pico[dados_novos$pico == 'jul'] <- 7
dados_novos$pico[dados_novos$pico == 'jun'] <- 6
dados_novos$pico[dados_novos$pico == 'mai'] <- 5
dados_novos$pico[dados_novos$pico == 'abr'] <- 4
dados_novos$pico[dados_novos$pico == 'mar'] <- 3
dados_novos$pico[dados_novos$pico == 'fev'] <- 2
dados_novos$pico[dados_novos$pico == 'jan'] <- 1
dados_novos$pico[dados_novos$pico == 'out'] <- 10
dados_novos$pico[dados_novos$pico == 'nov'] <- 11
dados_novos$pico[dados_novos$pico == 'dez'] <- 12
dados_novos$pico = as.numeric(dados_novos$pico)

pico.angulo = (360 * dados_novos$pico /12)-15
pico.circ = circular(pico.angulo, units = "degrees", template = "clock12", modulo = "2pi")
mean(pico.circ)
pico.rad = circular(pico.angulo, units = "radians", template = "clock12", modulo = "2pi")
dados_novos = cbind(dados_novos, pico.rad, pico.circ)
#### Separando Fenofases
picoflor = subset(dados_novos, fenofase=="A" | fenofase == "BO")
picofruto = subset(dados_novos, fenofase == "FV" | fenofase == "FM")
#### Tirando NA's 
picoflorfull =  drop_na(picoflor)
picofrutofull = drop_na(picofruto)
par(mar=c(1, 1, 1, 1))

plot(picoflorfull$pico.circ)

##################### --Rayleigh Tests--  #####################
rayleigh.test(picoflorfull$pico.circ)
rayleigh.test(picofrutofull$pico.circ)

##################### --Grupos: FLOR--  #####################
earlier.flor = filter(picoflorfull, ano == 2005 | ano == 2006| ano == 2007)
decade.flor = filter(picoflorfull, ano == 2015|ano==2016|ano == 2017)
later.flor = filter(picoflorfull, ano == 2017|ano==2018|ano == 2019)

rayleigh.test(earlier.flor$pico.circ)
rayleigh.test(decade.flor$pico.circ)
rayleigh.test(later.flor$pico.circ)

plot(earlier.flor$pico.circ)
plot(later.flor$pico.circ)
arrows.circular(mean(earlier.flor$pico.circ))
arrows.circular(mean(later.flor$pico.circ), col = "red")
arrows.circular(mean(decade.flor$pico.circ), col = "green")
### General advancement between earlier and later! Later in time, 
### it seems that the flower starts earlier in the year.

##################### --Grupos: Fruto--  #####################
earlier.fruto = filter(picofrutofull, ano == 2005 | ano == 2006| ano == 2007)
decade.fruto = filter(picofrutofull, ano == 2015|ano==2016|ano == 2017)
later.fruto = filter(picofrutofull, ano == 2017|ano==2018|ano == 2019)
plot(earlier.fruto$pico.circ)
plot(later.fruto$pico.circ)
arrows.circular(mean(earlier.fruto$pico.circ))
arrows.circular(mean(later.fruto$pico.circ), col = "red")
arrows.circular(mean(decade.fruto$pico.circ), col = "green")

watson.two.test(earlier.flor$pico.circ, later.flor$pico.circ)
watson.two.test(earlier.flor$pico.circ, decade.flor$pico.circ)

watson.two.test(earlier.fruto$pico.circ, later.fruto$pico.circ)
watson.two.test(earlier.fruto$pico.circ, decade.fruto$pico.circ)

circular::rose.diag(earlier.flor$pico.circ, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)
circular::rose.diag(later.flor$pico.circ, bins=12, col="pink", cex=1, prop = 1.3, add=TRUE)

##################### --Graphs--  #####################
plot(pico.circ)
circular::rose.diag(pico.circ, bins=12, col="darkgrey", cex=1, prop = 1.3, add=TRUE)

arrows.circular(mean(pico.circ, na.rm=TRUE))
arrows.circular(mean(na.omit(pico.circ, na.rm = TRUE)), col = "red")
lines(density.circular(na.omit(pico.circ),bw = 40))

bla = ggplot(picoflorfull, aes(x = pico.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bla

bla = ggplot(picoflorfull, aes(x = pico.circ, fill = as.factor(spp))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bla

##################### --Regressões Picos por Spp--  #####################
mod = bpnme(pico.rad ~ ano + (1|parcela) , picoflorfull, its = 100)
print(mod)
bla = rep(1, 2783)
aggregate(bla~picoflorfull$spp, FUN="sum")

picoabund.flor = subset(picoflorfull, spp != "Adenocalymma axillare")
picoabund.flor = subset(picoabund.flor, spp != "Aspidosperma tomentosum")
picoabund.flor = subset(picoabund.flor, spp != " Bowdichia virgilioides")
picoabund.flor = subset(picoabund.flor, spp != "Connarus suberosus")
picoabund.flor = subset(picoabund.flor, spp != "Dalbergia miscolobium")
picoabund.flor = subset(picoabund.flor, spp != "Diospyros hispida")
picoabund.flor = subset(picoabund.flor, spp != "Pouteria ramiflora")

mod0 = bpnme(pico.rad ~ ano + (1|parcela) , picoabund.flor, its = 100)
print(mod0)

mod1 = bpnme(pico.rad ~ ano + spp+ (1|parcela) , picoabund.flor, its = 100)
print(mod1)

mod2 = bpnme(pico.rad ~ ano / spp+ (1|parcela) , picoabund.flor, its = 100)
print(mod2)
## there seems to be a slight decrease along time
## no significant;
ble = ggplot(picoabund.flor, aes(x = pico.circ, fill = as.factor(spp))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
ble

bli = ggplot(picoabund.flor, aes(x = pico.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bli


bla = rep(1, 5432)

aggregate(bla~picofrutofull$spp, FUN="sum")

picoabund.fruto = subset(picofrutofull, spp != "Adenocalymma axillare")
picoabund.fruto = subset(picoabund.fruto, spp != "Aspidosperma tomentosum")
picoabund.fruto = subset(picoabund.fruto, spp != " Bowdichia virgilioides")
picoabund.fruto = subset(picoabund.fruto, spp != "Connarus suberosus")
picoabund.fruto = subset(picoabund.fruto, spp != "Dalbergia miscolobium")
picoabund.fruto = subset(picoabund.fruto, spp != "Pouteria ramiflora")

mod1 = bpnme(pico.rad ~ ano + spp+ (1|parcela) , picoabund.fruto, its = 100)
print(mod1)

mod1 = bpnme(pico.rad ~ spp/ano+ (1|parcela) , picoabund.fruto, its = 100)
print(mod1)

mod2 = bpnme(pico.rad ~ ano / spp+ (1|parcela) , picoabund.fruto, its = 100)
print(mod2)

## there seems to be a slight decrease along time
## no significant;
ble = ggplot(picoabund.fruto, aes(x = pico.circ, fill = as.factor(spp))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
ble

bli = ggplot(picoabund.fruto, aes(x = pico.circ, fill = as.factor(ano))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()
bli

##################### --Q3 - Picos ~ Clima--  #####################
bla = rep(1, 2760)
aggregate(bla~picoabund.flor$ano, FUN="sum")

clima2 <- rbind(as.data.frame(lapply(clima[1,], rep, 144)),
                as.data.frame(lapply(clima[2,], rep, 244)),
                as.data.frame(lapply(clima[3,], rep, 296)),
                as.data.frame(lapply(clima[4,], rep, 270)),
                as.data.frame(lapply(clima[5,], rep, 102)),
                as.data.frame(lapply(clima[6,], rep, 180)),
                as.data.frame(lapply(clima[7,], rep, 199)),
                as.data.frame(lapply(clima[8,], rep, 182)),
                as.data.frame(lapply(clima[9,], rep, 151)),
                as.data.frame(lapply(clima[10,], rep, 228)),
                as.data.frame(lapply(clima[11,], rep, 152)),
                as.data.frame(lapply(clima[12,], rep, 119)),
                as.data.frame(lapply(clima[13,], rep, 232)),
                as.data.frame(lapply(clima[14,], rep, 126)),
                as.data.frame(lapply(clima[15,], rep, 135))
)

dados.flor = cbind(picoabund.flor, clima2)

cresc.cor=clima2[,c(2:4)]
cor.test(cresc.cor$prec,cresc.cor$plu)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=2)
}
pairs(cresc.cor, upper.panel = panel.cor)

##################### --Models--  #####################
flexplot(pico~temp,data=dados.flor, method = "poisson")
flexplot(temp~ano,data=dados.flor, method = "poisson")
flexplot(plu~ano, data=dados.flor, method = "poisson")

persp = ggplot(dados.flor, aes(x = reorder(spp, pico), y =pico)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Peak") + xlab("Species")+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp + facet_wrap(~ano)

library(viridis)
fig2=ggplot(dados.flor, aes(x=ano, y = pico, fill = spp)) + 
  geom_point( alpha=.5) +
  stat_smooth(aes(y = inicio),method="glm", se=TRUE, fullrange=TRUE, color_palette("viridis"),
              method.args = list(family=poisson)) + 
  
  ylab("Peak of Flowering") +xlab("Years")+
  #  xlim(100, 4000)
  theme_classic()+theme(text=element_text(size=12), legend.text = element_text(face="italic"))
fig2+scale_color_viridis()

library(viridis)

mod1 = bpnme(pico.rad ~ temp/spp+ (1|parcela) , dados.flor, its = 100)
print(mod1)

mod2 = bpnme(pico.rad ~ plu / spp+ (1|parcela) , dados.flor, its = 100)
print(mod2)

##################### --Q4 - Grupos Reproducao --  #####################
colnames(dados.flor)[10] <- "Pollinator"
autocomp = filter(dados.flor, Pollinator == "no")

SistRep = c(rep("Autocompativel", 1321))
autocomp = cbind(autocomp, SistRep)         

incompat = filter(dados.flor, Pollinator == "yes" )
SistRep = c(rep("Incompativel", 1439))
incompat = cbind(incompat, SistRep)         
data2 = rbind(autocomp, incompat)

##################### --Models--  #####################
mod1 = bpnme(pico.rad ~ temp/spp+ (1|parcela) , incompat, its = 100)
print(mod1)

mod2 = bpnme(pico.rad ~ plu / spp+ (1|parcela) , incompat, its = 100)
print(mod2)

flexplot(pico.rad~temp|ano,data=incompat, method = "poisson")
flexplot(pico.rad~temp|ano,data=autocomp, method = "poisson")

persp = ggplot(incompat, aes(x = reorder(spp, pico), y =pico)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Peak") + xlab("Not Compatible")+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp + facet_wrap(~ano)

mod1 = bpnme(pico.rad ~ temp/spp+ (1|parcela) , autocomp, its = 100)
print(mod1)

mod2 = bpnme(pico.rad ~ plu / spp+ (1|parcela) , autocomp, its = 100)
print(mod2)

flexplot(pico.rad~temp|ano,data=autocomp, method = "poisson")

persp = ggplot(autocomp, aes(x = reorder(spp, pico/ano), y =pico)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Peak") + xlab("Compatible")+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.x = element_text(angle = 90, face="italic",vjust = 0.5, hjust=1))
persp + facet_wrap(~ano)

