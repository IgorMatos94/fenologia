colnames(DurFlor)
colnames(frutos_duracao_clima_poll_sem0)
colnames(duracao_antese_0404_clima_polli_sem0)
library(ggplot2)
library(viridis)
durflorgr=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y=duration,)) +  
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, linewidth = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Fruiting Duration") +xlab("Years")+
  #   xlim(2005, 2019)+
  theme_classic()+theme(text=element_text(size=22), legend.text = element_text(face="italic"))
durflorgr+scale_color_viridis()+labs(fill = "Pollinator")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

durfrugr=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y=duration, fill = pollination)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, linewidth = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Fruiting Duration") +xlab("Years")+
  #   xlim(2005, 2019)+
  theme_classic()+theme(text=element_text(size=22), legend.text = element_text(face="italic"))
durfrugr+scale_color_viridis()+labs(fill = "Pollinator")+scale_x_continuous(breaks = c(2005,2010,2015, 2019))

ylim.prim <- c(0, 0.8)  
ylim.sec <- c(20, 23) 

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


durflortemp=ggplot(duracao_antese_0404_clima_polli_sem0, aes(x=year, y=duration)) + 
  # geom_point(alpha=.5) +
  stat_smooth(method="glm", se=TRUE, fullrange=TRUE, size = 0.5,
              col = "black",
              method.args = list(family=poisson)) + 
  ylab("Duration of Flowering") +xlab("Years")+
  #stat_smooth(aes(y = a + temp*b), method = "glm", se = TRUE, fullrange = TRUE,
              size = 0.5, color = "red") +
 # scale_y_continuous("Duration of Flowering", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Temperature",
                                         labels = scales::number_format(accuracy = 0.1))) +
 # scale_x_continuous(breaks = c(2005,2010,2015, 2019))+
  theme_classic()+theme(text=element_text(size=22),
                        axis.line.y.right = element_line(color = "red"), 
                        axis.ticks.y.right = element_line(color = "red"),
                        axis.text.y.right = element_text(color = "red"), 
                        axis.title.y.right = element_text(color = "red"))
durflortemp

ylim.prim <- c(0, 1.8)  
ylim.sec <- c(20, 23) 

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

durfrutemp=ggplot(frutos_duracao_clima_poll_sem0, aes(x=year, y=duration)) + 
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
durfrutemp

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
library(circular)
pico.angulo = (360 * dados_novos$pico /12)-15
pico.circ = circular(pico.angulo, units = "degrees", template = "clock12", modulo = "2pi")
pico.rad = circular(pico.angulo, units = "radians", template = "clock12", modulo = "2pi")
pico = cbind(dados_novos, pico.circ, pico.rad)

#### Separando Fenofases
iniflor = subset(inicio, fenofase=="A" | fenofase == "BO")
inifruto = subset(inicio, fenofase == "FV" | fenofase == "FM")

picoflor = subset(pico, fenofase=="A" | fenofase == "BO")
picofruto = subset(pico, fenofase == "FV" | fenofase == "FM")

#### Tirando NA's 
library(tidyr)
iniflorfull =  drop_na(iniflor)
inifrutofull = drop_na(inifruto)
colnames(iniflorfull)

picoflorfull =  drop_na(picoflor)
picofrutofull = drop_na(picofruto)


mean.circular(iniflorfull$inicio.circ)
mean.circular(inifrutofull$inicio.circ)

persp = ggplot(iniflorfull, aes(x = reorder(spp, inicio), y =inicio)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Start") + xlab("Species")+ coord_flip()+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.y = element_text(face="italic"))
persp
persp + facet_wrap(~ano)

persp = ggplot(inifrutofull, aes(x = reorder(spp, inicio), y =inicio)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Fruit Start") + xlab("Species")+ coord_flip()+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.y = element_text(face="italic"))
persp
persp + facet_wrap(~ano)

persp = ggplot(picoflorfull, aes(x = reorder(spp, pico), y =pico)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Flower Peak") + xlab("Species")+ coord_flip()+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.y = element_text(face="italic"))
persp
persp + facet_wrap(~ano)

persp = ggplot(picofrutofull, aes(x = reorder(spp, pico), y =pico)) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+  
  ylab("Fruit Peak") + xlab("Species")+ coord_flip()+
  theme_classic()+theme(text=element_text(size=11), legend.position = "none",
                        axis.text.y = element_text(face="italic"))
persp
persp + facet_wrap(~ano)

colnames(picoflorfull)
colnames(picoflorfull) = c("Index" ,                "Local" ,                "spp"    ,              
                          "ind"                  , "fenofase"      ,        "parcela" ,             
                          "inicio"              ,  "ano"            ,       "flor/fruto",           
                          "pollinator", "pico"           ,       "pico.rad"             ,
                          "pico.angulo"        ,   "pico.rad.1"       ,     "pico.circ"   ,         
                          "pico.circ1"      ,     "pico.rad2"  )

colnames(picofrutofull) = c("Index" ,                "Local" ,                "spp"    ,              
                           "ind"                  , "fenofase"      ,        "parcela" ,             
                           "inicio"              ,  "ano"            ,       "flor/fruto",           
                           "pollinator", "pico"           ,       "pico.rad"             ,
                           "pico.angulo"        ,   "pico.rad.1"       ,     "pico.circ"   ,         
                           "pico.circ1"      ,     "pico.rad2"  )


colnames(iniflorfull) = c("Index" ,                "Local" ,                "spp"    ,              
                          "ind"                  , "fenofase"      ,        "parcela" ,             
                          "inicio"              ,  "ano"            ,       "flor/fruto",           
                           "pollinator", "pico"           ,       "pico.rad"             ,
                          "pico.angulo"        ,   "pico.rad.1"       ,     "pico.circ"   ,         
                           "inicio.circ"      ,     "inicio.rad"  )

iniflorfull$pollinator = replace(iniflorfull$pollinator, iniflorfull$pollinator=="yes", 1)
iniflorfull$pollinator = replace(iniflorfull$pollinator, iniflorfull$pollinator=="no", 0)

picoflorfull$pollinator = replace(picoflorfull$pollinator, picoflorfull$pollinator=="yes", 1)
picofrutofull$pollinator = replace(picofrutofull$pollinator, picofrutofull$pollinator=="no", 0)

bla = ggplot(iniflorfull, aes(x = inicio.circ, fill = as.factor(ano) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Flowering Start")+
  xlab("")+ ylim(-250,800)+labs(fill = "Year")+
  scale_fill_viridis_d()+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16),axis.text.x = element_blank(),axis.text.y  = element_blank())+ annotate("segment", x = 255, xend = 255, y = 0, yend = 480,
                                              colour = "black", size = 0.5, arrow = arrow())

bla

bli = ggplot(picoflorfull, aes(x = pico.circ, fill = as.factor(ano) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Flowering Peak")+
  xlab("")+ ylim(-250,1000)+labs(fill = "Year")+
  scale_fill_viridis_d()+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16),axis.text.x = element_blank(),axis.text.y  = element_blank())+ 
  annotate("segment", x = 255, xend = 255, y = 0, yend = 580,
  colour = "purple", size = 0.5, arrow = arrow())
bli

ble = ggplot(inifrutofull, aes(x = inicio.circ, fill = as.factor(ano) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Fruiting Start")+
  xlab("")+ ylim(-250,1000)+labs(fill = "Year")+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+
  scale_fill_viridis_d()+theme_minimal()+
  theme(text=element_text(size=16), axis.text.x = element_blank(), axis.text.y = element_blank())+
  annotate("segment", x = 315, xend = 315, y = 0, yend = 580,
             colour = "black", size = 0.5, arrow = arrow())
ble
blu = ggplot(picofrutofull, aes(x = pico.circ, fill = as.factor(ano) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Fruiting Peak")+
  xlab("")+ ylim(-250,1000)+labs(fill = "Year")+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+
  scale_fill_viridis_d()+theme_minimal()+
  theme(text=element_text(size=16), axis.text.x = element_blank(), axis.text.y = element_blank())+
  annotate("segment", x = 315, xend = 315, y = 0, yend = 680,
           colour = "purple", size = 0.5, arrow = arrow())
blu

library(ggpubr)
ggarrange(bla, ble, common.legend = TRUE,
          legend = "bottom")

ggarrange(bli, blu, common.legend = TRUE,
          legend = "bottom")

aggregate(inicio.circ~1, data=inifrutofull, FUN=mean)

colnames(iniflorfull) = c("Index" ,                "Local" ,                "spp"    ,              
                          "ind"                  , "fenofase"      ,        "parcela" ,             
                          "inicio"              ,  "ano"            ,       "flor/fruto",           
                           "pollinator", "pico"           ,       "pico.rad"             ,
                          "pico.angulo"        ,   "pico.rad.1"       ,     "pico.circ"   ,         
                           "inicio.circ"      ,     "inicio.rad"  )

iniflorfull$pollinator = replace(iniflorfull$pollinator, iniflorfull$pollinator=="yes", 1)
iniflorfull$pollinator = replace(iniflorfull$pollinator, iniflorfull$pollinator=="no", 0)

picoflorfull$pollinator = replace(picoflorfull$pollinator, picoflorfull$pollinator=="yes", 1)
picoflorfull$pollinator = replace(picoflorfull$pollinator, picoflorfull$pollinator=="no", 0)
picofrutofull$pollinator = replace(picofrutofull$pollinator, picofrutofull$pollinator=="yes", 1)
picofrutofull$pollinator = replace(picofrutofull$pollinator, picofrutofull$pollinator=="no", 0)
picoflorfull$pollinator = as.numeric(picoflorfull$pollinator)
picofrutofull$pollinator = as.numeric(picofrutofull$pollinator)

iniflorfull$pollinator = as.numeric(iniflorfull$pollinator)
florpol = ggplot(iniflorfull, aes(x = inicio.circ, fill = as.factor(pollinator) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Flowering Start")+
  xlab("")+
 ylim(-150,800)+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16), 
        axis.text.x = element_blank(), axis.text.y = element_blank())+
   scale_fill_viridis_d(name = "Pollinator")+ annotate("segment", x = 255, xend = 255, y = 0, yend = 680,
                                                      colour = "purple", size = 0.5, arrow = arrow())+
  annotate("segment", x = 285, xend = 285, y = 0, yend = 370,
           colour = "orange", size = 0.5, arrow = arrow())


florpol

aggregate(pico.circ~pollinator, picoflorfull, FUN=mean)

florpol2 = ggplot(picoflorfull, aes(x = pico.circ, fill = as.factor(pollinator) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Flowering Peak")+
  xlab("")+
  ylim(-150,1000)+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16), 
        axis.text.x = element_blank(), axis.text.y = element_blank())+
  scale_fill_viridis_d(name = "Pollinator")+ annotate("segment", x = 255, xend = 255, y = 0, yend = 680,
                                                      colour = "purple", size = 0.5, arrow = arrow())+
  annotate("segment", x = 285, xend = 285, y = 0, yend = 290,
           colour = "orange", size = 0.5, arrow = arrow())


florpol2

colnames(inifrutofull) = c("Index"      ,           "Local"     ,            "spp"   ,               
                           "ind"         ,          "fenofase"      ,        "parcela"   ,           
                           "inicio"   ,             "ano"        ,           "flor/fruto"  ,         
                           "pollinator" ,"pico"           ,       "pico.rad"  ,           
                           "pico.angulo"       ,    "pico.rad.1"     ,       "pico.circ"  ,          
                           "inicio.circ"      ,     "inicio.rad"  )

inifrutofull$pollinator = replace(inifrutofull$pollinator, inifrutofull$pollinator=="yes", 1)
inifrutofull$pollinator = replace(inifrutofull$pollinator, inifrutofull$pollinator=="no", 0)

inifrutofull$pollinator = as.numeric(inifrutofull$pollinator)
aggregate(inicio.circ~pollinator, data=inifrutofull, FUN=mean)

frupol = ggplot(inifrutofull, aes(x = inicio.circ, fill = as.factor(pollinator) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Fruiting Start")+
  xlab("")+
  ylim(-150,1000)+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16), 
        axis.text.x = element_blank(),axis.text.y = element_blank())+
  scale_fill_viridis_d(name = "Pollinator")+ annotate("segment", x = 315, xend = 315, y = 0, yend = 880,
                                                     colour = "purple", size = 0.5, arrow = arrow())+
  annotate("segment", x = 345, xend = 345, y = 0, yend = 310,
           colour = "orange", size = 0.5, arrow = arrow())


frupol
ggarrange(florpol, frupol, common.legend = TRUE,
          legend="bottom")

aggregate(pico.circ~pollinator, picofrutofull, FUN=mean)

frupol2 = ggplot(picofrutofull, aes(x = pico.circ, fill = as.factor(pollinator) )) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(start = 0) + ylab("Fruiting Peak")+
  xlab("")+
  ylim(-150,1000)+
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 30))+theme_minimal()+
  theme(text=element_text(size=16), 
        axis.text.x = element_blank(),axis.text.y = element_blank())+
  scale_fill_viridis_d(name = "Pollinator")+ annotate("segment", x = 345, xend = 345, y = 0, yend = 310,
                                                      colour = "purple", size = 0.5, arrow = arrow())+
  annotate("segment", x = 348, xend = 348, y = 0, yend = 310,
           colour = "orange", size = 0.5, arrow = arrow())


frupol2

ggarrange(florpol2, frupol2, common.legend = TRUE,
          legend="bottom")
