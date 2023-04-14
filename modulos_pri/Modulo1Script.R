### Script para Aula 1 - Curso de Introducao ao R ###
##################### --SETS--  #####################
rm(list=ls())
getwd()
setwd("/Users/priscila/Desktop/2022.1/Pheno.22/Aula_1")
load('Modulo1.RData')
#load(file.choose())
#save(laranjas, exemplo, file="Modulo1.RData")

library(ggplot2)
laranbox = 
  ggplot(laranjas, aes(x = Tratamento, y = Biomassa)) +
  geom_boxplot()
laranbox

laranbox = 
  ggplot(laranjas, aes(x = as.factor(Tratamento), y = Biomassa, fill= as.factor(Tratamento))) +
  geom_boxplot(width=0.2,position=position_dodge(1))+   
  scale_fill_manual(values=c("black", "darkgrey", "white"))+
  ylab("")+xlab("Tratamentos")+ 
  theme_classic()+theme(text=element_text(size=20),legend.position = "none")
laranbox

# ylim(c(0,150))+
