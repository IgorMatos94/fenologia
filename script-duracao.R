####################################
####fruits
fruit_zoo <- fruit %>%
  filter(Zoocoria == 1)

fruit_zoo1 <- fruit_zoo %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

fruit_zoo2 <- fruit_zoo %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

#duração por espécie
mean(fruit_zoo1$months)

#SD
sd(fruit_zoo1$months)

#duração por indivíduo
mean(fruit_zoo2$months)


fruit_anemo <- fruit %>%
  filter(Anemocoria == 1)

fruit_anemo1 <- fruit_anemo %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

fruit_anemo2 <- fruit_anemo %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(fruit_anemo1$months)
sd(fruit_anemo1$months)
mean(fruit_anemo2$months)


fruit_auto <- fruit %>%
  filter(Autocoria == 1)

fruit_auto1 <- fruit_auto %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

fruit_auto2 <- fruit_auto %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(fruit_auto1$months)
sd(fruit_auto1$months)
mean(fruit_auto2$months)



#flower
flower_zoo <- flower %>%
  filter(Zoofilia == 1)

flower_zoo1 <- flower_zoo %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

flower_zoo2 <- flower_zoo %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(flower_zoo1$months)
sd(flower_zoo1$months)
mean(flower_zoo2$months)


flower_anemo <- flower %>%
  filter(Anemofilia == 1)

flower_anemo1 <- flower_anemo %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

flower_anemo2 <- flower_anemo %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(flower_anemo1$months)
sd(flower_anemo1$months)
mean(flower_anemo2$months)  

#leaffall
leaffall_sempreverde <- leaffall %>%
  filter(Sempreverde == 1)

leaffall_sempreverde1 <- leaffall_sempreverde %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

leaffall_sempreverde2 <- leaffall_sempreverde %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(leaffall_sempreverde1$months)
sd(leaffall_sempreverde1$months)
mean(leaffall_sempreverde2$months)  


leaffall_semidecidua <- leaffall %>%
  filter(Semidecidua == 1)

leaffall_semidecidua1 <- leaffall_semidecidua%>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

leaffall_semidecidua2 <- leaffall_semidecidua %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(leaffall_semidecidua1$months)
sd(leaffall_semidecidua1$months)
mean(leaffall_semidecidua2$months)  


leaffall_decidua <- leaffall %>%
  filter(Decidua == 1)

leaffall_decidua1 <- leaffall_decidua %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

leaffall_decidua2 <- leaffall_decidua %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(leaffall_decidua1$months)
sd(leaffall_decidua1$months)
mean(leaffall_decidua2$months)  

#  sprouting
sprouting_sempreverde <- sprouting %>%
  filter(Sempreverde == 1)

sprouting_sempreverde1 <- sprouting_sempreverde %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

sprouting_sempreverde2 <- sprouting_sempreverde %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(sprouting_sempreverde1$months)
sd(sprouting_sempreverde1$months)
mean(sprouting_sempreverde2$months)  


sprouting_semidecidua <- leaffall %>%
  filter(Semidecidua == 1)

sprouting_semidecidua1 <- sprouting_semidecidua %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

sprouting_semidecidua2 <- sprouting_semidecidua %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(sprouting_semidecidua1$months)
sd(sprouting_semidecidua1$months)
mean(sprouting_semidecidua2$months)  


sprouting_decidua <- leaffall %>%
  filter(Decidua == 1)

sprouting_decidua1 <- sprouting_decidua %>%
  group_by(Species) %>%
  summarise(months = n_distinct(month))

sprouting_decidua2 <- sprouting_decidua %>%
  group_by(Tag) %>%
  summarise(months = n_distinct(month))

mean(sprouting_decidua1$months)
sd(sprouting_decidua1$months)
mean(sprouting_decidua2$months)  

#filter master spreadsheet to only include the species that have more than 3
#individuals

gr8r_3 <- master %>% 
  group_by(Species) %>% 
  filter(n_distinct(Tag) >= 3)
