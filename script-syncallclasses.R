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
                  "synchrony",
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
#synch
#fruit
fruit_zoo3 <- fruit_zoo %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_zoochory <- master %>%
  filter(Zoocoria == 1)

fruit_zoo4 <- master_zoochory %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

zoochory <- (fruit_zoo3$ind/fruit_zoo4$ind)


fruit_auto3 <- fruit_auto %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_autochory <- master %>%
  filter(Autocoria == 1)

fruit_auto4 <- master_autochory %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

autochory <- (fruit_auto3$ind/fruit_auto4$ind)


fruit_anemo3 <- fruit_anemo %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

class(fruit_anemo3$month)
class(fruit_anemo3$ind)

fruit_anemo3[nrow(fruit_anemo3) + 1,] <- list("12", 0)

master_anemochory <- master %>%
  filter(Anemocoria == 1)

fruit_anemo4 <- master_anemochory %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

anemochory <- (fruit_anemo3$ind/fruit_anemo4$ind)

#join all into one dataframe
sync_fruit <- data.frame(zoochory, autochory, anemochory)

community.sync(sync_fruit, nrands=100)

#flower
flower_zoo3 <- flower_zoo %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_zoophilia <- master %>%
  filter(Zoofilia == 1)

flower_zoo4 <- master_zoophilia %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

zoophilia <- (flower_zoo3$ind/flower_zoo4$ind)


flower_anemo3 <- fruit_anemo %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

flower_anemo3[nrow(flower_anemo3) + 1,] <- list("12", 0)

master_anemophilia <- master %>%
  filter(Anemofilia == 1)

flower_anemo4 <- master_anemophilia %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

anemophilia <- (flower_anemo3$ind/flower_anemo4$ind)  

#join all into one dataframe
sync_flower <- data.frame(zoophilia, anemophilia)

community.sync(sync_flower, nrands=100)

#leaffall
leaffall_sempreverde3 <- leaffall_sempreverde %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_lfsempreverde <- master %>%
  filter(Sempreverde == 1)

leaffall_sempreverde4 <- master_lfsempreverde %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

lfsempreverde <- (leaffall_sempreverde3$ind/leaffall_sempreverde4$ind)


leaffall_semidecidua3 <- leaffall_semidecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_lfsemidecidua <- master %>%
  filter(Semidecidua == 1)

leaffall_semidecidua4 <- master_lfsemidecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

lfsemidecidua <- (leaffall_semidecidua3$ind/leaffall_semidecidua4$ind)


leaffall_decidua3 <- leaffall_decidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

leaffall_decidua3[nrow(leaffall_decidua3) + 1,] <- list("12", 0)

master_lfdecidua <- master %>%
  filter(Decidua == 1)

leaffall_decidua4 <- master_lfdecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

lfdecidua <- (leaffall_decidua3$ind/leaffall_decidua4$ind)  

#join all into one dataframe
sync_leaffall <- data.frame(lfsempreverde, lfsemidecidua, lfdecidua)

community.sync(sync_leaffall, nrands=100)

#sprouting
sprouting_sempreverde3 <- sprouting_sempreverde %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_ssempreverde <- master %>%
  filter(Sempreverde == 1)

sprouting_sempreverde4 <- master_ssempreverde %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

ssempreverde <- (sprouting_sempreverde3$ind/sprouting_sempreverde4$ind)


sprouting_semidecidua3 <- sprouting_semidecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

master_ssemidecidua <- master %>%
  filter(Semidecidua == 1)

sprouting_semidecidua4 <- master_ssemidecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

ssemidecidua <- (sprouting_semidecidua3$ind/sprouting_semidecidua4$ind)


sprouting_decidua3 <- sprouting_decidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

sprouting_decidua3[nrow(sprouting_decidua3) + 1,] <- list("12", 0)

master_sdecidua <- master %>%
  filter(Decidua == 1)

sprouting_decidua4 <- master_sdecidua %>%
  group_by(month) %>%
  summarise(ind = n_distinct(Tag))

sdecidua <- (sprouting_decidua3$ind/sprouting_decidua4$ind)  

#join all into one dataframe
sync_sprouting <- data.frame(ssempreverde, ssemidecidua, sdecidua)

community.sync(sync_sprouting, nrands=100)