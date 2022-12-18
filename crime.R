### analyse de la bdd sur les crimes en france + corrélation avec la vf des biens immobiliers

# packages

install.packages("R.utils")
install.packages("data.table")
library(data.table)
install.packages("curl")
library("curl")
install.packages("ggplot2")
library(ggplot2)
install.packages("rgdal")
library(rgdal)
install.packages("sf")
library(sf)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# récupération des données

#crime_france = fread("https://www.data.gouv.fr/fr/datasets/r/fdf5afbf-ed3c-4c54-a4f0-3581c8a1eca4")
#crime_france = read.csv("Répartitions régionales 2021.csv",sep=";")

crime_test = fread("crime_donnees.gz")

s# récupération des shapes (france)

departements <-  sf::st_read('departements-20140306-100m.shp')
departements <- departements[1:96,]
departements = st_transform(departements,2154)


g=ggplot(data=departements)
g+geom_sf()


crime_2017 <- crime_test[crime_test$annee=='17']
crime_2018 <- crime_test[crime_test$annee=='18']
crime_2019 <- crime_test[crime_test$annee=='19']
crime_2020 <- crime_test[crime_test$annee=='20']
crime_2021 <- crime_test[crime_test$annee=='21']

crime_2017$tauxpourmille <- as.numeric(gsub(",", ".", crime_2017$tauxpourmille))
crime_2017$tauxpourmille <- round(crime_2017$tauxpourmille,2)

crime_2018$tauxpourmille <- as.numeric(gsub(",", ".", crime_2018$tauxpourmille))
crime_2018$tauxpourmille <- round(crime_2018$tauxpourmille,2)

crime_2019$tauxpourmille <- as.numeric(gsub(",", ".", crime_2019$tauxpourmille))
crime_2019$tauxpourmille <- round(crime_2019$tauxpourmille,2)

crime_2020$tauxpourmille <- as.numeric(gsub(",", ".", crime_2020$tauxpourmille))
crime_2020$tauxpourmille <- round(crime_2020$tauxpourmille,2)

crime_2021$tauxpourmille <- as.numeric(gsub(",", ".", crime_2021$tauxpourmille))
crime_2021$tauxpourmille <- round(crime_2021$tauxpourmille,2)


crime_2017 <- crime_2017 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))
crime_2018 <- crime_2018 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))
crime_2019 <- crime_2019 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))
crime_2020 <- crime_2020 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))
crime_2021 <- crime_2021 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))

# NOMBRE DE CRIME PAR DEPARTEMENTS + NOMBRE DE VENTE PAR DEPARTEMENTS

#Récupération du csv dézippé grâce à fread à l'url correspondant
DVF_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/full.csv.gz")
DVF_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/full.csv.gz")
DVF_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/full.csv.gz")
DVF_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/full.csv.gz")
DVF_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz")

DVF_2017 <- DVF_2017[DVF_2017$type_local=="Maison"]
DVF_2018 <- DVF_2018[DVF_2018$type_local=="Maison"]
DVF_2019 <- DVF_2019[DVF_2019$type_local=="Maison"]
DVF_2020 <- DVF_2020[DVF_2020$type_local=="Maison"]
DVF_2021 <- DVF_2021[DVF_2021$type_local=="Maison"]

DVF_2017 <- DVF_2017[DVF_2017$nature_mutation=="Vente"]
DVF_2018 <- DVF_2018[DVF_2018$nature_mutation=="Vente"]
DVF_2019 <- DVF_2019[DVF_2019$nature_mutation=="Vente"]
DVF_2020 <- DVF_2020[DVF_2020$nature_mutation=="Vente"]
DVF_2021 <- DVF_2021[DVF_2021$nature_mutation=="Vente"]

DVF_2017 <- DVF_2017 %>% 
  group_by(code_departement) %>% 
  count(nature_mutation)
DVF_2018 <- DVF_2018 %>% 
  group_by(code_departement) %>% 
  count(nature_mutation)
DVF_2019 <- DVF_2019 %>% 
  group_by(code_departement) %>% 
  count(nature_mutation)
DVF_2020 <- DVF_2020 %>% 
  group_by(code_departement) %>% 
  count(nature_mutation)
DVF_2021 <- DVF_2021 %>% 
  group_by(code_departement) %>% 
  count(nature_mutation)

DVF_2017 <- merge(crime_2017, DVF_2017, by.x='Code.département', by.y='code_departement')
DVF_2018 <- merge(crime_2018, DVF_2018, by.x='Code.département', by.y='code_departement')
DVF_2019 <- merge(crime_2019, DVF_2019, by.x='Code.département', by.y='code_departement')
DVF_2020 <- merge(crime_2020, DVF_2020, by.x='Code.département', by.y='code_departement')
DVF_2021 <- merge(crime_2021, DVF_2021, by.x='Code.département', by.y='code_departement')

DVF_2017 <- merge(departements, DVF_2017, by.x='code_insee', by.y='Code.département')
DVF_2018 <- merge(departements, DVF_2018, by.x='code_insee', by.y='Code.département')
DVF_2019 <- merge(departements, DVF_2019, by.x='code_insee', by.y='Code.département')
DVF_2020 <- merge(departements, DVF_2020, by.x='code_insee', by.y='Code.département')
DVF_2021 <- merge(departements, DVF_2021, by.x='code_insee', by.y='Code.département')

ggplot(data = DVF_2017) + geom_sf(aes(fill=n)) + stat_sf_coordinates(aes(size = tauxpourmille, color=NULL)) + scale_fill_gradient2(name = "Nombre de ventes", low = "darkblue", mid = "white", high = "darkred")


## selon la valeur fonciere

DVF_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz")
DVF_2021 <- DVF_2021[DVF_2021$type_local=="Maison"]
DVF_2021 <- DVF_2021[DVF_2021$nature_mutation=="Vente"]

crime_2021 <- crime_test[crime_test$annee=='21']
crime_2021$tauxpourmille <- as.numeric(gsub(",", ".", crime_2021$tauxpourmille))
crime_2021$tauxpourmille <- round(crime_2021$tauxpourmille,2)

crime_2021 <- crime_2021 %>% 
  group_by(Code.département) %>% 
  summarise(tauxpourmille = sum(tauxpourmille))

DVF_2021 <- DVF_2021 %>% 
  group_by(code_departement) %>% 
  summarise(median_val=median(valeur_fonciere,na.rm=TRUE))


DVF_2021 <- merge(crime_2021, DVF_2021, by.x='Code.département', by.y='code_departement')
DVF_2021 <- merge(departements, DVF_2021, by.x='code_insee', by.y='Code.département')


DVF_2021_test=DVF_2021[-73,]
DVF_2021_test=DVF_2021_test[-6,]
g=ggplot(data=DVF_2021_test)                             
g+geom_sf(aes(fill=median_val)) + stat_sf_coordinates(aes(size=tauxpourmille)) + scale_fill_gradient2(name = "Prix moyen de la valeur foncière", low = "blue", mid = "white", high = "red")+ ggtitle('Le taux pour mille de criminalité en France en 2021')+ scale_y_continuous(name = "Latitude")+scale_x_continuous(name = "Longitude")
