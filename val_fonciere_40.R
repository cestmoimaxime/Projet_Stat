# packages

install.packages("data.table")
library(data.table)
install.packages("curl")
library("curl")
install.packages("sf")
library(sf)
install.packages("ggplot2")
library(ggplot2)
install.packages("rgdal")
library(rgdal)
install.packages("sf")
library(sf)
library(tidyr)
library(dplyr)
install.packages("sjimisc")
library(sjimisc)

# récupération des données

#Récupération du csv dézippé grâce à fread à l'url correspondant
DVF_2021 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz")

DVF_2021 <- DVF_2021[DVF_2021$type_local=="Maison"]

DVF_2021 <- DVF_2021[DVF_2021$nature_mutation=="Vente"]

DVF_2021 <- DVF_2021[DVF_2021$code_departement=="40"]


DVF_2021_com <- DVF_2021 %>% 
  group_by(code_commune) %>% 
  summarise(mean_val=median(valeur_fonciere, na.rm=TRUE))


# recuperation du shape des landes


communes_40 <- sf::st_read('georef-france-commune-millesime.shp')


DVF_2021_com = merge(communes_40,DVF_2021_com, by.x="com_code", by.y="code_commune")

  
ggplot(data = DVF_2021_com) +
    geom_sf() +
    stat_sf_coordinates(aes(size = mean_val, 
                            color = arrdep_name))

DVF_2021_com_1 = DVF_2021_com[-284,]
g = ggplot(data =DVF_2021_com_1)
g+geom_sf(aes(fill=mean_val)) + scale_fill_gradient2(name = "Prix moyen de la valeur foncière", low = "blue", mid = "white", high = "red")+ ggtitle('La valeur foncière moyenne dans les Landes en 2021')+ scale_y_continuous(name = "Latitude")+scale_x_continuous(name = "Longitude")
