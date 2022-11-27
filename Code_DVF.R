library(data.table)
library("curl")
library("R.utils")
library("tidyr")
library("dplyr")
# DVF_2022=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2022.csv")
# DVF_2021=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2021.csv")
# DVF_2020=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2020.csv")
# DVF_2019=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2019.csv")
DVF_2018=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2018.csv")
DVF_2017=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2017.csv")

#Récupération du csv dézippé grâce à fread à l'url correspondant
DVF_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/full.csv.gz")
DVF_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/full.csv.gz")
DVF_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/full.csv.gz")
DVF_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/full.csv.gz")
DVF_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz")
DVF_2022= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2022/full.csv.gz")
#On retire les valeurs NA de valeurs foncieres, aucune en réalité (même avec cette fonction l'histo veut pas être fait si je précise pas na.rm=TRUE)
DVF_2017 %>% drop_na(valeur_fonciere)

#histogramme des valeurs foncières selon le qurtile ou l'écart de notre choix
#hist(DVF_2017[DVF_2017$valeur_fonciere<quantile(DVF_2017$valeur_fonciere,0.03,na.rm=TRUE)]$valeur_fonciere)

#Ventes "raisonnables" sont comprises entre 20 000 et 5 millions d'euros
raisonnables <- DVF_2017[DVF_2017$valeur_fonciere<5000000 & DVF_2017$valeur_fonciere>20000,]
a <- nrow(DVF_2017[DVF_2017$valeur_fonciere<5000000 & DVF_2017$valeur_fonciere>20000,])
b <- nrow(DVF_2017)
ratio_ventes_raisonnables <- a/b
#82.7% des transactions (maisons,appartments, dépendance, local ou non précisé) ont coûté entre 20 000 € et 5 000 000 €

Maisons_Raisonnables <- raisonnables[type_local=='Maison']
A_Raisonnables <- raisonnables[type_local=='Maison']

h <- "occurences"
nb_vente_type <- function(df){
  for (i in unique(df$type_local)){
    print(paste(i,sum(df$type_local==i),h))
  } 
}
nb_vente_type(raisonnables)
#hist(DVF_2017$valeur_fonciere,breaks=1000) illisibile
#hist(DVF_2017[DVF_2017$type_local=='Dépendance' & valeur_fonciere<500000]$valeur_fonciere)


DVF_2017_maison <- DVF_2017[DVF_2017$type_local=='Maison' & DVF_2017$nature_mutation=='Vente',]
DVF_2018_maison <- DVF_2018[DVF_2018$type_local=='Maison' & DVF_2018$nature_mutation=='Vente',]
DVF_2019_maison <- DVF_2019[DVF_2019$type_local=='Maison',]
DVF_2020_maison <- DVF_2020[DVF_2020$type_local=='Maison',]
DVF_2021_maison <- DVF_2021[DVF_2021$type_local=='Maison',]
DVF_2022_maison <- DVF_2022[DVF_2022$type_local=='Maison',]


#Trouver les maisons qui ont été vendus plusieurs fois en 6 ans, sur la base des coordonnées

DVF_same <- DVF_2017[DVF_2017_maison$longitude==DVF_2018_first_decile$longitude & DVF_2017_maison$latitude==DVF_2018_maison_first_decile$latitude,]

DVF_2018_first_decile=DVF_2018_maison[0:100000,]

test <- inner_join(DVF_2017_maison,DVF_2018_maison,by="id_parcelle")


#id_parcelles <- c()
# 
# for (i in DVF_2018_maison$id_parcelle){
#   if (is.na(i)){
#     next
#   }
#   result <- DVF_2017_maison[DVF_2017_maison$id_parcelle==i,]
#   if (!is.null(result)){
#     id_parcelles <- c(id_parcelles,result)
#   }
# }







