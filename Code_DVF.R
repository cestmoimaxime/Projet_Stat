library(data.table)
library("curl")
library("R.utils")
library("tidyr")
# DVF_2022=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2022.csv")
# DVF_2021=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2021.csv")
# DVF_2020=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2020.csv")
# DVF_2019=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2019.csv")
# DVF_2018=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2018.csv")
# DVF_2017=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2017.csv")

#Récupération du csv dézippé grâce à fread à l'url correspondant
dt = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/full.csv.gz")
DVF_2017=dt
#On retire les valeurs NA de valeurs foncieres, aucune en réalité (même avec cette fonction l'histo veut pas être fait si je précise pas na.rm=TRUE)
DVF_2017 %>% drop_na(valeur_fonciere)

#histogramme des valeurs foncières selon le qurtile ou l'écart de notre choix
#hist(DVF_2017[DVF_2017$valeur_fonciere<quantile(DVF_2017$valeur_fonciere,0.03,na.rm=TRUE)]$valeur_fonciere)

#Ventes "raisonnables" sont comprises entre 20 000 et 5 millions d'euros
a <- nrow(DVF_2017[DVF_2017$valeur_fonciere<5000000 & DVF_2017$valeur_fonciere>20000,])
b <- nrow(DVF_2017)
ratio_ventes_raisonnables <- a/b
#82.7% des transactions (maisons,appartments, dépendance, local ou non précisé) ont coûté entre 20 000 € et 5 000 000 €


#hist(DVF_2017$valeur_fonciere,breaks=1000) illisibile

