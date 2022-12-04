library(data.table)
library("curl")
library("R.utils")
library("tidyr")
library("dplyr")
library("ggplot2")
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

# Trouver les transactions les plus communes ------------------------------

#6 transactions en 6 ans
liste_parcelle_2017=unique(DVF_2017$id_parcelle)

revendues_2017_2018 <- DVF_2018[DVF_2018$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2018=unique(revendues_2017_2018$id_parcelle)

revendues_2017_2018_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2017_2018,]
liste_parcelle_2017_2018_2019=unique(revendues_2017_2018_2019$id_parcelle)

revendues_2017_2018_2019_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2017_2018_2019,]
liste_parcelle_2017_2018_2019_2020=unique(revendues_2017_2018_2019_2020$id_parcelle)

revendues_2017_2018_2019_2020_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2017_2018_2019_2020,]
liste_parcelle_2017_2018_2019_2020_2021=unique(revendues_2017_2018_2019_2020_2021$id_parcelle)

revendues_2017_2018_2019_2020_2021_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2017_2018_2019_2020_2021,]


revendues_2017_2018_2019_2020_2021_2022_Maison_Vente <-revendues_2017_2018_2019_2020_2021_2022[revendues_2017_2018_2019_2020_2021_2022$nature_mutation=='Vente' & revendues_2017_2018_2019_2020_2021_2022$type_local=='Maison',] 
revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente <-revendues_2017_2018_2019_2020_2021_2022[revendues_2017_2018_2019_2020_2021_2022$nature_mutation=='Vente' & revendues_2017_2018_2019_2020_2021_2022$type_local=='Appartement',]


#Vérification que ces transactions apparaissent dans chaque année

dans_2017 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2017$id_parcelle),]
dans_2018 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2018$id_parcelle),]
dans_2019 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2019$id_parcelle),]
dans_2020 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2020$id_parcelle),]
dans_2021 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2021$id_parcelle),]
dans_2022 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2022$id_parcelle),]
#0 observation dans chacune des variables ci-dessus, aucune transaction qui apparait dans l'union n'apparrait pas dans une année,
#les transactions dans l'union sont donc toutes dans chacune des 6 années


#5 et seulement 5 fois la transaction en 6 ans
#5 parmi 6, 6 combinaisons possibles
# 2017 2018 2019 2020 2021
# 2017 2018 2019 2020 2022
# 2017 2018 2019 2021 2022
# 2017 2018 2020 2021 2022
# 2017 2019 2020 2021 2022
# 2018 2019 2020 2021 2022
liste_parcelle_2017=unique(DVF_2017$id_parcelle)
revendues_2017_2018 <- DVF_2018[DVF_2018$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2018=unique(revendues_2017_2018$id_parcelle)
revendues_2017_2018_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2017_2018,]
liste_parcelle_2017_2018_2019=unique(revendues_2017_2018_2019$id_parcelle)
revendues_2017_2018_2019_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2017_2018_2019,]
liste_parcelle_2017_2018_2019_2020=unique(revendues_2017_2018_2019_2020$id_parcelle)
pr_morceau_revendues_2017_2018_2019_2020_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2017_2018_2019_2020,]

liste_parcelle_2017=unique(DVF_2017$id_parcelle)
revendues_2017_2018 <- DVF_2018[DVF_2018$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2018=unique(revendues_2017_2018$id_parcelle)
revendues_2017_2018_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2017_2018,]
liste_parcelle_2017_2018_2019=unique(revendues_2017_2018_2019$id_parcelle)
revendues_2017_2018_2019_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2017_2018_2019,]
liste_parcelle_2017_2018_2019_2020=unique(revendues_2017_2018_2019_2020$id_parcelle)
pr_morceau_revendues_2017_2018_2019_2020_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2017_2018_2019_2020,]

liste_parcelle_2017=unique(DVF_2017$id_parcelle)
revendues_2017_2018 <- DVF_2018[DVF_2018$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2018=unique(revendues_2017_2018$id_parcelle)
revendues_2017_2018_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2017_2018,]
liste_parcelle_2017_2018_2019=unique(revendues_2017_2018_2019$id_parcelle)
revendues_2017_2018_2019_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2017_2018_2019,]
liste_parcelle_2017_2018_2019_2021=unique(revendues_2017_2018_2019_2021$id_parcelle)
pr_morceau_revendues_2017_2018_2019_2021_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2017_2018_2019_2021,]

liste_parcelle_2017=unique(DVF_2017$id_parcelle)
revendues_2017_2018 <- DVF_2018[DVF_2018$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2018=unique(revendues_2017_2018$id_parcelle)
revendues_2017_2018_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2017_2018,]
liste_parcelle_2017_2018_2020=unique(revendues_2017_2018_2020$id_parcelle)
revendues_2017_2018_2020_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2017_2018_2020,]
liste_parcelle_2017_2018_2020_2021=unique(revendues_2017_2018_2020_2021$id_parcelle)
pr_morceau_revendues_2017_2018_2020_2021_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2017_2018_2020_2021,]

liste_parcelle_2017=unique(DVF_2017$id_parcelle)
revendues_2017_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2017,]
liste_parcelle_2017_2019=unique(revendues_2017_2019$id_parcelle)
revendues_2017_2019_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2017_2019,]
liste_parcelle_2017_2019_2020=unique(revendues_2017_2019_2020$id_parcelle)
revendues_2017_2019_2020_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2017_2019_2020,]
liste_parcelle_2017_2019_2020_2021=unique(revendues_2017_2019_2020_2021$id_parcelle)
pr_morceau_revendues_2017_2019_2020_2021_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2017_2019_2020_2021,]



liste_parcelle_2018=unique(DVF_2018$id_parcelle)
revendues_2018_2019 <- DVF_2019[DVF_2019$id_parcelle %in% liste_parcelle_2018,]
liste_parcelle_2018_2019=unique(revendues_2018_2019$id_parcelle)
revendues_2018_2019_2020 <- DVF_2020[DVF_2020$id_parcelle %in% liste_parcelle_2018_2019,]
liste_parcelle_2018_2019_2020=unique(revendues_2018_2019_2020$id_parcelle)
revendues_2018_2019_2020_2021 <- DVF_2021[DVF_2021$id_parcelle %in% liste_parcelle_2018_2019_2020,]
liste_parcelle_2018_2019_2020_2021=unique(revendues_2018_2019_2020_2021$id_parcelle)
pr_morceau_revendues_2018_2019_2020_2021_2022 <- DVF_2022[DVF_2022$id_parcelle %in% liste_parcelle_2018_2019_2020_2021,]

union=rbind(pr_morceau_revendues_2018_2019_2020_2021_2022,
            pr_morceau_revendues_2017_2019_2020_2021_2022,
            pr_morceau_revendues_2017_2018_2019_2020_2021,
            pr_morceau_revendues_2017_2018_2019_2021_2022,
            pr_morceau_revendues_2017_2018_2020_2021_2022,
            pr_morceau_revendues_2017_2018_2019_2020_2022)

cinq_transactions=union[!union$id_parcelle %in% revendues_2017_2018_2019_2020_2021_2022$id_parcelle]


#Tentatives plus simple
#On unionne toutes les années en même temps
#liste de toutes les id_parcell
#on prend les id ou le nombre est exactement 4 

toutes_annees<- rbind(DVF_2017,DVF_2018,DVF_2019,DVF_2020,DVF_2021,DVF_2022)

quatre_transactions <- data.frame()
names(quatre_transactions) <- names(DVF_2017)
columns = names(DVF_2017) 
quatre_transactions = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(quatre_transactions) = columns
liste_parcelles <- unique(toutes_annees$id_parcelle)
l <- length(liste_parcelles)
i <- 0
for (k in liste_parcelles){
  i <- i+1
  print(paste0(i," sur ,",l))
  sous_df_avec_id_k <- toutes_annees[toutes_annees$id_parcelle==k]
  annee=substring(sous_df_avec_id_k$id_mutation,1,4)[1]
  if (length(unique(substring(sous_df_avec_id_k$id_mutation,1,4)))==2){
    quatre_transactions <- rbind(quatre_transactions,sous_df_avec_id_k[sous_df_avec_id_k$id_mutation %in% annee,])
  }
  
}


#Visualisation
Maisons_6_ventes <- ggplot(revendues_2017_2018_2019_2020_2021_2022_Maison_Vente)+geom_point(aes (x=revendues_2017_2018_2019_2020_2021_2022_Maison_Vente$longitude,y=revendues_2017_2018_2019_2020_2021_2022_Maison_Vente$latitude))+coord_cartesian(xlim = c(-4.5,9.5), ylim = c(41.5, 51))
Appartement_6_ventes <- ggplot(revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente)+geom_point(aes (x=revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente$longitude,y=revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente$latitude))+coord_map(projection="lambert",lat0=42,lat1=52,xlim = c(-4.5,9.5), ylim = c(41.5, 51))





