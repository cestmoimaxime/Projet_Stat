library("data.table")
library("curl")
library("R.utils")
library("tidyr")
library("dplyr")
library("ggplot2")
library("arrangements")
library("gtools")
library("combinat")
library("rgdal")
library("randomForest")
 DVF_2022=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2022.csv")
 DVF_2021=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2021.csv")
 DVF_2020=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2020.csv")
 DVF_2019=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2019.csv")
DVF_2018=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2018.csv")
DVF_2017=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2017.csv")

#Récupération du csv dézippé grâce à fread à l'url correspondant
DVF_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/full.csv.gz")
DVF_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/full.csv.gz")
DVF_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/full.csv.gz")
DVF_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/full.csv.gz")
DVF_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz")
DVF_2022= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2022/full.csv.gz")

#Récupération des shape importants
departement <- readOGR(dsn="C:/Users/Maxime/Documents/Fichier SIG/France/Departement_fr",layer="departements-20180101")
poi <- readOGR(dsn="C:/Users/Maxime/Documents/R",layer="vrai_hebergements-classes")
save(poi,file="poi.RData")

poi_vrai_non_hotel=poi[poi@data$typologie_e %like% "HÔTEL",]
df <- poi_non_hotel@data
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

save(revendues_2017_2018_2019_2020_2021_2022,file="biens_vendus_six_fois.RData")

#Vérification que ces transactions apparaissent dans chaque année

dans_2017 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2017$id_parcelle),]
dans_2018 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2018$id_parcelle),]
dans_2019 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2019$id_parcelle),]
dans_2020 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2020$id_parcelle),]
dans_2021 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2021$id_parcelle),]
dans_2022 <- revendues_2017_2018_2019_2020_2021_2022[!(revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% DVF_2022$id_parcelle),]
#0 observation dans chacune des variables ci-dessus, aucune transaction qui apparait dans l'union n'apparrait pas dans une année,
#les transactions dans l'union sont donc toutes dans chacune des 6 années


#Tentatives plus simple par fonction

assign("2017", DVF_2017)
assign("2018", DVF_2018)
assign("2019", DVF_2019)
assign("2020", DVF_2020)
assign("2021", DVF_2021)
assign("2022", DVF_2022)

liste_annees <- c("2017","2018","2019","2020","2021","2022")
nombre_transactions <- function(x){
  columns = names(DVF_2017)
  biens_immo = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(biens_immo) = columns
  permu <- as.data.frame(combn(liste_annees, x))
  for (k in 1:length(permu)){
    permu_actuelle <- permu[,k]
    for (i in permu_actuelle){
      annee_current=get(i)
      print(i)
      if (i==permu_actuelle[1]){
        liste_parcelle <- unique(annee_current$id_parcelle)
        #adresse_complete=unique(paste0(annee_current$adresse_numero,annee_current$adresse_nom_voie,annee_current$code_commune))
        
      }
      else{
        #revendue <- annee_current[paste0(annee_current$adresse_numero,annee_current$adresse_nom_voie,annee_current$code_commune) %in% adresse_complete,]
        #adresse_complete <- unique(paste0(revendue$adresse_numero,revendue$adresse_nom_voie,revendue$code_commune))
        revendue <- annee_current[annee_current$id_parcelle %in% liste_parcelle,]
        liste_parcelle <- unique(revendue$id_parcelle)
        
        
        }
    }
    biens_immo <- rbind(biens_immo,revendue)
    print('un de plus')
  }
  #finit_transaction=biens_immo[!biens_immo$id_parcelle %in% test_six_biens$id_parcelle,]
  return(biens_immo)
}
test_encore_vente_maison=test_encore[test_encore$nature_mutation=='Vente' & test_encore$type_local=='Maison',]

adresse_complete=unique(paste0(annee_current$adresse_numero,annee_current$adresse_nom_voie,annee_current$code_commune))


save(test_cinq_biens,file="biens_vendus_cinq_fois.RData")
save(revendues_2017_2018_2019_2020_2021_2022,file="biens_vendus_six_fois.RData")



liste_biens_department <- as.data.frame(table(revendues_2017_2018_2019_2020_2021_2022$code_departement))

departements_biens <- merge(departement,liste_biens_department,by.x="code_insee",by.y="Var1")
#Visualisation


Maisons_6_ventes <- ggplot(revendues_2017_2018_2019_2020_2021_2022_Maison_Vente)+geom_point(aes (x=revendues_2017_2018_2019_2020_2021_2022_Maison_Vente$longitude,y=revendues_2017_2018_2019_2020_2021_2022_Maison_Vente$latitude))+coord_cartesian(xlim = c(-4.5,9.5), ylim = c(41.5, 51))
Appartement_6_ventes <- ggplot(revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente)+geom_point(aes (x=revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente$longitude,y=revendues_2017_2018_2019_2020_2021_2022_Appartement_Vente$latitude))+coord_map(projection="lambert",lat0=42,lat1=52,xlim = c(-4.5,9.5), ylim = c(41.5, 51))
Maisons_5_ventes <- ggplot(cinq_transactions_Maisons_Vente)+geom_point(aes (x=cinq_transactions_Maisons_Vente$longitude,y=cinq_transactions_Maisons_Vente$latitude))+coord_map(projection="lambert",lat0=42,lat1=52,xlim = c(-4.5,9.5), ylim = c(41.5, 51))
Appartement_5_ventes <- ggplot(cinq_transactions_Appartement_Vente)+geom_point(aes (x=cinq_transactions_Appartement_Vente$longitude,y=cinq_transactions_Appartement_Vente$latitude))+coord_map(projection="lambert",lat0=42,lat1=52,xlim = c(-4.5,9.5), ylim = c(41.5, 51))


revendues_2017_2018_2019_2020_2021_2022 <- revendues_2017_2018_2019_2020_2021_2022[revendues_2017_2018_2019_2020_2021_2022$nature_mutation=='Vente' & revendues_2017_2018_2019_2020_2021_2022$type_local=='Maison',]
Maisons_5_ventes <- ggplot(revendues_2017_2018_2019_2020_2021_2022)+geom_point(aes (x=revendues_2017_2018_2019_2020_2021_2022$longitude,y=revendues_2017_2018_2019_2020_2021_2022$latitude))+coord_map(projection="lambert",lat0=42,lat1=52,xlim = c(-4.5,9.5), ylim = c(41.5, 51))

library("sf")

departements <- read_sf("C:/Users/Maxime/Documents/Fichier SIG/France/Departement_fr/departements-20180101.shp")
departements_biens <- departements_biens[!departements_biens$nom %in% c("Mayotte", "La Réunion", "Guadeloupe", "Martinique", "Guyane"),]
u <- ggplot(departements) + geom_sf(aes(fill = "surf_km2"))

save(departements,file="departements.RData")

dans_2017 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2017$id_parcelle),]
dans_2018 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2018$id_parcelle),]
dans_2019 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2019$id_parcelle),]
dans_2020 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2020$id_parcelle),]
dans_2021 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2021$id_parcelle),]
dans_2022 <- test_5_ventes[!(test_5_ventes$id_parcelle %in% DVF_2022$id_parcelle),]

shapefile_df <- fortify(departements_biens)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'Freq', size = .2)

print(map) 

maison_vente <- ggplot(test_encore_vente_maison)+geom_point(aes (x=longitude,y=latitude))+coord_cartesian(xlim = c(-4.5,9.5), ylim = c(41.5, 51))


#Tentative de récupérer les maisons vendues sans les campings

liste_toutes_parcelles=unique(revendues_2017_2018_2019_2020_2021_2022_Maison_Vente$id_parcelle)

DVF_2017_6_fois=DVF_2017[DVF_2017$id_parcelle %in% liste_toutes_parcelles ,]
DVF_2018_6_fois=DVF_2018[DVF_2018$id_parcelle %in% liste_toutes_parcelles ,]
DVF_2019_6_fois=DVF_2019[DVF_2019$id_parcelle %in% liste_toutes_parcelles ,]
DVF_2020_6_fois=DVF_2020[DVF_2020$id_parcelle %in% liste_toutes_parcelles ,]
DVF_2021_6_fois=DVF_2021[DVF_2021$id_parcelle %in% liste_toutes_parcelles ,]
DVF_2022_6_fois=DVF_2022[DVF_2022$id_parcelle %in% liste_toutes_parcelles,]


toutes_ventes_6_biens=rbind(DVF_2017_6_fois,
                            DVF_2018_6_fois,
                            DVF_2019_6_fois,
                            DVF_2020_6_fois,
                            DVF_2021_6_fois,
                            DVF_2022_6_fois)










DVF_2017_Maison_Vendues_6_fois$id_parcelle=='593503550B4716'



liste_parcelle_6_fois=table_nombre_variable[table_nombre_variable$Freq==6,]$Var1

biens_vendus_uniquement_6_fois <- revendues_2017_2018_2019_2020_2021_2022[revendues_2017_2018_2019_2020_2021_2022$id_parcelle %in% liste_toutes_parcelles,]

Maisons_6_ventes <- ggplot(biens_vendus_uniquement_6_fois)+geom_point(aes (x=longitude,y=latitude))+coord_cartesian(xlim = c(-4.5,9.5), ylim = c(41.5, 51))

#Test d'apprentissage
# Champs utiles pour l'apprentissage:
# date_mutation,
# numero_disposition
# valeur_foncière,
# code_postal
# code_commune
# code_departement
# id_parcelle
# numero_volume
# lot_1_numero
# lot_1_surf
# lot_2_numero
# lot_2_surf
# lot_3_numero
# lot_3_surf
# lot_4_numero
# lot_4_surf
# lot_5_numero
# lot_5_surf
# nombre_lot
# type_local
# surface_reel_bati
# nombre_piece_principale
# natrure_culture
# nature_culture_special
# surface terrain

donnees_test_arrangees <- donnees_test[,c("valeur_fonciere",
                 "lot1_surface_carrez",
                 "lot2_surface_carrez",
                 "lot3_surface_carrez",
                 "lot4_surface_carrez",
                 "lot5_surface_carrez",
                 "type_local",
                 "surface_reelle_bati",
                 "nombre_pieces_principales",
                 "nature_culture",
                 "nature_culture_speciale",
                 "surface_terrain",
                 "longitude",
                 "latitude",
                 "n")]

col_names <- names(donnees_test1)
donnees_test1[,col_names] <- lapply(donnees_test1[,col_names] , factor)


save(donnees_test,file='training_sample.RData')
save(donnees_apprentissage,file="verif_sample.RData")
donnees_test=DVF_2017[DVF_2017$type_local=='',]
donnees_apprentissage=DVF_2017[DVF_2017$type_local!='',]

#Trouver ceux avec le moins d'occurence à virer car trop de factor (88) je dois réduire à moins de 53
liste_type_culture <- as.data.frame(table(donnees_test_arrangees$nature_culture_speciale))
liste_nature_a_prendre <- liste_type_culture[liste_type_culture$Freq>5,]$Var1

donnees_test_arrangees <- donnees_test_arrangees[nature_culture_speciale %in% liste_nature_a_prendre,]

#après avoir essayer sans succès d'enlever le trop lein de nombre de nature_culture_special je vais juste enlever cette colonne
donnees_test_arrangees_1 <- donnees_test_arrangees[,-c("nature_culture_speciale")]
#ça ne change pas le souci
#je suprime toutes les lignes qui pèsent lourd pour rien: trop de valeur NA
#je créer des dummy variable pour ne pas traiter les champs avec des valeurs quasi tout le temps nul différezment

donnees_test_arrangees$surface_5_carrez <- ifelse(!is.na(donnees_test_arrangees$lot5_surface_carrez), 1, 0)
#en réalité celles qui ont pas de valeur de surface ont plus de 1 lot

donnees_test_arrangees <- donnees_test_arrangees[,-c("lot1_surface_carrez","lot2_surface_carrez","lot3_surface_carrez","lot4_surface_carrez","lot5_surface_carrez")]

data_test_na <- data_test_na[!is.na(surface_reelle_bati),]


save(data_test_na,file='table_apprentissage.RData')

data_test_na$type_local <- factor(data_test_na$type_local)


#Je me suis rendu compte que la surface reelle et le nomnbre de pièce n'apparait pas dans la base non étiquetée, je vais donc la supprimer du jeu de test
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[,-c("surface_reelle_bati","nombre_pieces_principales")]

#Je supprime les lignes qui ont des valeurs NA pour les autres champs de test et apprentissage:
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[!is.na(valeur_fonciere),]
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[!is.na(nature_culture),]
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[!is.na(surface_terrain),]
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[!is.na(latitude),]

#Transforme les character en factor
donnees_apprentissage_arrangees$type_local <- factor(donnees_apprentissage_arrangees$type_local)
donnees_apprentissage_arrangees$nature_culture <- factor(donnees_apprentissage_arrangees$nature_culture)
donnees_apprentissage_arrangees$nature_culture_speciale <- factor(donnees_apprentissage_arrangees$nature_culture_speciale)


model=randomForest(type_local~.,data=donnees_apprentissage_arrangees_bis,ntree=300)


donnees_test_arrangees$nature_culture_speciale <- as.factor(donnees_test_arrangees$nature_culture_speciale)
#pour plus de simplicité on enlève le champs nature culture spécial qui a pkus de 53 valeurs
donnees_apprentissage_arrangees <- donnees_apprentissage_arrangees[,-c("nature_culture_speciale")]

#après test HORRIBLE je vais aussi supprimer les coordonnées, ça sert à rien
save(model,file="model_NUL.RData")

donnees_apprentissage_arrangees_bis <- donnees_apprentissage_arrangees[,-c("latitude","longitude")]

save(donnees_test_arrangees,file="data_test.RData")

library(caret) 
library (e1071)
logit.fit <- train(type_local ~ ., data = donnees_test1,method="glm")

#Tri des données : suppression des doublons (en faisant la somme des surfaces de terrain) et garde que les maisons
DVF_2017 <- DVF_2017[DVF_2017$type_local=="Maison"]
DVF_2017 <- DVF_2017 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))
DVF_2018 <- DVF_2018[DVF_2018$type_local=="Maison"]
DVF_2018 <- DVF_2018 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))
DVF_2019 <- DVF_2019[DVF_2019$type_local=="Maison"]
DVF_2019 <- DVF_2019 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))
DVF_2020 <- DVF_2020[DVF_2020$type_local=="Maison"]
DVF_2020 <- DVF_2020 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))
DVF_2021 <- DVF_2021[DVF_2021$type_local=="Maison"]
DVF_2021 <- DVF_2021 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))
DVF_2022 <- DVF_2022[DVF_2022$type_local=="Maison"]
DVF_2022 <- DVF_2022 %>% group_by(id_mutation) %>% 
  summarise_all(funs(if(is.numeric(.))sum(.) else str_c(unique(.),collapse="_")))



nb_surfaces_non_renseignes <- nrow(donnees_apprentissage[surface_terrain==1,])
#[1] 2431
nb_surfaces_non_renseignes_vide <- nrow(donnees_apprentissage[is.na(surface_terrain),])
#[2] 77315

donnees_apprentissage_bien <- donnees_apprentissage[!is.na(surface_terrain) & surface_terrain!=1,]

plot_mosaic <- mosaicplot(table(DVF_2017$type_local,DVF_2017$nature_mutation),xlab="Type du local",ylab="Nature de la mutation",main="Représentation de la table de contingence entre le type de local et la nature de la mutation",color=TRUE)

png("plot_mosaic.png") 
save(data_test_na,file="apprentissage_best.RData")


#Création du champs qui compte le nombre de lignes par mutation

df_decompte <- DVF_2017 %>% count(id_parcelle, date_mutation)

#onmerge ensuite avec la base DVF pour rajouter le champs
DVF_2017 <- merge(DVF_2017,df_decompte,by.x=c("id_parcelle","date_mutation"),by.y=c("id_parcelle","date_mutation"))
                     
#Modif chaimaa _calcul ACP et boxplots 
                     
library("data.table")
library("curl")
library("R.utils")
library("tidyr")
library("dplyr")
library("ggplot2")
library("arrangements")
library("gtools")
library("combinat")
library("rgdal")
library("readr")
library("tidyr")

#departement Landes 
DVF_Landes_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/departements/40.csv.gz")
DVF_Landes_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/departements/40.csv.gz")
DVF_Landes_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/departements/40.csv.gz")
DVF_Landes_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/departements/40.csv.gz")
DVF_Landes_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/departements/40.csv.gz")
DVF_Landes_2022= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2022/departements/40.csv.gz")


#departement Meuse

DVF_Meuse_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/departements/55.csv.gz")
DVF_Meuse_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/departements/55.csv.gz")
DVF_Meuse_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/departements/55.csv.gz")
DVF_Meuse_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/departements/55.csv.gz")
DVF_Meuse_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/departements/55.csv.gz")
DVF_Meuse_2022= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2022/departements/55.csv.gz")


#departement Loire Atlentique

DVF_LoireAtl_2017 = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/departements/44.csv.gz")
DVF_LoireAtl_2018= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2018/departements/44.csv.gz")
DVF_LoireAtl_2019= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2019/departements/44.csv.gz")
DVF_LoireAtl_2020= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2020/departements/44.csv.gz")
DVF_LoireAtl_2021= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2021/departements/44.csv.gz")
DVF_LoireAtl_2022= fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2022/departements/44.csv.gz")


departements<- st_read("departements-20140306-100m.shp")




Filtre<-function(dvfdate){
  
  dvfdate =  dvfdate %>%  filter(type_local=="Maison" & nature_mutation== "Vente" )
  
  
}
#Filtre sur vente et maison 

DVF_Landes_2017<-Filtre(DVF_Landes_2017)
DVF_Landes_2018<-Filtre(DVF_Landes_2018)
DVF_Landes_2019<-Filtre(DVF_Landes_2019)
DVF_Landes_2020<-Filtre(DVF_Landes_2020)
DVF_Landes_2021<-Filtre(DVF_Landes_2021)
DVF_Landes_2022<-Filtre(DVF_Landes_2022)



#Filtre sur vente et maison 

DVF_Meuse_2017<-Filtre(DVF_Meuse_2017)
DVF_Meuse_2018<-Filtre(DVF_Meuse_2018)
DVF_Meuse_2019<-Filtre(DVF_Meuse_2019)
DVF_Meuse_2020<-Filtre(DVF_Meuse_2020)
DVF_Meuse_2021<-Filtre(DVF_Meuse_2021)
DVF_Meuse_2022<-Filtre(DVF_Meuse_2022)

#Filtre sur vente et maison 

DVF_LoireAtl_2017<-Filtre(DVF_LoireAtl_2017)
DVF_LoireAtl_2018<-Filtre(DVF_LoireAtl_2018)
DVF_LoireAtl_2019<-Filtre(DVF_LoireAtl_2019)
DVF_LoireAtl_2020<-Filtre(DVF_LoireAtl_2020)
DVF_LoireAtl_2021<-Filtre(DVF_LoireAtl_2021)
DVF_LoireAtl_2022<-Filtre(DVF_LoireAtl_2022)


library("factoextra")
library("ade4")




#visualiser les valeurs foncieres en 2017 pour la vente des maisons 

ValFonciere<-function(dvfdate){
  ValFonc <-dvfdate[dvfdate$valeur_fonciere<1000000 & dvfdate$valeur_fonciere>0,]
  
  HistFonciere <-  hist(ValFonc$valeur_fonciere, xlab="Valeur Fonciere",main="Histogramme de valeurs foncieres")
  return(HistFonciere)
}


ValFonciere(DVF_Landes_2017)
ValFonciere(DVF_Landes_2018)
ValFonciere(DVF_Landes_2019)
ValFonciere(DVF_Landes_2020)
ValFonciere(DVF_Landes_2021)
ValFonciere(DVF_Landes_2022)


#ACP 2017 Landes 

var_quantitativeLandes_2017 <-DVF_Landes_2017[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]

newData_Landes_2017<-var_quantitativeLandes_2017 %>% drop_na()


ACP_Landes_2017 <- prcomp(newData_Landes_2017, scale = TRUE)
fviz_eig(ACP_Landes_2017,addlabels = TRUE)




#ACP 2017 Meuse

var_quantitative_Meuse_2017 <- DVF_Meuse_2017[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]

newData_Meuse_2017 <-var_quantitative_Meuse_2017 %>% drop_na()


ACP_Meuse_2017 <- prcomp(newData_Meuse_2017, scale = TRUE)
fviz_eig(ACP_Meuse_2017,addlabels = TRUE)

#center = True ==> permet de centrer les données (soustraction de la moyenne)
#scale = True ==> permet de normer les données ==> center et scale = T donnent des données centrées-réduites.
#ACP 2017 

var_quantitative_LoireAtl_2017 <- DVF_LoireAtl_2017[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]

newData_LoireAtl_2017 <-var_quantitative_LoireAtl_2017 %>% drop_na()


ACP_LoireAtl_2017 <- prcomp(newData_LoireAtl_2017, scale = TRUE)
fviz_eig(ACP_LoireAtl_2017,addlabels = TRUE,barfill="gray",barcolor="red")

var <- factoextra::get_pca_var(ACP_LoireAtl_2017)

var$cor #correlation variables/ CP



#autre methode que prcomp 

acp_LoireAtl_2017 <- dudi.pca(newData_LoireAtl_2017, scannf= F,scale=T, center=T)

 
fviz_eig(acp_LoireAtl_2017)

#Graphique des individus. Coloration en fonction du cos2 (qualité de représentation). Les individus similaires sont groupés ensemble.

fviz_pca_ind(ACP_LoireAtl_2017,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)



#Graphique des variables. 
#Coloration en fonction de la contribution des variables. 
#Les variables corrélées positivement sont du même côté du graphique.
#Les variables corrélées négativement sont sur des côtés opposés du graphique.

fviz_pca_var(ACP_LoireAtl_2017,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)



  #Biplot des individus et des variables

fviz_pca_biplot(acp_LoireAtl_2017, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)


# Eigenvalues
eig.val <- get_eigenvalue(acp_LoireAtl_2017)
eig.val

# Results for Variables
res.var <- get_pca_var(acp_LoireAtl_2017)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(acp_LoireAtl_2017)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2  





#calculer ACP avec pca pour Landes



var_quantitativeLandes_2017 <-DVF_Landes_2017[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]

newData_Landes_2017<-var_quantitativeLandes_2017 %>% drop_na()




ACP_LANDES_2017 <- dudi.pca(newData_Landes_2017, scannf= F,scale=T, center=T)


fviz_eig(ACP_LANDES_2017,addlabels = TRUE)
#representation des individus dans les LANDES 

fviz_pca_ind(ACP_LANDES_2017,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
            
)



#ploter
plot(newData_Landes_2017$valeur_fonciere,newData_Landes_2017$surface_reelle_bati)




#Graphique des variables. 

fviz_pca_var(ACP_LANDES_2017,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

#-------------------------------

#calculer ACP avec pca pour Meuse en 2017

var_quantitativeMeuse_2017 <-DVF_Meuse_2017[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]

newData_Meuse_2017<-var_quantitativeMeuse_2017 %>% drop_na()


ACP_MEUSE_2017 <- dudi.pca(newData_Meuse_2017, scannf= F,scale=T, center=T)

fviz_eig(ACP_MEUSE_2017,addlabels = TRUE)

fviz_pca_ind(ACP_MEUSE_2017,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


#Graphique des variables. 

fviz_pca_var(ACP_MEUSE_2017,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


#calculer ACP avec pca pour Meuse en 2021

var_quantitativeMeuse_2021 <-DVF_Meuse_2021[,c("valeur_fonciere","surface_reelle_bati","nombre_pieces_principales","longitude","latitude")]
newData_Meuse_2021<-var_quantitativeMeuse_2021 %>% drop_na()

ACP_MEUSE_2021 <- dudi.pca(newData_Meuse_2021, scannf= F,scale=T, center=T)

fviz_eig(ACP_MEUSE_2021,addlabels = TRUE)

fviz_pca_ind(ACP_MEUSE_2021,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)



#Graphique des variables. 

fviz_pca_var(ACP_MEUSE_2021,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


#Afficher la valeur fonciere en fct de la surface reelle 
#affichage des observations sur les boxplots 

#trouver les valeurs des outliers 

outlier_val <- boxplot.stats(newData_Landes_2017$valeur_fonciere)$out
outlier_val

#filtrer les données sur le min des outliers  /garder que les valeurs au dessous de l'outlier 

newData_Landes_2017=newData_Landes_2017[newData_Landes_2017$valeur_fonciere<min(outlier_val),]
#on decoupe la surface en 5 classes 
newData_Landes_2017$surface_classe<-cut(x=newData_Landes_2017$surface_reelle_bati,5)


library(ggplot2)
ggplot(newData_Landes_2017, aes( x=newData_Landes_2017$surface_classe,y=newData_Landes_2017$valeur_fonciere,fill = "wheat",coulour=newData_Landes_2017$surface_reelle_bati)) +
  geom_boxplot()+ 
 # geom_jitter(width=0.25) +
  geom_boxplot(alpha=0.5)+ 
  xlab(label = "surface relle bati") +
  ylab(label = "valeur fonciere ") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Exemple de boxplots sur les données Landes") 

 
#on etudie la répartition de y en fonction de x (x c'est la variable qualitative contenant les classes qu'on souhaite comparer )
#Dans notre cas, y est une variable quantitative 

ggplot(newData_Landes_2017) + geom_boxplot(aes(x = newData_Landes_2017$surface_reelle_bati, y = newData_Landes_2017$valeur_fonciere))



outlier_idx <-which(newData_Landes_2017$valeur_fonciere %in% c(outlier_val))
outlier_idx

newData_Landes_2017[outlier_idx,]




 





                    
