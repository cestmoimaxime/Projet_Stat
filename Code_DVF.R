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
