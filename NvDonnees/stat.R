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


departements<- read_csv("departements-france.csv")




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


##supprimer les valeurs na 

DVF_Landes_2017 %>% drop_na(valeur_fonciere)
DVF_Landes_2018 %>% drop_na(valeur_fonciere)
DVF_Landes_2019 %>% drop_na(valeur_fonciere)
DVF_Landes_2020 %>% drop_na(valeur_fonciere)
DVF_Landes_2021 %>% drop_na(valeur_fonciere)
DVF_Landes_2022 %>% drop_na(valeur_fonciere)




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










