## faire un graphe pour voir l'évolution de la valeur foncière dans le temps
## + superposer avec le taux de criminalité

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

#Calculez la valeur médiane due la valeur foncière pour chaque année

DVF_2017_med = summary(DVF_2017$valeur_fonciere,na.rm=TRUE)[3]
DVF_2018_med = summary(DVF_2018$valeur_fonciere,na.rm=TRUE)[3]
DVF_2019_med = summary(DVF_2019$valeur_fonciere,na.rm=TRUE)[3]
DVF_2020_med = summary(DVF_2020$valeur_fonciere,na.rm=TRUE)[3]
DVF_2021_med = summary(DVF_2021$valeur_fonciere,na.rm=TRUE)[3]

Tab_DVF = data.frame(DVF = c('DVF_2017','DVF_2018','DVF_2019','DVF_2020','DVF_2021'), mediane = c(DVF_2017_med,DVF_2018_med,DVF_2019_med,DVF_2020_med,DVF_2021_med))

##calculez la valeur médiane du taux de criminalité pour chaque année

crime_test = fread("crime_donnees.gz")

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

crime_2017_med = summary(crime_2017$tauxpourmille,na.rm=TRUE)[3]
crime_2018_med = summary(crime_2018$tauxpourmille,na.rm=TRUE)[3]
crime_2019_med = summary(crime_2019$tauxpourmille,na.rm=TRUE)[3]
crime_2020_med = summary(crime_2020$tauxpourmille,na.rm=TRUE)[3]
crime_2021_med = summary(crime_2021$tauxpourmille,na.rm=TRUE)[3]

Tab_DVF = data.frame(DVF = c(2017,2018,2019,2020,2021), mediane = c(DVF_2017_med,DVF_2018_med,DVF_2019_med,DVF_2020_med,DVF_2021_med), crime=c(crime_2017_med,crime_2018_med,crime_2019_med,crime_2020_med,crime_2021_med))

split.screen(1:2)
screen(2) ; plot(Tab_DVF$DVF,Tab_DVF$mediane,col='red',pch=19,type='l')
screen(1) ; plot(Tab_DVF$DVF,Tab_DVF$crime,col='green',pch=19,type='l')

lm(crime~mediane, data=Tab_DVF)
