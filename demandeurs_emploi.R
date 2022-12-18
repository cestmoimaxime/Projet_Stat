## corrélation ou non du nombre de demandeurs d'emploi en france avec la valeur fonciere (selon les departements)

install.packages("sf")
library(sf)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
library(dplyr)
library(car)

# recuperation des fichiers nb demandeurs d'emploi en france
nb_emploi_17 = read.csv(file = 'nb_emploi_2017.csv', sep=';')
nb_emploi_18 = read.csv(file = 'nb_emploi_2018.csv', sep=';')
nb_emploi_19 = read.csv(file = 'nb_emploi_2019.csv', sep=';')
nb_emploi_20 = read.csv(file = 'nb_emploi_2020.csv', sep=';')
nb_emploi_21 = read.csv(file = 'nb_emploi_2021.csv', sep=';')

nb_emploi_17_med <- nb_emploi_17 %>% 
  group_by(Code.Officiel.Département) %>% 
  summarise(median_emploi=median(Nb.moyen.demandeur.emploi,na.rm=TRUE))
nb_emploi_18_med <- nb_emploi_18 %>% 
  group_by(Code.Officiel.Département) %>% 
  summarise(median_emploi=median(Nb.moyen.demandeur.emploi,na.rm=TRUE))
nb_emploi_19_med <- nb_emploi_19 %>% 
  group_by(Code.Officiel.Département) %>% 
  summarise(median_emploi=median(Nb.moyen.demandeur.emploi,na.rm=TRUE))
nb_emploi_20_med <- nb_emploi_20 %>% 
  group_by(Code.Officiel.Département) %>% 
  summarise(median_emploi=median(Nb.moyen.demandeur.emploi,na.rm=TRUE))
nb_emploi_21_med <- nb_emploi_21 %>% 
  group_by(Code.Officiel.Département) %>% 
  summarise(median_emploi=median(Nb.moyen.demandeur.emploi,na.rm=TRUE))

# recuperation du shape departements

departements <-  sf::st_read('departements-20140306-100m.shp')
departements <- departements[1:96,]
departements = st_transform(departements,2154)

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
  summarise(med_val=median(valeur_fonciere,na.rm=TRUE))
DVF_2018 <- DVF_2018 %>% 
  group_by(code_departement) %>% 
  summarise(med_val=median(valeur_fonciere,na.rm=TRUE))
DVF_2019 <- DVF_2019 %>% 
  group_by(code_departement) %>% 
  summarise(med_val=median(valeur_fonciere,na.rm=TRUE))
DVF_2020 <- DVF_2020 %>% 
  group_by(code_departement) %>% 
  summarise(med_val=median(valeur_fonciere,na.rm=TRUE))
DVF_2021 <- DVF_2021 %>% 
  group_by(code_departement) %>% 
  summarise(med_val=median(valeur_fonciere,na.rm=TRUE))

dvf_emploi_2017 <- merge(nb_emploi_17_med, DVF_2017, by.x='Code.Officiel.Département', by.y='code_departement')
dvf_emploi_2018 <- merge(nb_emploi_18_med, DVF_2018, by.x='Code.Officiel.Département', by.y='code_departement')
dvf_emploi_2019 <- merge(nb_emploi_19_med, DVF_2019, by.x='Code.Officiel.Département', by.y='code_departement')
dvf_emploi_2020 <- merge(nb_emploi_20_med, DVF_2020, by.x='Code.Officiel.Département', by.y='code_departement')
dvf_emploi_2021 <- merge(nb_emploi_21_med, DVF_2021, by.x='Code.Officiel.Département', by.y='code_departement')

dvf_emploi_2017_dep <- merge(departements, dvf_emploi_2017, by.x='code_insee', by.y='Code.Officiel.Département')
dvf_emploi_2018_dep <- merge(departements, dvf_emploi_2018, by.x='code_insee', by.y='Code.Officiel.Département')
dvf_emploi_2019_dep <- merge(departements, dvf_emploi_2019, by.x='code_insee', by.y='Code.Officiel.Département')
dvf_emploi_2020_dep <- merge(departements, dvf_emploi_2020, by.x='code_insee', by.y='Code.Officiel.Département')
dvf_emploi_2021_dep <- merge(departements, dvf_emploi_2021, by.x='code_insee', by.y='Code.Officiel.Département')

test = dvf_emploi_2021_dep[-73,]
test = test[-89,]
g = ggplot(data =test)
g+geom_sf(aes(fill=med_val)) + stat_sf_coordinates(aes(size=median_emploi)) + scale_fill_gradient2(name = "Prix moyen de la valeur foncière", low = "blue", mid = "white", high = "red")+ ggtitle('Le nombre de demandeurs d emploi en 2021')+ scale_y_continuous(name = "Latitude")+scale_x_continuous(name = "Longitude")

## sous forme de graphe pour voir l'évolution dans le temps :

nb_emploi_17_med_1 = summary(nb_emploi_17_med$median_emploi,na.rm=TRUE)[4]
nb_emploi_18_med_1 = summary(nb_emploi_18_med$median_emploi,na.rm=TRUE)[4]
nb_emploi_19_med_1 = summary(nb_emploi_19_med$median_emploi,na.rm=TRUE)[4]
nb_emploi_20_med_1 = summary(nb_emploi_20_med$median_emploi,na.rm=TRUE)[4]
nb_emploi_21_med_1 = summary(nb_emploi_21_med$median_emploi,na.rm=TRUE)[4]

DVF_2017_med = summary(DVF_2017$med_val,na.rm=TRUE)[3]
DVF_2018_med = summary(DVF_2018$med_val,na.rm=TRUE)[3]
DVF_2019_med = summary(DVF_2019$med_val,na.rm=TRUE)[3]
DVF_2020_med = summary(DVF_2020$med_val,na.rm=TRUE)[3]
DVF_2021_med = summary(DVF_2021$med_val,na.rm=TRUE)[3]

Tab_emploi_dvf = data.frame(DVF = c(2017,2018,2019,2020,2021), emploi_med = c(nb_emploi_17_med_1,nb_emploi_18_med_1,nb_emploi_19_med_1,nb_emploi_20_med_1,nb_emploi_21_med_1), val_fonciere=c(DVF_2017_med,DVF_2018_med,DVF_2019_med,DVF_2020_med,DVF_2021_med))

split.screen(1:2)
screen(2) ; plot(Tab_emploi_dvf$DVF,Tab_emploi_dvf$emploi_med,col='red',pch=19,type='l')
screen(1) ; plot(Tab_emploi_dvf$val_fonciere,Tab_emploi_dvf$emploi_med,col='green',pch=19,type='l')

scatterplot(emploi_med~DVF, data=Tab_emploi_dvf)

ggplot(Tab_emploi_dvf$DVF,Tab_emploi_dvf$emploi_med,col='red',pch=19,type='l')
lines(Tab_emploi_dvf$DVF,Tab_emploi_dvf$val_fonciere,col='green',pch=19,type='l')

ggplot(Tab_emploi_dvf, aes(x=DVF, y=val_fonciere))+geom_line()+geom_line(data =Tab_emploi_dvf, aes(x=DVF, y=emploi_med, colour='green'))


plot.new()
par(mar=c(4,4,3,5)) 
plot(Tab_emploi_dvf$DVF,Tab_emploi_dvf$emploi_med,col="blue",type='l') 
par(new = T)
plot(Tab_emploi_dvf$DVF,Tab_emploi_dvf$val_fonciere,col="red",axes=F,xlab="",ylab="",type='l') 
axis( 4 ,col="red",col.axis="red") 
mtext("Prix moyen de la valeur foncière en France",side=4,line=2.5,col="red") 
mtext("Année",side=1,line=2.5,col="black") 

