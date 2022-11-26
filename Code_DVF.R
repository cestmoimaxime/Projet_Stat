library(data.table)
library("curl")
library("R.utils")

# DVF_2022=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2022.csv")
# DVF_2021=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2021.csv")
# DVF_2020=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2020.csv")
# DVF_2019=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2019.csv")
# DVF_2018=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2018.csv")
# DVF_2017=read.csv("C://Users/Maxime/Documents/ING3/Projet_DVF/DVF_2017.csv")


dt = fread("https://files.data.gouv.fr/geo-dvf/latest/csv/2017/full.csv.gz")
DVF_2017=dt
