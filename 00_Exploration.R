# Pour explorer les données
### Pour l'instant sur les revenus DECLARES : Avant redistribution
library(readxl)
library(data.table)
library(sf)
library(tmap)
tmap_mode('view')

# Dictionnaires : 
# https://www.insee.fr/fr/statistiques/2507751#dictionnaire (2012)
# https://www.insee.fr/fr/statistiques/7233950#dictionnaire (2020)


path <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS"

path_data <- paste(path, "Data", sep = "/")

filo_2020 <- read.csv(paste(path_data, "BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";" )
filo_2012 <- read_excel(path = paste(path_data, "BASE_TD_FILO_DEC_IRIS_2012.xls", sep = "/"), sheet = 1, skip = 5)

filo_2020 <- as.data.table(filo_2020)
filo_2012 <- as.data.table(filo_2012)


######### On récupère les IRIS d'IdF, présents sur filo_2012
# Paris (75)
# Seine-et-Marne (77)
# Yvelines (78)
# Essonne (91)
# Hauts-de-Seine (92)
# Seine-Saint-Denis (93)
# Val-de-Marne (94)

liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94')
filo_2012 <- filo_2012[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]
filo_2020 <- filo_2020[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]

liste_iris_2012 <- filo_2012$IRIS
liste_iris_2020 <- filo_2020$IRIS
setdiff(liste_iris_2012, liste_iris_2020) ### 8 IRIS diffèrent. Il faudra regarder...



###### On récupère les IRIS bénéficiaires du GPE
path_iris <- paste(path, "Data/CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

# tm_shape(map_iris) + tm_polygons()

# map_iris %>%
#   filter(CODE_IRIS %in% liste_iris_2020)

path_stations_gpe <- paste(path, 'Data/Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)

liste_coordonnees_stations <- map_stations_gpe$geometry

# map_iris$beneficiaire_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)

contient_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)

liste_lignes <- as.data.frame(contient_station)$row.id

liste_IRIS <- map_iris[liste_lignes,]$CODE_IRIS


######## On assigne la variable de traitement

filo_2012[, beneficiaire := 0]
filo_2012[IRIS %in% liste_IRIS, beneficiaire := 1]

filo_2020[, beneficiaire := 0]
filo_2020[IRIS %in% liste_IRIS, beneficiaire := 1]

table(filo_2012$beneficiaire)
table(filo_2020$beneficiaire)
