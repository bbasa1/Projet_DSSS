################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################
# Le dossier général
repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin


# Les sous-dossiers
repo_prgm <- paste(repgen, "Projet_DSSS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")

# On commence par importer les packages
source(paste(repo_prgm , "01_packages.R" , sep = "/"))

############################ IMPORT FILOSOFI ###################################
filo_2020 <- read.csv(paste(repo_data, "BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";" )
filo_2012 <- read_excel(path = paste(repo_data, "BASE_TD_FILO_DEC_IRIS_2012.xls", sep = "/"), sheet = 1, skip = 5)
filo_2020 <- as.data.table(filo_2020)
filo_2012 <- as.data.table(filo_2012)

# Filtre sur les IRIS d'IdF
liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94')
filo_2012 <- filo_2012[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]
filo_2020 <- filo_2020[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]

liste_iris_2012 <- filo_2012$IRIS
liste_iris_2020 <- filo_2020$IRIS
setdiff(liste_iris_2012, liste_iris_2020) ### 8 IRIS diffèrent. Il faudra regarder...
filo_2020[, COM := substr(IRIS, 1, 5)] # Il manque le code commune dans filo_2020 ==> On le rajoute

############################ IMPORT IRIS GPE ###################################

# On récupère les IRIS bénéficiaires du GPE
path_iris <- paste(repgen, "Data/CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

# Et les coordonnées géographiques des gares du GPE
path_stations_gpe <- paste(repgen, 'Data/Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)

# On récupère les numéros des IRIS bénéficiaires
liste_coordonnees_stations <- map_stations_gpe$geometry
contient_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)
liste_lignes <- as.data.frame(contient_station)$row.id
liste_IRIS_beneficiaires <- map_iris[liste_lignes,]$CODE_IRIS


########################## ASSIGNATION VARIABLE TRAITEMENT #####################
filo_2012[, beneficiaire := 0]
filo_2012[IRIS %in% liste_IRIS_beneficiaires, beneficiaire := 1]

filo_2020[, beneficiaire := 0]
filo_2020[IRIS %in% liste_IRIS_beneficiaires, beneficiaire := 1]


################################### BROUILLON / EXPLORATION ####################
### Problème n°1 : les IRIS qui disparaissent/apparaissent entre 2012 et 2020
filo_2012[, IRIS_20 := copy(IRIS)]

setdiff(filo_2012$IRIS, filo_2020$IRIS)
filo_2012[IRIS == '911820101'] # Cet IRIS a disparu en 2020
filo_2012[COM == '91182'] # Courcouronne a disparu ==> Nouveau code en 2020 : 91228
filo_2020[COM == '91228'] # 29 IRIS en 2020
filo_2012[COM == '91228'] # 19 IRIS en 2020 ==> La fusion a augmenté de 10 le nb d'IRIS

filo_2020[substr(IRIS, 1, 5) == '91182'] #Les communes ont fusionnées...
# https://www.insee.fr/fr/information/2017499 la page des modifications d'IRIS
filo_2012[IRIS == '911820101', IRIS_20 := '912280201']
filo_2012[IRIS == '911820102', IRIS_20 := '912280202']
filo_2012[IRIS == '911820103', IRIS_20 := '912280203']
filo_2012[IRIS == '911820104', IRIS_20 := '912280204']
filo_2012[IRIS == '911820105', IRIS_20 := '912280205']
filo_2012[IRIS == '911820106', IRIS_20 := '912280206']
filo_2012[IRIS == '911820107', IRIS_20 := '912280207']

setdiff(filo_2012$IRIS_20, filo_2020$IRIS)

setdiff(filo_2012$COM, filo_2020$COM) # https://www.insee.fr/fr/metadonnees/cog/commune/COM91182-courcouronnes#:~:text=Le%20code%20officiel%20g%C3%A9ographique%20de%20la%20commune%20de%20Courcouronnes%20%C3%A9tait%2091182.

# codes restants : "940220101" "940220103" "940220104"
filo_2012[IRIS == '940220101'] # Cet IRIS a disparu en 2020
filo_2020[COM == '94022']$IRIS
filo_2012[COM == '94022']$IRIS # ===> Les 3 IRIS ont disparu entre 2012 et 2020 ==> Suffit de trouver leurs nouveaux IRIS





filo_2012[, IRIS_20 := copy(IRIS)] # On part d'une copie des IRIS de 2012, qu'on va modifier petit à petit


data_modif_IRIS <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2012.xls", sep = "/"), sheet = 1, skip = 5)) # Les IRIS modifiés dans l'année
data_modif_IRIS <- data_modif_IRIS[MODIF_IRIS != '00' & DEP %in% liste_dep_idf] # Ceux qui sont en IdF
liste_IRIS_modifies <- data_modif_IRIS$IRIS_INI
filo_2012[IRIS_20 %in% liste_IRIS_modifies, ]



data_modif_IRIS <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2016.xls", sep = "/"), sheet = 3, skip = 5)) # Les IRIS modifiés dans l'année
data_modif_IRIS <- data_modif_IRIS[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf] # Ceux qui sont en IdF
liste_IRIS_modifies <- data_modif_IRIS$IRIS_INI
filo_2012[IRIS_20 %in% liste_IRIS_modifies, ]


  
data_modif_IRIS <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2019.xls", sep = "/"), sheet = 3, skip = 5)) # Les IRIS modifiés dans l'année
data_modif_IRIS <- data_modif_IRIS[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf] # Ceux qui sont en IdF
liste_IRIS_modifies <- data_modif_IRIS$IRIS_INI
filo_2012[IRIS_20 %in% liste_IRIS_modifies, ]







  
  
                