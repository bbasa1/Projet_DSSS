################################################################################
################################################################################
#### CE SCRIPT SERT A MARQUER COMME TRAITES LES IRIS BENEFICIAIRES DU GPE ######
################################################################################
################################################################################

#### IL FAUDRA RAJOUTER : le bail du rayon autour de la gare du GPE...


###### On récupère les IRIS bénéficiaires du GPE
path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)

#Rayon = 500m vol d'oiseau
map_stations_gpe_rayon <- st_buffer(map_stations_gpe, 500)

liste_coordonnees_stations <- map_stations_gpe$geometry

contient_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)
contient_rayon <- st_intersects(map_iris$geometry, map_stations_gpe_rayon$geometry)


liste_lignes <- as.data.frame(contient_station)$row.id
liste_IRIS_beneficiaires <- map_iris[liste_lignes,]$CODE_IRIS

liste_lignes_rayon <- as.data.frame(contient_rayon)$row.id
liste_IRIS_beneficiaires_rayon <- map_iris[liste_lignes_rayon,]$CODE_IRIS


######## On assigne la variable de traitement


filo_merged[, beneficiaire := 0]

if (rayon){
  filo_merged[IRIS %in% liste_IRIS_beneficiaires_rayon, beneficiaire := 1]} else {
    filo_merged[IRIS %in% liste_IRIS_beneficiaires, beneficiaire := 1]}

  