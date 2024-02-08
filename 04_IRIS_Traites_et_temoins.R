################################################################################
################################################################################
#### CE SCRIPT SERT A MARQUER COMME TRAITES LES IRIS BENEFICIAIRES DU GPE ######
################################################################################
################################################################################

Assigner_traitement <- function(dist_rayon_loc, data_loc){
  
  ###### On récupère les IRIS bénéficiaires du GPE
  path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
  map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)
  
  path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
  map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)
  
  map_stations_gpe_dist_rayon_loc <- st_buffer(map_stations_gpe, dist_rayon_loc)
  
  # Identification des IRIS à traiter
  
  liste_coordonnees_stations <- map_stations_gpe$geometry
  
  contient_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)
  contient_dist_rayon_loc <- st_intersects(map_iris$geometry, map_stations_gpe_dist_rayon_loc$geometry)
  
  
  liste_lignes <- as.data.frame(contient_station)$row.id
  liste_IRIS_beneficiaires <- map_iris[liste_lignes,]$CODE_IRIS
  
  liste_lignes_dist_rayon_loc <- as.data.frame(contient_dist_rayon_loc)$row.id
  liste_IRIS_beneficiaires_dist_rayon_loc <- map_iris[liste_lignes_dist_rayon_loc,]$CODE_IRIS
  
  
  ######## On assigne la variable de traitement
  
  data_loc[, beneficiaire := 0]
  
  if(dist_rayon_loc > 0){
    data_loc[IRIS %in% liste_IRIS_beneficiaires_dist_rayon_loc, beneficiaire := 1]} else {
      data_loc[IRIS %in% liste_IRIS_beneficiaires, beneficiaire := 1]}
  
  return(data_loc)
}

## Variable instrumentale


Variable_distance_aeroport <- function(data_loc){
  data_loc <- filo_merged
  ###### On récupère les IRIS bénéficiaires du GPE
  path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
  map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)
  
  # Coordonnées récupérées manuellement sur Google Maps
  puntos <- data.frame(name = c("Aéroport Roissy", "Les Halles", "Aéroport Orly"),
                       lat = c(49.009762, 48.861708, 48.72956),
                       lon = c(2.542081, 2.347543, 2.359528))
  
  # Coordonnées GPS donc EPSG:4326 mais on utilise la projection INSPIRE : EPSG:3035
  puntos_sf <- st_as_sf(puntos, coords = c("lon","lat"), crs = 4326) |> st_transform(crs = 3035)
  
  # On crée la ligne qui relie les 3 points (voir script cartographie pour tracé)
  puntos_linestr <- st_combine(puntos_sf) %>% st_cast("LINESTRING")
  df <- copy(data_loc)
  df <- merge(map_iris, df, by.x = "CODE_IRIS", by.y = "IRIS")
  
  df$distance <- st_distance(df, puntos_linestr)
  # taper 'carte_verif' dans la console pour vérifier 
  carte_verif <- tm_shape(df) + tm_polygons(col = "distance", style = "cont") + tm_shape(puntos_linestr) + tm_lines(col = "red")
  df <- df |> st_drop_geometry()
  return(df)
}
  