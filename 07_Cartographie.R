# Données géospatiales, on utilise le système de coordonnées INSPIRE
# C'est préférable de s'assurer que les données spatiales utilisent toutes le mêmes système de coordonnées dès le départ
# Ça évitera des erreurs bêtes si l'on souhaite faire des opérations entre deux bases de données spatiales

path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)
map_stations_gpe_rayon <- st_buffer(map_stations_gpe, dist_rayon)

arrets <- st_read(paste(repo_data, "Lignes IDF/arrets-lignes.shp", sep = '/')) |> 
  select(id, nom_commune, code_insee, stop_id, stop_name, route_long_, operatornam) |>
  st_transform(crs = 3035)


lignes <- st_read(paste(repo_data, "Lignes IDF/lignes.shp", sep = '/')) |> 
  select(route_id, route_long_, route_type, networkname, operatornam) |>
  st_transform(crs = 3035)

# On retire IRIS typés "D" (IRIS divers : en général parcs et forêts donc peu habités)
map_iris_idf <- map_iris |> filter(substr(as.character(INSEE_COM), 1, 2) %in% liste_dep_idf, TYP_IRIS != "D")

# arrets_c <- arrets |> group_by(stop_id) |> st_centroid() |> unique()
# tm_shape(arrets_c) + tm_dots(col = 'blue')
#gtfsio::import_gtfs(paste(repo_data, "GTFS/Archive.zip", sep = '/'), encoding = "UTF-8")

  
intersection <- st_intersection(map_iris_idf, map_stations_gpe_rayon)
intersects <- st_intersects(map_iris_idf, map_stations_gpe_rayon) |> as.data.frame()

intersects_no_rayon <- st_intersects(map_iris_idf, map_stations_gpe) |> as.data.frame()

map_iris_idf$row = row(map_iris_idf)[,1]
map_iris_idf <- map_iris_idf |> mutate(intersects = row %in% intersects$row.id, 
                                       intersects_no_rayon = row %in% intersects_no_rayon$row.id)

# Traitement = IRIS contient gare GPE

tm_shape(map_iris_idf) + tm_polygons(alpha = 0) + 
  tm_shape(map_stations_gpe) + tm_dots(alpha = 1, col = 'red') +
  tm_shape(map_iris_idf |> filter(intersects_no_rayon)) + tm_polygons(alpha = 0.4, col = "red")

# Traitement = Rayon 500m autour des gares

tm_shape(map_iris_idf) + tm_polygons(alpha = 0) + 
  tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red') +
  tm_shape(map_iris_idf |> filter(intersects)) + tm_polygons(alpha = 0.4, col = "red")

# On reconstruit les variables évolution pour les tracer sur la carte

data_loc <- copy(filo_merged)
for(var in liste_var_reg_12_20){
  var_20 <- paste(var, "20", sep = "")
  var_12 <- paste(var, "12", sep = "")
  
  data_loc[, paste("EVO", var, sep = "_") := get(var_20) - get(var_12)]
}


data_loc_sf <- merge(data_loc, map_iris_idf, by.x = "IRIS", by.y = "CODE_IRIS") |> st_as_sf()


for(var in liste_var_reg_12_20){
  varevo <- paste("EVO", var, sep = "_")
  var_20 <- paste(var, "20", sep = "")
  var_12 <- paste(var, "12", sep = "")
  txt = paste("tmap_full_", varevo, " <- tm_shape(a |> filter(intersects)) + tm_polygons(col = varevo, palette = 'viridis', midpoint = 0, style = 'cont', border.alpha = 0) + tm_shape(a) + tm_polygons(col = varevo, palette = 'viridis', midpoint = 0, style = 'cont', border.alpha = 0) + tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red')", sep = "")
  txt = paste("tmap_compare_", var, " <- 
              tm_shape(a |> filter(intersects)) + tm_polygons(col = var_12, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a |> filter(intersects)) + tm_polygons(col = var_20, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a) + tm_polygons(col = var_12, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a) + tm_polygons(col = var_20, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red')", sep = "")
  eval(parse(text = txt))
}

tm_shape(data_loc_sf) + tm_polygons(col = "DEC_MED12", palette = 'viridis', style = 'cont', border.alpha = 0)
tm_shape(data_loc_sf) + tm_polygons(col = "DEC_MED20", palette = 'viridis', style = 'cont', border.alpha = 0)

# Création des points pour la IV distance_aéroport
# Coordonnées récupérées manuellement sur Google Maps
puntos <- data.frame(name = c("Aéroport Roissy", "Les Halles", "Aéroport Orly"),
                     lat = c(49.009762, 48.861708, 48.72956),
                     lon = c(2.542081, 2.347543, 2.359528))

# ORLY 48.72956, 2.359528
# ROISSY 49.009762, 2.542081
# CHATELET 48.861708, 2.347543

# Coordonnées GPS donc EPSG:4326 mais on utilise la projection INSPIRE : EPSG:3035
puntos_sf <- st_as_sf(puntos, coords = c("lon","lat"), crs = 4326) |> st_transform(crs = 3035)

# On crée la ligne qui relie les 3 points
puntos_linestr <- st_combine(puntos_sf) %>% st_cast("LINESTRING")
tm_shape(puntos_linestr) + tm_lines(col = "red") +
  tm_shape(puntos_sf) + tm_dots(col = "green")


### Stats desc spatiales ###

#Indice de Moran pour l'autocorrélation spatiale globale, package spdep
#On dégage l'ile Saint-Louis qui n'a pas de voisins : crée des erreurs

iris.nb <- poly2nb(data_loc_sf[-60,])
iris.lw <- nb2listw(iris.nb, zero.policy = FALSE)
iris.data <- as.data.frame(data_loc_sf[-60,])

iris.nb
iris.lw

for(var in liste_var_reg_12_20){
  varevo <- paste("EVO", var, sep = "_")
  txt <- paste("moran.test(iris.data$", varevo,", iris.lw, zero.policy = FALSE, na.action = na.omit)")
  eval(parse(text = txt))
}

tm_shape(data_loc_sf[-60,]) + tm_polygons(col = "EVO_DEC_TP60")

moran.test(iris.data$EVO_DEC_MED, iris.lw, zero.policy = FALSE, na.action = na.omit)
moran.mc(iris.data$EVO_DEC_MED, iris.lw, nsim = 1000, zero.policy = FALSE, na.action = na.omit)

# Autocorrélation spatiale locale

localG_perm(iris.data$EVO_DEC_TP60, iris.lw, nsim = 1000, zero.policy = FALSE)

# spdep n'est pas du tout robuste aux données manquantes
# Pb si on enlève juste les lignes sans data, iris.lw n'est plus de même taille que x : erreur
# Si on ajuste iris.lw également, fortes chances de se trouver avec des Iris sans voisins : erreur

test_autocorrelation_spatiale <- function(var){
  # Test de moran : H_0 = pas d'autocorrélation spatiale, H_1 = présence d'autocorr. spatiale
  test_moran <- moran.test(var, iris.lw, zero.policy = FALSE, na.action = na.omit)
  xtable(test_moran)
}

# Visu IRIS avec données complètes
# Environ 60% des observations incomplètes
data_loc_sf_noNA <- data_loc_sf %>% na.omit()
tm_shape(data_loc_sf_noNA) + tm_polygons(col = 'black')

