# Données géospatiales

path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)
map_stations_gpe_rayon <- st_buffer(map_stations_gpe, dist_rayon)

arrets <- st_read(paste(repo_data, "Lignes IDF/arrets-lignes.shp", sep = '/')) |> 
  select(id, nom_commune, code_insee, stop_id, stop_name, route_long_, operatornam)


lignes <- st_read(paste(repo_data, "Lignes IDF/lignes.shp", sep = '/')) |> 
  select(route_id, route_long_, route_type, networkname, operatornam)

# On retire IRIS typés "D" (IRIS divers : en général parcs et forêts donc peu habités)
map_iris_idf <- st_as_sf(map_iris) |> filter(substr(as.character(INSEE_COM), 1, 2) %in% liste_dep_idf, TYP_IRIS != "D")

arrets_c <- arrets |> group_by(stop_id) |> st_centroid() |> unique()
tm_shape(arrets_c) + tm_dots(col = 'blue')
#gtfsio::import_gtfs(paste(repo_data, "GTFS/Archive.zip", sep = '/'), encoding = "UTF-8")

#tm_shape(arrets |> filter(id == "IDFM:C01743")) + tm_dots(col = "blue") +
  #tm_shape(arrets |> filter(route_long_ == "A")) + tm_dots(col = "red")
  
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


a <- merge(data_loc, map_iris_idf, by.x = "IRIS", by.y = "CODE_IRIS") |> st_as_sf()


for(var in liste_var_reg_12_20){
  varevo <- paste("EVO", var, sep = "_")
  var_20 <- paste(var, "20", sep = "")
  var_12 <- paste(var, "12", sep = "")
  txt = paste("tmap_full_", varevo, " <- tm_shape(a |> filter(intersects)) + tm_polygons(col = varevo, palette = 'viridis', midpoint = 0, style = 'cont', border.alpha = 0) + tm_shape(a) + tm_polygons(col = varevo, palette = 'viridis', midpoint = 0, style = 'cont', border.alpha = 0) + tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red')", sep = "")
  txt = paste("tmap_compare_", var, " <- 
              tm_shape(a |> filter(intersects)) + tm_polygons(col = var_12, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a |> filter(intersects)) + tm_polygons(col = var_20, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a) + tm_polygons(col = var_12, palette = 'viridis', style = 'cont', border.alpha = 0) + 
              tm_shape(a) + tm_polygons(col = var_20, palette = 'viridis', style = 'cont', border.alpha = 0, labels = ) + 
              tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red')", sep = "")
  eval(parse(text = txt))
}

tm_shape(a) + tm_polygons(col = "DEC_MED12", palette = 'viridis', style = 'cont', border.alpha = 0)
tm_shape(a) + tm_polygons(col = "DEC_MED20", palette = 'viridis', style = 'cont', border.alpha = 0)




# tmap_full_EVO_DEC_D9

# #781890000
# filo_merged[IRIS == "781890000"]
# filo_merged[IRIS == "781890000"]$DEC_D918
# 
# filo_2012[IRIS == '781890000']
# pop_2020[IRIS == "781890000"]
# colnames(pop_20)
# 
# filo_2020[IRIS == '78189']
#GGPLOT, en cours

fond_carte_idf <- map_data(map = "france", region = c("Paris", "Hauts-de-Seine", "Val-de-Marne", "Seine-Saint-Denis", "Essonne", "Yvelines", "Seine-et-Marne", "Val-Doise"))

ggplot(fond_carte_idf, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  geom_polygon(data = a, mapping = aes( fill = "EVO_DEC_D1")) +
  coord_quickmap() 
