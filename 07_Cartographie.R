# Données géospatiales

path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)
map_stations_gpe_rayon <- st_buffer(map_stations_gpe, dist_rayon)

arrets <- st_read(paste(repo_data, "Lignes IDF/arrets-lignes.shp", sep = '/')) |> 
  select(id, nom_commune, code_insee, stop_id, stop_name, route_long_, operatornam)

map_iris_idf <- st_as_sf(map_iris) |> filter(substr(as.character(INSEE_COM), 1, 2) %in% liste_dep_idf)


#gtfsio::import_gtfs(paste(repo_data, "GTFS/Archive.zip", sep = '/'), encoding = "UTF-8")

#tm_shape(arrets |> filter(id == "IDFM:C01743")) + tm_dots(col = "blue") +
  #tm_shape(arrets |> filter(route_long_ == "A")) + tm_dots(col = "red")
  

intersection <- st_intersection(map_iris_idf, map_stations_gpe_rayon)
intersects <- st_intersects(map_iris_idf, map_stations_gpe_rayon) |> as.data.frame()


map_iris_idf$row = row(map_iris_idf)[,1]

map_iris_idf <- map_iris_idf |> mutate(intersects = row %in% intersects$row.id)


tm_shape(map_iris_idf) + tm_polygons(alpha = 0) + 
  tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red') +
  tm_shape(map_iris_idf |> filter(intersects)) + tm_polygons(alpha = 0.4, col = "red")

a <- merge(filo_merged, map_iris_idf, by.x = "IRIS", by.y = "CODE_IRIS") |> st_as_sf()

tm_shape(a) + tm_polygons(col = "P20_POP", palette = 'viridis', style = 'cont', border.alpha = 0)
