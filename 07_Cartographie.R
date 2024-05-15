################################################################################
######### Manipulation des objets spatiaux et tracés des cartes ################
########## Création de la variable instrumentale géographique ##################
################################################################################

# Données géospatiales, on utilise le système de coordonnées INSPIRE
# C'est préférable de s'assurer que les données spatiales utilisent toutes le mêmes système de coordonnées dès le départ
# Pour éviter des erreurs si l'on souhaite faire des opérations entre deux bases de données spatiales

contours_communes <- st_read(paste(repo_data, "communes-20220101-shp/communes-20220101.shp", sep = '/')) |> 
  filter(substr(as.character(insee), 1, 2) %in% liste_dep_idf) |> 
  st_transform(crs = 3035)

path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)
map_stations_gpe_rayon <- st_buffer(map_stations_gpe, dist_rayon)

data_election_sf <- merge(contours_communes, leg2007comm, by.x = 'insee', by.y = 'codecommune')
data_election_sf$rd_traite = abs(data_election_sf$pvoixMAJO - 0.5) < 0.1
résultats_élections = tm_shape(data_election_sf) + tm_polygons(col = 'pvoixMAJO', palette = "RdBu")
visu_traitement_rdd = tm_shape(data_election_sf) + tm_polygons(col = 'rd_traite')

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
# gtfsio::import_gtfs(paste(repo_data, "GTFS/Archive.zip", sep = '/'), encoding = "UTF-8")

  
intersection <- st_intersection(map_iris_idf, map_stations_gpe_rayon)
intersects <- st_intersects(map_iris_idf, map_stations_gpe_rayon) |> as.data.frame()

intersects_no_rayon <- st_intersects(map_iris_idf, map_stations_gpe) |> as.data.frame()

map_iris_idf$row = row(map_iris_idf)[,1]
map_iris_idf <- map_iris_idf |> mutate(intersects = row %in% intersects$row.id, 
                                       intersects_no_rayon = row %in% intersects_no_rayon$row.id)

# On reconstruit les variables évolution pour les tracer sur la carte

data_loc <- copy(filo_merged)
for(var in liste_var_reg_12_20){
  var_20 <- paste(var, "20", sep = "")
  var_12 <- paste(var, "12", sep = "")
  
  data_loc[, paste("EVO", var, sep = "_") := get(var_20) - get(var_12)]
  data_loc[, paste("EVO_rel", var, sep = "_") := (get(var_20) - get(var_12))/get(var_12)]
  
}

# On merge nos données avec l'objet 'sf' pour associer contours géographiques et données socio-éco

data_loc_sf <- merge(data_loc, map_iris_idf, by.x = "IRIS", by.y = "CODE_IRIS") |> st_as_sf()



######################################
#### IV distance : tracé de l'axe ####
######################################

# Coordonnées récupérées manuellement sur Google Maps
pts <- data.frame(name = c("Aéroport Roissy", "Les Halles", "Aéroport Orly"),
                     lat = c(49.009762, 48.861708, 48.72956),
                     lon = c(2.542081, 2.347543, 2.359528))

# Coordonnées GPS donc EPSG:4326 mais on utilise la projection INSPIRE : EPSG:3035
pts_sf <- st_as_sf(pts, coords = c("lon","lat"), crs = 4326) |> st_transform(crs = 3035)

# On crée la ligne qui relie les 3 points
pts_linestr <- st_combine(pts_sf) %>% st_cast("LINESTRING")
tm_shape(pts_linestr) + tm_lines(col = "red") +
  tm_shape(pts_sf) + tm_dots(col = "green")

data_loc_sf$distance = st_distance(data_loc_sf |> st_centroid(), pts_linestr) %>% units::set_units("m") %>% as.integer()

######################################
####            Tracés            ####
######################################

#### Tmap ####


# Traitement = IRIS contient gare GPE

tm_shape(map_stations_gpe_rayon) + tm_polygons(alpha = 1, col = 'red')


tm_shape(map_iris_idf) + tm_polygons(alpha = 0) + 
  tm_shape(map_stations_gpe) + tm_dots(alpha = 1, col = 'red') +
  tm_shape(map_iris_idf |> filter(intersects_no_rayon)) + tm_polygons(alpha = 0.4, col = "red")

# Traitement = Rayon 500m autour des gares

tm_shape(map_iris_idf) + tm_polygons(alpha = 0) + 
  tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red') +
  tm_shape(map_iris_idf |> filter(intersects)) + tm_polygons(alpha = 0.4, col = "red")

# Evolution (absolue) et état des lieux en 2012 et 2020 pour chaque indicateur

for(var in liste_var_reg_12_20){
  varevo <- paste("EVO", var, sep = "_")
  var_20 <- paste(var, "20", sep = "")
  var_12 <- paste(var, "12", sep = "")
  txt = paste("tmap_full_", varevo, " <- tm_shape(data_loc_sf |> filter(intersects)) + 
                                    tm_polygons(col = varevo, palette = 'plasma', midpoint = 0, style = 'cont', border.alpha = 0) + 
                                    tm_shape(data_loc_sf) + tm_polygons(col = varevo, palette = 'plasma', midpoint = 0, style = 'cont', border.alpha = 0) + 
                                    tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red') + 
                                    tm_shape(contours_communes) + tm_borders(alpha = 0.3)", sep = "")
  eval(parse(text = txt))
  txt = paste("tmap_compare_", var, " <-
              tm_shape(data_loc_sf |> filter(intersects)) + tm_polygons(col = var_12, palette = 'plasma', style = 'cont', border.alpha = 0) +
              tm_shape(data_loc_sf |> filter(intersects)) + tm_polygons(col = var_20, palette = 'plasma', style = 'cont', border.alpha = 0) +
              tm_shape(data_loc_sf) + tm_polygons(col = var_12, palette = 'plasma', style = 'cont', border.alpha = 0) +
              tm_shape(data_loc_sf) + tm_polygons(col = var_20, palette = 'plasma', style = 'cont', border.alpha = 0) +
              tm_shape(intersection) + tm_polygons(alpha = 0.8, col = 'red') + 
              tm_shape(contours_communes) + tm_borders(alpha = 1)", sep = "")
  eval(parse(text = txt))
}

#### GGPLOT ####

# On choisit les limites du fond de carte (boundary box), on choisit le rectangle dans lequel sont inscrits les cercles de rayon 6km centrés en les gares du GPE
# Le but de cette étape est arbitraire : on veut s'assurer que toutes les gares soient représentées, sans représenter l'intégralité de l'IDF (dont la majorité comporte des données manquantes)
# On utilise le package pgirmess qui comporte une fonction pour créer un polygon à partir de données bbox
bbox <- st_bbox(map_stations_gpe |> st_buffer(6000) |> st_transform(crs = 4326))
boundbox <- bbox2sf(bbox = bbox) 

# On garde la partie de nos données inscrites dans notre cadre, et onn passe en coordonnées GPS (EPSG:4326), sinon on a un cadre de travers
bbox_data_loc_sf <- st_intersection(data_loc_sf |> st_transform(crs = 4326), boundbox)
bbox_contours_communes <- st_intersection(contours_communes |> st_transform(crs = 4326), boundbox)
bbox_data_election_sf <- st_intersection(data_election_sf |> st_transform(crs = 4326), boundbox)

# Carte salaire médian en 2020
ggsave(
  filename = paste(repo_sorties, "Map_MED20.png", sep = '/'),
  ggplot() +
  geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
  geom_sf(data = bbox_data_loc_sf, mapping = aes(fill = DEC_MED20)) + 
  scale_fill_viridis(option = "B", discrete = FALSE) + 
  geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black") +
  guides(color = guide_legend(
         direction = "vertical",
         order = 1,
         title.position = "top",
         title.hjust = 0.5,
         nrow = 1,
         label.position = "top"
       )) +
  theme(legend.position = "right",
        panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
        panel.background = element_blank(), #retire arrièreplan gris
        axis.text = element_blank(), #retire données latitude/longitude en légende
        axis.ticks = element_blank())) #retire les points associés

# Carte salaire médian en 2012

ggsave(
  filename = paste(repo_sorties, "map_MED12.png", sep = '/'),
  ggplot() +
    geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
    geom_sf(data = bbox_data_loc_sf, mapping = aes(fill = DEC_MED12)) + 
    scale_fill_viridis(option = "B", discrete = FALSE) + 
    geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black") +
    guides(color = guide_legend(
      direction = "vertical",
      order = 1,
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "top"
    )) +
    theme(legend.position = "right",
          panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
          panel.background = element_blank(), #retire arrièreplan gris
          axis.text = element_blank(), #retire données latitude/longitude en légende
          axis.ticks = element_blank())) #retire les points associés

# Visu elections


ggsave(
  filename = paste(repo_sorties, "map_election.png", sep = '/'),
  ggplot() +
    geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
    geom_sf(data = bbox_data_election_sf, mapping = aes(fill = pvoixMAJO)) + 
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "RdBu")) +
    geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black") +
    guides(color = guide_legend(
      direction = "vertical",
      order = 1,
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "top"
    )) +
    theme(legend.position = "right",
          panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
          panel.background = element_blank(), #retire arrièreplan gris
          axis.text = element_blank(), #retire données latitude/longitude en légende
          axis.ticks = element_blank())) #retire les points associés


# IV spatiale


ggsave(
  filename = paste(repo_sorties, "map_IV_geographique.png", sep = '/'),
  ggplot() +
    geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
    geom_sf(data = bbox_data_loc_sf, mapping = aes(fill = distance)) +
    geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black") +
    geom_sf(data = puntos_linestr, mapping = aes(), color = 'red') + 
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "Reds")) +
    guides(color = guide_legend(
      direction = "vertical",
      order = 1,
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "top"
    )) +
    theme(legend.position = "right",
          panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
          panel.background = element_blank(), #retire arrièreplan gris
          axis.text = element_blank(), #retire données latitude/longitude en légende
          axis.ticks = element_blank())) #retire les points associés


# Carte évolution revenu médian entre 2012 et 2020
ggsave(
  filename = paste(repo_sorties, "Map_EVO_MED.png", sep = '/'),
  ggplot() +
    geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
    geom_sf(data = bbox_data_loc_sf, mapping = aes(fill = EVO_DEC_MED)) + 
    scale_fill_viridis(option = "B", discrete = FALSE) + 
    geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black", alpha = 0.3) +
    guides(color = guide_legend(
      direction = "vertical",
      order = 1,
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "top"
    )) +
    theme(legend.position = "right",
          panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
          panel.background = element_blank(), #retire arrièreplan gris
          axis.text = element_blank(), #retire données latitude/longitude en légende
          axis.ticks = element_blank())) #retire les points associés


# Carte évolution relative revenu médian entre 2012 et 2020 (par rapport au revenu 2012)
ggsave(
  filename = paste(repo_sorties, "Map_EVO_rel_MED.png", sep = '/'),
  ggplot() +
    geom_sf(data = bbox_contours_communes, color = "black", mapping = aes()) +
    geom_sf(data = bbox_data_loc_sf, mapping = aes(fill = EVO_rel_DEC_MED)) + 
    scale_fill_viridis(option = "B", discrete = FALSE) + 
    geom_sf(data = map_stations_gpe_rayon, mapping = aes(), fill = "black", alpha = 0.3) +
    guides(color = guide_legend(
      direction = "vertical",
      order = 1,
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      label.position = "top"
    )) +
    theme(legend.position = "right",
          panel.grid.major = element_line(colour = "transparent"), #retire quadrillage GPS (méridiens et parallèles)
          panel.background = element_blank(), #retire arrièreplan gris
          axis.text = element_blank(), #retire données latitude/longitude en légende
          axis.ticks = element_blank())) #retire les points associés

