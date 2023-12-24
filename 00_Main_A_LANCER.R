################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################
# Le dossier général

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin
# repgen <- "~/Desktop/R/Projet_DSSS" # Tanguy


# Les sous-dossiers
repo_prgm <- paste(repgen, "Projet_DSSS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")

# On commence par importer les packages et les sous-programmes composés uniquement de fnts
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "03_Traces_graphiques.R" , sep = "/"))


pourcentage_NAN_max <- 0 # Au délà on vire l'IRIS car on considère qu'on en fera rien 

# On importe les tables
source(paste(repo_prgm , "02_Importation_merge_base.R" , sep = "/"))



filo_merged

liste_longeurs_merged
liste_longeurs_filo_annee
nrow(filo_merged)

###### Gestion des IRIS modifiés :
# Chaque fichier de modif est sur les 5 dernières années, nous on a 2012 ==> 2020 donc 8 ans ==> Il en faut 2
filo_modif_16_20 <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2020.xlsx", sep = "/"), sheet = 3, skip = 5))
filo_modif_16_20 <- filo_modif_16_20[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf]
filo_modif_12_16 <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2016.xls", sep = "/"), sheet = 3, skip = 5))
filo_modif_12_16 <- filo_modif_12_16[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf]

# On cacatène
setnames(filo_modif_16_20, "annee_modif", "ANNEE_MODIF")
filo_modif <- rbind(filo_modif_16_20, filo_modif_12_16)

####### IDEE DE BASE : Si un IRIS est traité ET se trouve dans la base filo_modif, alors tous les autres IRIS en contact avec lui dans filo_modif seront considérés comme traités

filo_merged[IRIS == 751010101]$P20_POP


filo_merged[IRIS %in% filo_modif$IRIS_FIN]$IRIS
filo_merged[IRIS %in% filo_modif$IRIS_INI]$IRIS


filo_modif_reelles[IRIS_FIN %in% filo_modif_reelles$IRIS_INI] # Aucune double chaine/IRIS deux fois modifié OUF

filo_merged[IRIS == 940220104] # ANCIEN IRIS
filo_merged[IRIS == 940220113] # NOUVEL IRIS
filo_merged[IRIS == 940220114] # NOUVEL IRIS 2
filo_merged[IRIS == 940220106] # Aie

filo_merged[IRIS == 940220104, DEC_PIMP20 := sum(DEC_PIMP20*P20_POP)]
filo_merged_INI <- filo_merged[IRIS == 940220104]
filo_merged_FIN <- filo_merged[IRIS %in% c(940220113, 940220114)]
filo_merged_FIN <- merge(filo_merged_FIN, Base_pop, by = 'IRIS')


Base_pop[IRIS == 940220106]$P20_POP


filo_merged_INI$DEC_PIMP20 <- sum(filo_merged_FIN$DEC_PIMP20 * filo_merged_FIN$P20_POP)/sum(filo_merged_FIN$P20_POP)

940220113

filo_modif[IRIS_INI %in% c(940220106, 940220104) | IRIS_FIN %in% c(940220106, 940220104)]

# La population en 2020
Base_pop <- as.data.table(read_excel(path = paste(repo_data, "base-ic-evol-struct-pop-2020.xlsx", sep = "/"), sheet = 1, skip = 5))
Base_pop <- Base_pop[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]

Base_pop[IRIS == 940220106]$P20_POP

filo_modif[IRIS_FIN == 940220106 | IRIS_INI == 940220106]

# filo_2020
# filo_modif[ANNEE_MODIF == 2020]$IRIS_FIN
# filo_2020_modif <- copy(filo_2020)
# liste_iris_modif <- filo_modif[ANNEE_MODIF == 2020]$IRIS_FIN
# 
# 
# filo_2020_modif[IRIS %in% liste_iris_modif, IRIS := sapply(new_to_old_iris(IRIS, filo_modif[ANNEE_MODIF == 2020]))]
# 
# 
# dt_iris_modif <- filo_modif[ANNEE_MODIF == 2020]
# new_to_old_iris <- function(old_iris){return(dt_iris_modif[IRIS_FIN == old_iris]$IRIS_INI)}
# 
# filo_2020_modif[IRIS %in% liste_iris_modif,
#                 IRIS := lapply(IRIS,new_to_old_iris)]
# 
# filo_2020_modif[IRIS %in% liste_iris_modif,
#                 IRIS_MOD := lapply(.SD, new_to_old_iris),
#                 .SDcols = c("IRIS")] 
# 
# filo_2020_modif[IRIS %in% liste_iris_modif]
# 
# new_to_old_iris(940220106)


###### On récupère les IRIS bénéficiaires du GPE
path_iris <- paste(repo_data, "CONTOURS-IRIS-2020/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020", sep = "/")
map_iris <- st_read(paste(path_iris, "CONTOURS-IRIS.shp", sep = '/'))  %>% st_transform(crs = 3035)

path_stations_gpe <- paste(repo_data, 'Gares_GPE', sep = '/')
map_stations_gpe <- st_read(paste(path_stations_gpe, "GPE_GARE_LOCALISATION.shp", sep = '/')) %>% st_transform(crs = 3035)

liste_coordonnees_stations <- map_stations_gpe$geometry

contient_station <- st_contains(map_iris$geometry, liste_coordonnees_stations)
liste_lignes <- as.data.frame(contient_station)$row.id
liste_IRIS_beneficiaires <- map_iris[liste_lignes,]$CODE_IRIS

######## On assigne la variable de traitement

filo_merged[, beneficiaire := 0]
filo_merged[IRIS %in% liste_IRIS_beneficiaires, beneficiaire := 1]

length(liste_IRIS_beneficiaires) - nrow(filo_merged)


colnames(filo_2012)
colonne_trace <- "DEC_TP60" # La racine de la variable souhaitée
# DEC_RD = Rapport interdécile 9/1
# DEC_TP60 = Taux de bas revenus déclarés au seuil de 60%
label_colonne_trace <- "Taux de bas revenus déclarés au seuil de 60%"
titre_save <- paste(repo_sorties, "Trace_DECTP60_IRIS.pdf", sep = "/")
label_color <- "IRIS bénéficiaires\ndu GPE"
titre <- "Taux de bas revenus déclarés au seuil de 60 % du revenu déclaré par unité de consommation médian métropolitain (%)\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE"


table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
data_loc <- table_RD_for_plot
trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
summary(model)

############### On généralise ===> A terminer...

# Listes des variables et labels associés
# Non inclues à chaque année 'DEC_NBMENFISC', 'DEC_PRA' 

liste_var <- c('DEC_PIMP', 'DEC_TP60',
               'DEC_D1', 'DEC_D2', 'DEC_D3', 'DEC_D4', 'DEC_MED', 'DEC_D6', 'DEC_D7',
               'DEC_D8', 'DEC_D9',  'DEC_RD', 'DEC_PCHO', 'DEC_PPEN', 'DEC_PAUT') 

liste_label_var = c('Part des ménages fiscaux imposés (%)', 
                    'Taux de bas revenus déclarés au seuil de 60 % du revenu déclaré par unité de consommation médian métropolitain (%)',
                    '1er décile du revenu déclaré par unité de consommation (en euros)',
                    '2e décile du revenu déclaré par unité de consommation (en euros)',
                    '3e décile du revenu déclaré par unité de consommation (en euros)',
                    '4e décile du revenu déclaré par unité de consommation (en euros)',
                    'Médiane du revenu déclaré par unité de consommation (en euros)',
                    '6e décile du revenu déclaré par unité de consommation (en euros)',
                    '7e décile du revenu déclaré par unité de consommation (en euros)',
                    '8e décile du revenu déclaré par unité de consommation (en euros)',
                    '9e décile du revenu déclaré par unité de consommation (en euros)',
                    'Rapport interdécile D9/D1 du revenu déclaré par unité de consommation',
                    "Part des indemnités de chômage (%)",
                    "Part des pensions, retraites et rentes (%)",
                    "Part des autres revenus (essentiellement des revenus du patrimoine) (%)")


for (i in 1:length(liste_var)){
  colonne_trace <- liste_var[i]
  label_colonne_trace <- liste_label_var[i]
  # Tracé du graphe : évolution de la variable dans le temps, fichier enregistré
  titre_save <- paste(repo_sorties, "/Trace_", colonne_trace,"_IRIS.pdf", sep = "")
  label_color <- "IRIS bénéficiaires\ndu GPE"
  titre <- paste(label_colonne_trace, "\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE")
  table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
  data_loc <- table_RD_for_plot
  trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
  # Régression des différences sur le temps, summary enregistré en format .txt
  model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
  sink(paste(repo_sorties, "/lm_", colonne_trace,".txt", sep = ''))
  print(summary(model))
  sink()
}


# model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'diff'])
# summary(model)
