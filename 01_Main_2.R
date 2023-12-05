################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################
# Le dossier général

# repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin
repgen <- "~/Desktop/R/Projet_DSSS" # Tanguy


# Les sous-dossiers
repo_prgm <- paste(repgen, "Projet_DSSS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")

# On commence par importer les packages
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "02_Traces_graphiques.R" , sep = "/"))



# read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";", dec = ",")
# 
# fread(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), sep = ";", dec = ",")
# 
# 
# # # DF$b = 
# # 
# as.numeric(gsub(',','.',filo_2020$DEC_PAUT20))
# 
# as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), sheet = 1, skip = 5, na.strings = c('ns','', 'so')))
# 
# scan(text=filo_2020$DEC_PAUT20, dec=",", sep=".")
# 
# filo_2020 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";", dec = ",",  na.strings = c('ns','', 'so', 'nd')))
# scan(text=DEC_PAUT20, dec=",", sep=".")
# 
# 
# as.data.table(read.table(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), dec=",", sep=";", header = TRUE, na.strings = c('ns','', 'so', 'nd')))

# Puis les filosofi
filo_2020 <- as.data.table(read.table(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), dec=",", sep=";", header = TRUE, na.strings = c('ns', 's', '', 'so', 'nd')))
filo_2019 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2019.csv", sep = "/"), header = TRUE, sep="," ))
filo_2018 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2018.csv", sep = "/"), header = TRUE, sep=";" ))
filo_2017 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2017.xlsx", sep = "/"), sheet = 1, skip = 5))
filo_2016 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2016.xls", sep = "/"), sheet = 1, skip = 5))
filo_2015 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2015.xls", sep = "/"), sheet = 1, skip = 5))
filo_2014 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2014.xls", sep = "/"), sheet = 1, skip = 5))
filo_2013 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2013.xls", sep = "/"), sheet = 1, skip = 5))
filo_2012 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2012.xls", sep = "/"), sheet = 1, skip = 5))

# On filter sur l'IdF
liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94')
for(annee in 2012:2020){
  txt <- paste("filo_", annee, "<- filo_", annee, "[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]", sep = "")
  eval(parse(text = txt))
}


# filo_2020$DEC_RD20 <- as.numeric(filo_2020$DEC_RD20)
# liste_col <- filo_2020$DEC_TP6020
# liste_col <- gsub(',','.', filo)
# filo_2020$DEC_TP6020 <- liste_col
# filo_2020$DEC_TP6020 <- as.numeric(filo_2020$DEC_TP6020)


# Merge des tables
filo_merged <- copy(filo_2012)
liste_longeurs <- nrow(filo_2012) # Le nombre de ligne dans la table merged ==> va décroitre avec les années
liste_longeurs_IRIS <- nrow(filo_2012) # Le nombre d'IRIS au total en IdF

for(annee in 2013:2017){ # On merge sur les plein de colonnes avant 2017
  txt <- paste("filo_merged <- merge(filo_merged, filo_", annee, ",all = FALSE, by = c('IRIS', 'LIBIRIS', 'COM', 'LIBCOM'))", sep = "")
  eval(parse(text = txt))
  liste_longeurs <- append(liste_longeurs,nrow(filo_merged))
  txt <- paste("liste_longeurs_IRIS <- append(liste_longeurs_IRIS,nrow(filo_", annee, "))", sep = "")
  eval(parse(text = txt))
  
}
for(annee in 2018:2020){ # Et uniquement sur l'IRIS pour après 2018
  txt <- paste("filo_merged <- merge(filo_merged, filo_", annee, ",all = FALSE, by = 'IRIS')", sep = "")
  eval(parse(text = txt))
  liste_longeurs <- append(liste_longeurs,nrow(filo_merged))
  txt <- paste("liste_longeurs_IRIS <- append(liste_longeurs_IRIS,nrow(filo_", annee, "))", sep = "")
  eval(parse(text = txt))
}


liste_longeurs
liste_longeurs_IRIS

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

# Non inclues à chaque année 'DEC_NBMENFISC',

liste_var <- c('DEC_PIMP', 'DEC_TP60',
               'DEC_D1', 'DEC_D2', 'DEC_D3', 'DEC_D4', 'DEC_MED', 'DEC_D6', 'DEC_D7',
               'DEC_D8', 'DEC_D9',  'DEC_RD', 'DEC_PCHO', 'DEC_PPEN', 'DEC_PAUT') # 'DEC_PRA' présente dans 2012 seulement

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
                    # "Part des revenus d'activités (salariées et non salariées), hors indemnités de chômage (%)" présente dans 2012 seulement


for (i in 1:length(liste_var)){
  colonne_trace <- liste_var[i]
  label_colonne_trace <- liste_label_var[i]

  titre_save <- paste(repo_sorties, "/Trace_", colonne_trace,"_IRIS.pdf", sep = "")
  label_color <- "IRIS bénéficiaires\ndu GPE"
  titre <- paste(label_colonne_trace, "\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE")
  table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
  data_loc <- table_RD_for_plot
  trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
  
  model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
  sink(paste(repo_sorties, "/lm_", colonne_trace,".txt", sep = ''))
  print(summary(model))
  sink()
}


# model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'diff'])
# summary(model)