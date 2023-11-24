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


# Puis les filosofi
filo_2020 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";" ))
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



