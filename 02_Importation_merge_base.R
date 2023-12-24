################################################################################
################################################################################
#### CE SCRIPT SERT A IMPORTER LES DONNES FILOSOFI PUIS A PREPARER LA BASE FILO_MERGED #####
################################################################################
################################################################################

##### ETAPE 1 = Importation des filosofi et des demgraphies ####
filo_2020 <- as.data.table(read.table(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), dec=",", sep=";", header = TRUE, na.strings = c('ns', 's', '', 'so', 'nd')))
filo_2019 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2019.csv", sep = "/"), header = TRUE, sep="," ))
filo_2018 <- as.data.table(read.csv(paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2018.csv", sep = "/"), header = TRUE, sep=";" ))
filo_2017 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2017.xlsx", sep = "/"), sheet = 1, skip = 5))
filo_2016 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2016.xls", sep = "/"), sheet = 1, skip = 5))
filo_2015 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2015.xls", sep = "/"), sheet = 1, skip = 5))
filo_2014 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2014.xls", sep = "/"), sheet = 1, skip = 5))
filo_2013 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2013.xls", sep = "/"), sheet = 1, skip = 5))
filo_2012 <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2012.xls", sep = "/"), sheet = 1, skip = 5))

# Pour récupérer la population de l'IRIS
pop_2020 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2020.xlsx", sep = "/"), sheet = 1, skip = 5))
pop_2019 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2019.xlsx", sep = "/"), sheet = 1, skip = 5))
pop_2018 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2018.xlsx", sep = "/"), sheet = 1, skip = 5))
pop_2017 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2017.xlsx", sep = "/"), sheet = 1, skip = 5))
pop_2016 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2016.xls", sep = "/"), sheet = 1, skip = 5))
pop_2015 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2015.xls", sep = "/"), sheet = 1, skip = 5))
pop_2014 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2014.xls", sep = "/"), sheet = 1, skip = 5))
pop_2013 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2013.xls", sep = "/"), sheet = 1, skip = 5))
pop_2012 <- as.data.table(read_excel(path = paste(repo_data, "Population_Annees/base-ic-evol-struct-pop-2012.xls", sep = "/"), sheet = 1, skip = 5))

##### ETAPE 2 = Filtration sur IdF ####
liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94')
for(annee in 2012:2020){
  txt <- paste("filo_loc <- filo_", annee, sep = "")
  eval(parse(text = txt)) # On peut travailler avec filo_loc dans la boucle
  txt <- paste("pop_loc <- pop_", annee, sep = "")
  eval(parse(text = txt)) 
  
  filo_loc <- filo_loc[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]
  pop_loc <- pop_loc[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]

  txt <- paste("pop_loc <- pop_loc[, .(IRIS, P", substr(as.character(annee), 3, 4), "_POP)]", sep = "") # Si on veut rajouter d'autres renseignements démo on pourra le faire là ensuite
  eval(parse(text = txt))

  pop_loc$IRIS <- as.numeric(pop_loc$IRIS)
  filo_loc$IRIS <- as.numeric(filo_loc$IRIS)
  filo_loc_merged <- merge(filo_loc, pop_loc, by = 'IRIS')

  txt <- paste("filo_",annee, "<- filo_loc_merged", sep = "")
  eval(parse(text = txt)) # On remet filo modifié
}



##### ETAPE 2 = Merge des tables en partant de 2020 et en retournant vers le passé ####

filo_merged <- copy(filo_2020)

# Premier traitement sur filo_merged ==> On vire les IRIS vides (genre les Halles)
filo_merged[, NB_nan := Reduce(`+`, lapply(.SD,function(x) is.na(x)))]
filo_merged[, Prct_NAN := 100 * NB_nan/(length(filo_merged) - 1) ]
filo_merged <- filo_merged[Prct_NAN <= pourcentage_NAN_max,]
# On vire les colonnes sinon ça pose un pbm au merge
filo_merged[,Prct_NAN:=NULL]
filo_merged[,NB_nan:=NULL]

liste_longeurs_merged <- nrow(filo_merged) # Le nombre de ligne dans la table merged ==> va décroitre avec les années
liste_longeurs_filo_annee <- nrow(filo_merged) # Le nombre d'IRIS par année 

for(annee in 2019:2012){

  txt <- paste("filo_loc <- filo_", annee, sep = "")
  eval(parse(text = txt)) # On peut travailler avec filo_loc dans la boucle
  
  # On vire les IRIS avec trop de NAN
  filo_loc[, NB_nan := Reduce(`+`, lapply(.SD,function(x) is.na(x)))]
  filo_loc[, Prct_NAN := 100 * NB_nan/(length(filo_loc) - 1) ]
  filo_loc <- filo_loc[Prct_NAN <= pourcentage_NAN_max,]
  
  # On vire les colonnes sinon ça pose un pbm au merge
  filo_loc[,Prct_NAN:=NULL]
  filo_loc[,NB_nan:=NULL]
  
  if(annee %in% 2016:2012){
    filo_merged_2 <- merge(filo_merged, filo_loc, all = TRUE, by = c('IRIS', 'LIBIRIS', 'COM', 'LIBCOM'))
  }else{
    filo_merged_2 <- merge(filo_merged, filo_loc, all = TRUE, by = 'IRIS')
  }
  
  # try(filo_merged_2 <- merge(filo_merged, filo_loc, all = TRUE, by = c('IRIS', 'LIBIRIS', 'COM', 'LIBCOM')), silent = TRUE) # Pour certaines années il faut merge sur tout ça
  
  filo_merged <- copy(filo_merged_2) # On re-met dedans au cas où on est rentré dans le try
  
  liste_longeurs_filo_annee <- append(liste_longeurs_filo_annee,nrow(filo_loc))
  liste_longeurs_merged <- append(liste_longeurs_merged,nrow(filo_merged))
}


# for(annee in 2016:2012){ # On merge sur les plein de colonnes avant 2017
#   txt <- paste("filo_loc <- filo_", annee, sep = "")
#   eval(parse(text = txt)) # On peut travailler avec filo_loc dans la boucle
#   
#   # On vire les IRIS avec trop de NAN
#   filo_loc[, NB_nan := Reduce(`+`, lapply(.SD,function(x) is.na(x)))]
#   filo_loc[, Prct_NAN := 100 * NB_nan/(length(filo_loc) - 1) ]
#   filo_loc <- filo_loc[Prct_NAN <= pourcentage_NAN_max,]
#   
#   # On vire les colonnes sinon ça pose un pbm au merge
#   filo_loc[,Prct_NAN:=NULL]
#   filo_loc[,NB_nan:=NULL]
#   
#   
#   filo_merged <- merge(filo_merged, filo_loc, all = TRUE, by = c('IRIS', 'LIBIRIS', 'COM', 'LIBCOM'))
#   liste_longeurs_filo_annee <- append(liste_longeurs_filo_annee, nrow(filo_loc))
#   liste_longeurs_merged <- append(liste_longeurs_merged,nrow(filo_merged))
# }


# Enfin, on retourne les listes pour que ça soit plus naturel (2012, puis 2013, puis...)
liste_longeurs_merged <- rev(liste_longeurs_merged)
liste_longeurs_filo_annee <- rev(liste_longeurs_filo_annee)

# Exemple d'IRIS présent uniquement en 2020 : 751010101