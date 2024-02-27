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

# Filo pour communes
# filo_2020_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2020.xlsx", sep = "/"), sheet = 2, skip = 5))
# filo_2019_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2019.xlsx", sep = "/"), sheet = 2, skip = 5))
# filo_2018_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2018.xlsx", sep = "/"), sheet = 2, skip = 5))
# filo_2017_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2017.xlsx", sep = "/"), sheet = 2, skip = 5))
# filo_2016_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2016.xls", sep = "/"), sheet = 2, skip = 5))
# filo_2015_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2015.xls", sep = "/"), sheet = 2, skip = 5))
# filo_2014_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2014.xls", sep = "/"), sheet = 2, skip = 5))
# filo_2013_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2013.xls", sep = "/"), sheet = 2, skip = 5))
# filo_2012_comm <- as.data.table(read_excel(path = paste(repo_data, "Filosofi_Annees/FILO_DEC_COM_2012.xls", sep = "/"), sheet = 2, skip = 5))


##### ETAPE 2 = Filtration sur IdF ####
liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94', '95')
for(annee in 2012:2020){
  txt <- paste("filo_loc <- copy(filo_", annee, ")", sep = "")
  eval(parse(text = txt)) # On peut travailler avec filo_loc dans la boucle
  txt <- paste("pop_loc <- copy(pop_", annee,")", sep = "")
  eval(parse(text = txt))
  # txt <- paste("filo_loc_comm <- copy(filo_", annee,"_comm)", sep = "")
  # eval(parse(text = txt)) 
  
  filo_loc <- filo_loc[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]
  pop_loc <- pop_loc[substr(as.character(IRIS), 1, 2) %in% liste_dep_idf]
  # filo_loc_comm <- filo_loc_comm[substr(as.character(CODGEO), 1, 2) %in% liste_dep_idf]
  
  # liste_var_demographie <- c("POP", "POP0014", "POP1529",	"POP3044",	"POP4559","POP6074","POP75P") (ex P14_)
  prefix_demo <- paste("P", substr(as.character(annee), 3, 4), sep = "")
  liste_var_demographie_loc <- paste(prefix_demo, liste_var_demographie, sep = "_")
  liste_var_demographie_loc <- append(liste_var_demographie_loc, c("IRIS", "TYP_IRIS"))
  pop_loc <- pop_loc[,..liste_var_demographie_loc]

  setnames(pop_loc, 'TYP_IRIS', paste('TYP_IRIS_', substr(as.character(annee), 3, 4), sep = ""))
  filo_loc_merged <- merge(filo_loc, pop_loc, by = 'IRIS', all = TRUE)

  nom_col_IRIS_NAN <- paste('DEC_TP60', substr(annee, 3, 4), sep = "")
  
  # liste_IRIS_NAN <- filo_loc_merged[is.na(get(nom_col_IRIS_NAN))]$IRIS
  # liste_COMM_NAN <- substr(liste_IRIS_NAN, 1, 5)
  # filo_loc_comm <- filo_loc_comm[CODGEO %in% liste_COMM_NAN & substr(CODGEO, 1, 2) != "75"] # On ne prend PAS Paris

  # On renome qq colonnes pour récupérer les données communes  
  # setnames(filo_loc_comm, "RD", paste("RD", substr(annee, 3, 4), sep = ""))
  # setnames(filo_loc_comm, "Q3_Q1", paste("Q3_Q1", substr(annee, 3, 4), sep = ""))
  # setnames(filo_loc_comm, paste("Q2", substr(annee, 3, 4), sep = ""),  paste("MED", substr(annee, 3, 4), sep = ""))
  # filo_loc_comm[, LIBGEO := NULL]
  # filo_loc_merged[, COM := substr(IRIS, 1, 5)]
  # filo_loc_merged <- merge(filo_loc_merged, filo_loc_comm, by.x = 'COM', by.y = "CODGEO", all.x = TRUE)

  # # On met les données communes dans les IRIS manquants
  # if(annee != 2012){ # Pour cette liste ça ne marche pas en 2012
  #   for(var in liste_var_reg_13_20){
  #     nom_col_iris <- paste(var, substr(annee, 3, 4), sep = "")
  #     nom_col_comm <- substr(nom_col_iris, 5, 1000)
  #     filo_loc_merged[IRIS %in% liste_IRIS_NAN, eval(nom_col_iris) := get(nom_col_comm)]
  #   }  
  # }
  # 
  # for(var in liste_var_reg_12_20){
  #   nom_col_iris <- paste(var, substr(annee, 3, 4), sep = "")
  #   nom_col_comm <- substr(nom_col_iris, 5, 1000)
  #   filo_loc_merged[IRIS %in% liste_IRIS_NAN, eval(nom_col_iris) := get(nom_col_comm)]
  # }
  
  
  
  txt <- paste("filo_",annee, "<- filo_loc_merged", sep = "")
  eval(parse(text = txt)) # On remet filo modifié
}



##### ETAPE 3 = Merge des tables en partant de 2020 et en retournant vers le passé ####

filo_merged <- copy(filo_2020)

liste_longeurs_merged <- nrow(filo_merged) # Le nombre de ligne dans la table merged ==> va décroitre avec les années
liste_longeurs_filo_annee <- nrow(filo_merged) # Le nombre d'IRIS par année 

for(annee in 2019:2012){
  txt <- paste("filo_loc <- copy(filo_", annee, ")", sep = "")
  eval(parse(text = txt)) # On peut travailler avec filo_loc dans la boucle
  
  # On vire 'LIBIRIS', 'COM', 'LIBCOM' ==> Sinon ça pause pbm, il faut les rajouter une fois à la fin
  try(filo_loc[, c('LIBIRIS', 'COM', 'LIBCOM') := NULL], silent = TRUE)
  
  filo_merged <- merge(filo_merged, filo_loc, all = TRUE, by = 'IRIS')
  liste_longeurs_filo_annee <- append(liste_longeurs_filo_annee,nrow(filo_loc))
  liste_longeurs_merged <- append(liste_longeurs_merged,nrow(filo_merged))
}

#### ETAPE 4 = Les modifs de fin

# On rajoute 'LIBIRIS', 'COM', 'LIBCOM' qui sont pratiques pour identifier rapidement les IRIS ==> On prend filo_2017
filo_merged <- merge(filo_merged, filo_2017[, c('IRIS','LIBIRIS', 'COM', 'LIBCOM')], all = FALSE, by = 'IRIS')


# Enfin, on retourne les listes pour que ça soit plus naturel (2012, puis 2013, puis...)
liste_longeurs_merged <- rev(liste_longeurs_merged)

# Exemple d'IRIS présent uniquement en 2020 : 751010101





