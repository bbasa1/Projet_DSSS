################################################################################
################################################################################
#### CE SCRIPT SERT A GERER LES IRIS QUI ONT ETE MODIFIES AU FIL DES ANNEES ####
################################################################################
################################################################################


######## CE SCRIPT N'EST PAS TERMINE, ET NE SERT A RIEN AUJOURD'HUI ==> Aujourd'hui on ignore juste les NAN dans les calculs...

###### Gestion des IRIS modifiés :
# Chaque fichier de modif est sur les 5 dernières années, nous on a 2012 ==> 2020 donc 8 ans ==> Il en faut 2
filo_modif_16_20 <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2020.xlsx", sep = "/"), sheet = 3, skip = 5))
filo_modif_16_20 <- filo_modif_16_20[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf]
filo_modif_12_16 <- as.data.table(read_excel(path = paste(repo_data, "Suivi_IRIS_Annees/reference_IRIS_geo2016.xls", sep = "/"), sheet = 3, skip = 5))
filo_modif_12_16 <- filo_modif_12_16[substr(as.character(IRIS_INI), 1, 2) %in% liste_dep_idf]

# On cacatène
setnames(filo_modif_16_20, "annee_modif", "ANNEE_MODIF")
filo_modif <- rbind(filo_modif_16_20, filo_modif_12_16)

# Problème : chaque cas est différent... On va essayer de faire des grandes catégories pour régler les cas qu'on peut régler

####### IDEE DE BASE : Si un IRIS est traité ET se trouve dans la base filo_modif, alors tous les autres IRIS en contact avec lui dans filo_modif seront considérés comme traités
# IRIS INI = 773160000   
# IRIS FIN = 773160101
# ANNEE = 2017

filo_merged[IRIS %in% c(773160000, 773160101)]$TYP_IRIS_20



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
