################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################

################################################################################
#### PACKAGES, PARAMETRES ET DOSSIERS ==========================================
################################################################################

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin
# repgen <- "~/Desktop/R/Projet_DSSS" # Tanguy
utiliser_filo_merged_sauvegardee <- FALSE # FALSE pour créer la base, TRUE pour charger filo_merged déjà créée (pour gagner du temps si on relance le prbm)

dist_rayon <- 500 # Rayon = 500m vol d'oiseau
rayon = TRUE

# Les sous-dossiers
repo_prgm <- paste(repgen, "Projet_DSSS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")

# On commence par importer les packages et les sous-programmes composés uniquement de fnts
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "05_Traces_graphiques.R" , sep = "/"))
source(paste(repo_prgm , "06_Econometrie.R" , sep = "/"))


liste_var_reg_12_20 <- c("DEC_MED","DEC_D1", "DEC_D2", "DEC_D3", "DEC_D4", "DEC_D6", "DEC_D7", "DEC_D8", "DEC_D9",
                         "DEC_RD", "DEC_PCHO", "DEC_PPEN", "DEC_PAUT")
liste_var_reg_13_20 <- c("DEC_Q1", "DEC_Q3","DEC_S80S20", "DEC_GI", "DEC_PTSA", "DEC_PBEN")


################################################################################
#### CREATION ET IMPORT DE BASES ===============================================
################################################################################
# Importation ou création de la base des IRIS : filo_merged
if(! utiliser_filo_merged_sauvegardee){
  source(paste(repo_prgm , "02_Importation_merge_base.R" , sep = "/"))
  save(filo_merged, file = paste(repo_data, "filo_merged.RData", sep = "/"))
}else{
  load(paste(repo_data, "filo_merged.RData", sep = "/"))
}

filo_merged[]
filo_merged[COM.x == "78189" | COM.y == "78189"]

# Il faudrait lancer le script 03 ici pour gérer les IRIS modifiés/Créés/supprimés ==> Script à terminer
# source(paste(repo_prgm , "03_Gestion_IRIS_modifs_par_annee.R" , sep = "/"))

# On assigne la variable de traitement, rayon pour traitement des IRIS dans un certain rayon autour des gares GPE
source(paste(repo_prgm , "04_IRIS_Traites_et_temoins.R" , sep = "/"))


# Vérification de la construction  
table(filo_merged$beneficiaire) # On a 67 IRIS bénéficiaires pour 69 gares
length(unique(liste_IRIS_beneficiaires)) # Mais en fait deux IRIS sont en doublons (et ont 2 gares)
filo_merged # La tête de la base
# liste_longeurs_merged # La longueur de la base merged 2012 --> 2020
# liste_longeurs_filo_annee # Le nombre d'IRIS par année en IdF dans filosofi 2012 --> 2020


################################################################################
#### ECONOMETRIE ===============================================================
################################################################################
# Régression de évolution des variables socio-éco sur 8 ans par la variable de traitement

data_loc <- copy(filo_merged)
dt_recap <- Faire_regression_evolution_traitement(data_loc, liste_var_reg_12_20, liste_var_reg_13_20)
  

alpha <- 0.1
data_loc <- copy(dt_recap)
dt_recap <- Ajout_pval_BH(data_loc, alpha)

titre <- "pvalue et significativité\nau sens de la procédure de Benjamini-Hochberg"
titre_save <- paste(repo_sorties, "Trace_pval_BH.pdf", sep = "/")
scale_y <- "identity" # identity ou log10 pour l'axe y
trace_pval_BH(dt_recap, alpha, titre_save, titre, scale_y)

# Idem en échelle log
titre_save <- paste(repo_sorties, "Trace_pval_BH_log.pdf", sep = "/")
scale_y <- "log10" # identity ou log10 pour l'axe y
trace_pval_BH(dt_recap, alpha, titre_save, titre, scale_y)

# Pour faire des variables plus compréhensibles dans les sorties
dt_recap[, variable_label := factor(
  fcase(
    variable == "DEC_PPEN", "Part des pensions, retraites et rentes (%)",
    variable == "DEC_D2", "2e décile (€)",
    variable == "DEC_D1", "1er décile (€)",
    variable == "DEC_D3", "3e décile (€)",
    variable == "DEC_D4", "4e décile (€)",
    variable == "DEC_PBEN", "Part des revenus d'activités non salariées (%)",
    variable == "DEC_Q1", "1er quartile(€)",
    variable == "DEC_PCHO", "Part des indemnités de chômage (%)",
    variable == "DEC_EQ", "Écart interquartile rapporté à la médiane",
    variable == "DEC_TP60", "Taux de bas revenus déclarés au seuil de 60 % (%)",
    variable == "DEC_MED", "Médiane (€)",
    variable == "DEC_D8", "8e décile (€)",
    variable == "DEC_GI", "Indice de Gini",
    variable == "DEC_Q3", "3e quartile(€)",
    variable == "DEC_D6", "6e décile (€)",
    variable == "DEC_D7", "7e décile (€)",
    variable == "DEC_PTSA", "Part des revenus d'activités salariées (%)",
    variable == "DEC_D9", "9e décile (€)",
    variable == "DEC_RD", "Rapport interdécile D9/D1",
    variable == "DEC_S80S20", "S80/S20",
    variable == "DEC_PAUT", "Part des autres revenus (%)"
    
  )
)
]
l <- c("variable_label", "pval_BH", "Estimate")
xtable(dt_recap[,..l])
dt_recap[,..l]

l <- c("reconstruit", "DEC_EQ20")
filo_merged[,..l]

#### BROUILLON EN DESSOUS ##############
# 
# ########## Puis les stats des à compléter.... ##########
# 
# colnames(filo_2012)
# colonne_trace <- "DEC_D9" # La racine de la variable souhaitée
# # DEC_RD = Rapport interdécile 9/1
# # DEC_TP60 = Taux de bas revenus déclarés au seuil de 60%
# label_colonne_trace <- "Taux de bas revenus déclarés au seuil de 60%"
# titre_save <- paste(repo_sorties, "Trace_DECTP60_IRIS.pdf", sep = "/")
# label_color <- "IRIS bénéficiaires\ndu GPE"
# titre <- "Taux de bas revenus déclarés au seuil de 60 % du revenu déclaré par unité de consommation médian métropolitain (%)\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE"
# 
# 
# table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
# data_loc <- table_RD_for_plot
# trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
# model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
# summary(model)
# 
# ############### On généralise ===> A terminer...
# 
# # Listes des variables et labels associés
# # Non inclues à chaque année 'DEC_NBMENFISC', 'DEC_PRA' 
# 
# liste_var <- c('DEC_PIMP', 'DEC_TP60',
#                'DEC_D1', 'DEC_D2', 'DEC_D3', 'DEC_D4', 'DEC_MED', 'DEC_D6', 'DEC_D7',
#                'DEC_D8', 'DEC_D9',  'DEC_RD', 'DEC_PCHO', 'DEC_PPEN', 'DEC_PAUT') 
# 
# liste_label_var = c('Part des ménages fiscaux imposés (%)', 
#                     'Taux de bas revenus déclarés au seuil de 60 % du revenu déclaré par unité de consommation médian métropolitain (%)',
#                     '1er décile du revenu déclaré par unité de consommation (en euros)',
#                     '2e décile du revenu déclaré par unité de consommation (en euros)',
#                     '3e décile du revenu déclaré par unité de consommation (en euros)',
#                     '4e décile du revenu déclaré par unité de consommation (en euros)',
#                     'Médiane du revenu déclaré par unité de consommation (en euros)',
#                     '6e décile du revenu déclaré par unité de consommation (en euros)',
#                     '7e décile du revenu déclaré par unité de consommation (en euros)',
#                     '8e décile du revenu déclaré par unité de consommation (en euros)',
#                     '9e décile du revenu déclaré par unité de consommation (en euros)',
#                     'Rapport interdécile D9/D1 du revenu déclaré par unité de consommation',
#                     "Part des indemnités de chômage (%)",
#                     "Part des pensions, retraites et rentes (%)",
#                     "Part des autres revenus (essentiellement des revenus du patrimoine) (%)")
# 
# if(!rayon){
# for (i in 1:length(liste_var)){
#   colonne_trace <- liste_var[i]
#   label_colonne_trace <- liste_label_var[i]
#   # Tracé du graphe : évolution de la variable dans le temps, fichier enregistré
#   titre_save <- paste(repo_sorties, "/Trace_", colonne_trace,"_IRIS.pdf", sep = "")
#   label_color <- "IRIS bénéficiaires\ndu GPE"
#   titre <- paste(label_colonne_trace, "\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE")
#   table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
#   data_loc <- table_RD_for_plot
#   trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
#   # Régression des différences sur le temps, summary enregistré en format .txt
#   model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
#   sink(paste(repo_sorties, "/lm_", colonne_trace,".txt", sep = ''))
#   print(summary(model))
#   sink()}
# }
# 
# 
# ## Avec un rayon autour des gares comme traitement
# if(rayon){
# for (i in 1:length(liste_var)){
#   colonne_trace <- liste_var[i]
#   label_colonne_trace <- liste_label_var[i]
#   # Tracé du graphe : évolution de la variable dans le temps, fichier enregistré
#   titre_save <- paste(repo_sorties, "/Trace_", colonne_trace,"_IRIS_rayon.pdf", sep = "")
#   label_color <- "IRIS bénéficiaires\ndu GPE (rayon)"
#   titre <- paste(label_colonne_trace, "\nen fonction des années, pour les IRIS bénéficiaires et non bénéficiaires du GPE")
#   table_RD_for_plot <- preparation_table_stat_par_annee(filo_merged, colonne_trace)
#   data_loc <- table_RD_for_plot
#   trace_var_annee(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color)
#   # Régression des différences sur le temps, summary enregistré en format .txt
#   model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'Differences'])
#   sink(paste(repo_sorties, "/lm_rayon", colonne_trace,".txt", sep = ''))
#   print(summary(model))
#   sink()}
# }
# # model <- lm(value ~ annee, data = table_RD_for_plot[beneficiaire == 'diff'])
# # summary(model)
