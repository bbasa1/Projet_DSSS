################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################

################################################################################
#### PACKAGES, PARAMETRES ET DOSSIERS ==========================================
################################################################################

# repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin
repgen <- "~/Desktop/R/Projet_DSSS" # Tanguy
utiliser_filo_merged_sauvegardee <- TRUE # FALSE pour créer la base, TRUE pour charger filo_merged déjà créée (pour gagner du temps si on relance le prbm)

dist_rayon <- 500 # Rayon = 500m vol d'oiseau
Ponderer_regression <- FALSE # TRUE pour pondérer les régressions par la population des IRIS, FALSE sinon

# Les sous-dossiers
repo_prgm <- paste(repgen, "Projet_DSSS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")

# On commence par importer les packages et les sous-programmes composés uniquement de fnts
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "04_IRIS_Traites_et_temoins.R" , sep = "/"))
source(paste(repo_prgm , "05_Traces_graphiques.R" , sep = "/"))
source(paste(repo_prgm , "06_Econometrie.R" , sep = "/"))


liste_var_reg_12_20 <- c("DEC_TP60","DEC_MED","DEC_D1", "DEC_D2", "DEC_D3", "DEC_D4", "DEC_D6", "DEC_D7", "DEC_D8", "DEC_D9",
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

# Il faudrait lancer le script 03 ici pour gérer les IRIS modifiés/Créés/supprimés ==> Script à terminer
# source(paste(repo_prgm , "03_Gestion_IRIS_modifs_par_annee.R" , sep = "/"))

# On assigne la variable de traitement, rayon pour traitement des IRIS dans un certain rayon autour des gares GPE
data_loc <- copy(filo_merged)
dist_rayon_loc <- dist_rayon
filo_merged <- Assigner_traitement(dist_rayon, data_loc)
  
# Vérification de la construction  
table(filo_merged$beneficiaire) # On a 67 IRIS bénéficiaires pour 69 gares

################################################################################
#### ECONOMETRIE ===============================================================
################################################################################
# Régression de évolution des variables socio-éco sur 8 ans par la variable de traitement

# Les régressions
data_loc <- copy(filo_merged)
dt_recap <- Faire_regression_evolution_traitement(data_loc, liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression)
  
# Calcul des pvalues, mise en forme
alpha <- 0.1
dt_recap <- Ajout_pval_BH(dt_recap, alpha)
dt_recap <- Ajout_pval_Bonf(dt_recap, alpha)

data_loc <- copy(dt_recap)
dt_recap <- ajout_label_variables_filosofi(data_loc)
  

l <- c("variable_label", "pval_BH","pval_Bonf", "Estimate")
xtable(dt_recap[,..l])
dt_recap[,..l]

# Tracés
titre <- "pvalue et significativité\nau sens de la procédure de Benjamini-Hochberg"
titre_save <- paste(repo_sorties, "Trace_pval_BH.pdf", sep = "/")
scale_y <- "identity" # identity ou log10 pour l'axe y
trace_pval_BH(dt_recap, alpha, titre_save, titre, scale_y)

titre <- "pvalue et significativité\nau sens de la procédure de Bonferroni"
titre_save <- paste(repo_sorties, "Trace_pval_Bonf.pdf", sep = "/")
scale_y <- "identity" # identity ou log10 pour l'axe y
trace_pval_Bonf(dt_recap, alpha, titre_save, titre, scale_y)



############ QQ VERIFS
mean(filo_merged[beneficiaire == 1]$DEC_D220 - filo_merged[beneficiaire == 1]$DEC_D212, na.rm = TRUE)
mean(filo_merged[beneficiaire == 0]$DEC_D220 - filo_merged[beneficiaire == 0]$DEC_D212, na.rm = TRUE)

mean(filo_merged[beneficiaire == 1]$DEC_D220, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0 & substr(IRIS, 1, 2) != "75"]$DEC_D220, na.rm = TRUE)
mean(filo_merged[beneficiaire == 1]$DEC_D220, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0]$DEC_D220, na.rm = TRUE)
mean(filo_merged[beneficiaire == 1]$DEC_D212, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0]$DEC_D212, na.rm = TRUE)


table(filo_merged$DEC_D220)







################################################################################
########################### BROUILLON EN DESSOUS ###############################
################################################################################

# Avec nouvelle variable instru (il y a un bug qq part)
nrow(filo_merged)
data_loc <- copy(filo_merged)
nrow(data_loc)

data_loc2 <- Variable_distance_aeroport(data_loc)
nrow(data_loc2)

data_loc <- copy(data_loc2)

dt_recap <- Faire_regression_IV_aeroport_evolution_traitement(data_loc2, liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression)

dt_recap


dt_recap <- ajout_label_variables_filosofi(dt_recap)

l <- c("Estimate", 'pvalue', 'variable_label', "pval_weak", "pval_WH")

dt_recap[,..l]

#Weak instruments : pval très faible = on rejette HO = "l'instrument est faible" ==> OUF
# Wu-Hausman : pval très faible = on rejette HO = "OLS et IV sont également consistant" ==> OUF : on y gagne avec l'IV !!!
# Sargan : Uniquement dans le cas où on a plusieurs IV



# This presentation provides a decent overview with worked examples.
# 
# Weak instruments means that the instrument has a low correlation with the endogenous explanatory variable. This could result in a larger variance in the coefficient, and severe finite-sample bias. "The cure can be worse than the disease" (Bound, Jaeger, Baker, 1993/1995). See here for more details. From the help file for AER, it says it does an F-test on the first stage regression; I believe the null is that the instrument is weak. For the model you report, the null is rejected, so you can move forward with the assumption that the instrument is sufficiently strong.
# 
# Wu-Hausman tests that IV is just as consistent as OLS, and since OLS is more efficient, it would be preferable. The null here is that they are equally consistent; in this output, Wu-Hausman is significant at the p<0.1 level, so if you are OK with that confidence level, that would mean IV is consistent and OLS is not.
# 
# Sargan tests overidentification restrictions. The idea is that if you have more than one instrument per endogenous variable, the model is overidentified, and you have some excess information. All of the instruments must be valid for the inferences to be correct. So it tests that all exogenous instruments are in fact exogenous, and uncorrelated with the model residuals. If it is significant, it means that you don't have valid instruments (somewhere in there, as this is a global test). In this case, this isn't a concern. This can get more complex, and researchers have suggested doing further analysis (see this).

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


# as.data.table(read.csv("C:/Users/Benjamin/Desktop/municipales-2008-résultats-bureaux_vote-tour2.csv",skip = 1, header = FALSE, sep=c(",", "\t") , quote=""))
# read.table("C:/Users/Benjamin/Desktop/municipales-2008-résultats-bureaux_vote-tour2.csv",header=TRUE,dec=",")
# 
# # 36 colonnes L1
# # 26 colonnes header
# 
# "municipales-2008-résultats-bureaux_vote-tour2.csv"