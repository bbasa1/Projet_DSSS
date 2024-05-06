################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################

################################################################################
#### PACKAGES, PARAMETRES ET DOSSIERS ==========================================
################################################################################

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin
# repgen <- "~/Desktop/R/Projet_DSSS" # Tanguy
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

modeliser_relatif <- FALSE


liste_var_reg_12_20 <- c("DEC_TP60","DEC_MED","DEC_D1", "DEC_D2", "DEC_D3", "DEC_D4", "DEC_D6", "DEC_D7", "DEC_D8", "DEC_D9",
                         "DEC_RD", "DEC_PCHO", "DEC_PPEN", "DEC_PAUT")
liste_var_reg_13_20 <- c("DEC_Q1", "DEC_Q3","DEC_S80S20", "DEC_GI", "DEC_PTSA", "DEC_PBEN")

liste_var_demographie <- c("POP", "POP0014", "POP1529",	"POP3044",	"POP4559","POP6074","POP75P") # Le nom des variables sélectionnées dans démo, sans le préfix (ex P14_)

liste_dep_idf <- c('75', '77', '78', '91', '92', '93', '94', '95')
liste_partis_opposition <- c("pvoixDIV",'pvoixAUG','pvoixPCF','pvoixPS','pvoixRDG','pvoixDVG','pvoixVEC','pvoixECO','pvoixUDFD','pvoixDIV','pvoixMPF','pvoixFN','pvoixAUD')
liste_partis_majorite <- c('pvoixUMP', 'pvoixDVD')

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

# On assigne la variable de traitement, rayon pour traitement des IRIS dans un certain rayon autour des gares GPE
data_loc <- copy(filo_merged)
dist_rayon_loc <- dist_rayon
filo_merged <- Assigner_traitement(dist_rayon, data_loc)
  
# Vérification de la construction  
table(filo_merged$beneficiaire) # On a 67 IRIS bénéficiaires pour 69 gares

################################################################################
#### METHODE DE DIFF DE DIFF  ##################################################
################################################################################
# Régression de évolution des variables socio-éco sur 8 ans par la variable de traitement

# Les régressions
data_loc <- copy(filo_merged)
dt_recap <- Faire_regression_evolution_traitement(data_loc, liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression, liste_var_demographie)
  
# Calcul des pvalues, mise en forme
alpha <- 0.1
dt_recap <- Ajout_pval_BH(dt_recap, alpha)
dt_recap <- Ajout_pval_Bonf(dt_recap, alpha)

data_loc <- copy(dt_recap)
dt_recap <- ajout_label_variables_filosofi(data_loc)
  

l <- c("variable_label", "pval_BH","pval_Bonf", "Estimate")
print(xtable(dt_recap[,..l]), include.rownames= FALSE)
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


filo_merged[, mean(DEC_MED20 - DEC_MED12, na.rm = TRUE), by = 'beneficiaire']
filo_merged[, mean(DEC_MED20, na.rm = TRUE), by = 'beneficiaire']
filo_merged[, mean(DEC_MED12, na.rm = TRUE), by = 'beneficiaire']



################################################################################
########################### ANALYSE PAR IV  ####################################
################################################################################

## Distance à l'axe
data_loc <- Variable_distance_aeroport(copy(filo_merged))
var_instru <- "distance_aeroport"
dt_recap1 <- Faire_regression_IV(data_loc,var_instru = var_instru,liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression,
                                liste_var_demographie, modeliser_relatif = modeliser_relatif, var_clustering = "LIBCOM")

l <- c("variable_label","Estimate_95", 'pvalue')
dt_recap1[,..l][order(pvalue)]
print(xtable(dt_recap1[,..l][order(pvalue)]), include.rownames=FALSE)

alpha <- 0.10
dt_recap1 <- Ajout_pval_BH(dt_recap1, alpha)
dt_recap1 <- Ajout_pval_Bonf(dt_recap1, alpha)

l <- c("variable_label", "pvalue", "pval_Bonf", "pval_BH", "Estimate_95")
print(xtable(dt_recap1[,..l]), include.rownames=FALSE)



# Régressions variable politique
marge_a_50_pct <- 10 # En pourcentage d'écart
data_loc <- Variable_elections_legislative(copy(filo_merged), marge_a_50_pct)

data_loc[, MAJO_plus_50 := pvoixMAJO >= 0.5]
dt_recap2 <- Faire_regression_IV(data_loc[Z_instru == 1],var_instru = "MAJO_plus_50",liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression,
                                 liste_var_demographie, modeliser_relatif = modeliser_relatif, var_clustering = "LIBCOM", var_controle = "pvoixMAJO")
l <- c("variable_label","Estimate_95", 'pvalue')
dt_recap2[,..l][order(pvalue)]
print(xtable(dt_recap2[,..l][order(pvalue)]), include.rownames=FALSE)


alpha <- 0.10
dt_recap2 <- Ajout_pval_BH(dt_recap2, alpha)
dt_recap2 <- Ajout_pval_Bonf(dt_recap2, alpha)

l <- c("variable_label", "pvalue", "pval_Bonf", "pval_BH", "Estimate_95")
print(xtable(dt_recap2[,..l]), include.rownames=FALSE)




# Une deuxième version des régressions, sans variable de contrôle
marge_a_50_pct <- 10 # En pourcentage d'écart
data_loc <- Variable_elections_legislative(copy(filo_merged), marge_a_50_pct)
dt_recap2 <- Faire_regression_IV(data_loc[Z_instru == 1],var_instru = "pvoixMAJO",liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression,
                                 liste_var_demographie, modeliser_relatif = modeliser_relatif, var_clustering = "LIBCOM", var_controle = "")
l <- c("variable_label","Estimate_95", 'pvalue')
dt_recap2[,..l][order(pvalue)]
print(xtable(dt_recap2[,..l][order(pvalue)]), include.rownames=FALSE)
