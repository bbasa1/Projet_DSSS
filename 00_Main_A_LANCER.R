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



# ############ QQ VERIFS
# mean(filo_merged[beneficiaire == 1]$DEC_D220 - filo_merged[beneficiaire == 1]$DEC_D212, na.rm = TRUE)
# mean(filo_merged[beneficiaire == 0]$DEC_D220 - filo_merged[beneficiaire == 0]$DEC_D212, na.rm = TRUE)
# 
# mean(filo_merged[beneficiaire == 1]$DEC_D220, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0 & substr(IRIS, 1, 2) != "75"]$DEC_D220, na.rm = TRUE)
# mean(filo_merged[beneficiaire == 1]$DEC_D220, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0]$DEC_D220, na.rm = TRUE)
# mean(filo_merged[beneficiaire == 1]$DEC_D212, na.rm = TRUE) - mean(filo_merged[beneficiaire == 0]$DEC_D212, na.rm = TRUE)
# 
# 
# table(filo_merged$DEC_D220)


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

l <- c("variable_label","pval_weak", "pval_WH")
print(xtable(dt_recap1[,..l]), include.rownames=FALSE)



# Elections
marge_a_50_pct <- 10 # En pourcentage d'écart
data_loc <- Variable_elections_legislative(copy(filo_merged), marge_a_50_pct)
data_loc[, MAJO_plus_50 := pvoixMAJO >= 0.5]
dt_recap2 <- Faire_regression_IV(data_loc[Z_instru == 1],var_instru = "MAJO_plus_50",liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression,
                                liste_var_demographie, modeliser_relatif = modeliser_relatif, var_clustering = "LIBCOM", var_controle = "pvoixMAJO")
l <- c("variable_label","Estimate_95", 'pvalue')
dt_recap2[,..l][order(pvalue)]
print(xtable(dt_recap2[,..l][order(pvalue)]), include.rownames=FALSE)

cor(data_loc[Z_instru == 1]$beneficiaire, data_loc[Z_instru == 1]$MAJO_plus_50)

data_loc$beneficiaire

################################################################################
########################### BROUILLON EN DESSOUS ###############################
################################################################################

l <- c("IRIS", "Evolution", "P20_POP", "P12_POP")
filo_merged[, Evolution := P20_POP - P12_POP][,..l][IRIS == '956800110']
data_loc[, Evolution := P20_POP - P12_POP][,..l][IRIS == '940810306']

14260 - 14040

data_loc$P12_POP

pop_2020[IRIS == '956800110']

# data_loc <- Variable_elections_legislative(copy(filo_merged), marge_a_50_pct)
# dt_recap <- Faire_regression_IV(data_loc,var_instru = "Z_instru",liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression, liste_var_demographie, modeliser_relatif = modeliser_relatif, var_clustering = "LIBCOM")
# 
# l <- c("variable_label","Estimate_95", 'pvalue')
# dt_recap[,..l][order(pvalue)]
# print(xtable(dt_recap[,..l][order(pvalue)]), include.rownames=FALSE)
# 
# l <- c("variable_label","pval_weak", "pval_WH")
# print(xtable(dt_recap[,..l]), include.rownames=FALSE)
# 
#   
# 
# cor(filo_merged$pvoixOPPOS, filo_merged$beneficiaire)
# 
# 
# dt_recap_merged <- merge(dt_recap_copy, dt_recap, by = c("variable", "variable_label"), suffixes = c("_Distance","_Elections"))
# 
# l <- c("variable_label", "Estimate_Distance", "Estimate_Elections", "std_error_Distance", "std_error_Elections")

# Weak instruments : pval très faible = on rejette HO = "l'instrument est faible" ==> OUF
# Wu-Hausman : pval très faible = on rejette HO = "OLS et IV sont également consistant" ==> OUF : on y gagne avec l'IV !!!
# Sargan : Uniquement dans le cas où on a plusieurs IV

# This presentation provides a decent overview with worked examples.
# 
# Weak instruments means that the instrument has a low correlation with the endogenous explanatory variable. This could result in a larger variance in the coefficient, and severe finite-sample bias. "The cure can be worse than the disease" (Bound, Jaeger, Baker, 1993/1995). See here for more details. From the help file for AER, it says it does an F-test on the first stage regression; I believe the null is that the instrument is weak. For the model you report, the null is rejected, so you can move forward with the assumption that the instrument is sufficiently strong.
# 
# Wu-Hausman tests that IV is just as consistent as OLS, and since OLS is more efficient, it would be preferable. The null here is that they are equally consistent; in this output, Wu-Hausman is significant at the p<0.1 level, so if you are OK with that confidence level, that would mean IV is consistent and OLS is not.
# 
# Sargan tests overidentification restrictions. The idea is that if you have more than one instrument per endogenous variable, the model is overidentified, and you have some excess information. All of the instruments must be valid for the inferences to be correct. So it tests that all exogenous instruments are in fact exogenous, and uncorrelated with the model residuals. If it is significant, it means that you don't have valid instruments (somewhere in there, as this is a global test). In this case, this isn't a concern. This can get more complex, and researchers have suggested doing further analysis (see this).
# data_loc <- Variable_distance_aeroport(copy(filo_merged))
# var <- "DEC_D1"
# 
# var_20 <- paste(var, "20", sep = "")
# var_12 <- paste(var, "12", sep = "")
# data_loc[, Evolution := get(var_20) - get(var_12)]
# 
# sous_dt <- data_loc[TYP_IRIS_20 == 'H']
# sous_dt$LIBCOM <- as.factor(sous_dt$LIBCOM)
# sous_dt$beneficiaire <- as.numeric(sous_dt$beneficiaire)
# 
# 
# model_iv <- ivreg(Evolution ~ beneficiaire | distance_aeroport, data = sous_dt, na.action = na.omit)
# 
# # Calcul des erreurs standards clusterisées
# clustered_se <- coeftest(model_iv, vcov. = vcovHC(model_iv, type = "HC1", cluster = "LIBCOM"))
# # Calcul des intervalles de confiance à 95%
# conf_int <- confint(clustered_se)
# ligne_IC <- conf_int[2,]
# df_loc$std_error <- (df_loc$Estimate - ligne_IC[1])/1.96
# # Affichage des intervalles de confiance
# conf_int
# ligne_IC <- conf_int[2,]
# df_loc$std_error <- (df_loc$Estimate - ligne_IC[1])/1.96
# df_loc$std_error_p <- (ligne_IC[2] - df_loc$Estimate)/1.96
# 
# #  (dt_recap$Estimate - IC)/1.96 = dt_recap$std_error
# # dt_recap$Estimate_max - dt_recap$Estimate = 1.96*dt_recap$std_error
# df_loc <- as.data.table(summary(model_iv)$coefficients)[2,]
# setnames(df_loc, "Pr(>|t|)", "pvalue")
# setnames(df_loc, "Std. Error", "std_error")
# df_loc$variable <- var
# df_loc_weak <- as.data.table(summary(model)$diagnostics)[1,]
# df_loc$pval_weak <- df_loc_weak$`p-value`
# df_loc_WH <- as.data.table(summary(model)$diagnostics)[2,]
# df_loc$pval_WH <- df_loc_WH$`p-value`




model <- ivreg(Evolution ~ beneficiaire | distance_aeroport | cluster(LIBCOM), data = sous_dt, na.rm = TRUE)

sous_dt$LIBCOM

model <- ivreg(Y ~ X | Z | cluster(C), data = nom_de_votre_dataframe)


# # Création d'un data.tabla vierge pour stocker les résultats, en particulier les tests propres aux régressions IV (test de qualité de l'instrument, test de Wu-Hausman)
# dt_recap_loc <- data.table(Estimate = numeric(),
#                            pvalue = numeric(),
#                            variable = character(),
#                            Annees = character(),
#                            pval_weak = numeric(), #weak instrument test
#                            pval_WH = numeric(),
#                            std_error = numeric()) 
# 
# 
# for(var in liste_var_reg_12_20){ # Les variables évolutions 2012 --> 2020
#   var_20 <- paste(var, "20", sep = "")
#   var_12 <- paste(var, "12", sep = "")
#   
#   
#   data_loc[get(var_12) != 0, Evolution := 100*(get(var_20) - get(var_12))/get(var_12)]
#   data_loc[get(var_12) == 0, Evolution := NaN]
#   
#   # Est-ce qu'on pondère la regression ou non
#   if(Ponderer_regression){
#     model <- ivreg(Evolution ~ beneficiaire | pvoixOPPOS, data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP) 
#   }else{
#     model <- ivreg(Evolution ~ beneficiaire | pvoixOPPOS, data = data_loc[TYP_IRIS_20 == 'H'])
#     # model_lin <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H']) #Inutile (pas utilisé ailleurs)?
#   }
#   
#   df_loc <- as.data.table(summary(model)$coefficients)[2,]
#   setnames(df_loc, "Pr(>|t|)", "pvalue")
#   setnames(df_loc, "Std. Error", "std_error")
#   df_loc$variable <- var
#   df_loc_weak <- as.data.table(summary(model)$diagnostics)[1,]
#   df_loc$pval_weak <- df_loc_weak$`p-value`
#   df_loc_WH <- as.data.table(summary(model)$diagnostics)[2,]
#   df_loc$pval_WH <- df_loc_WH$`p-value`
#   
#   l <- c("Estimate", "pvalue", "variable", "pval_weak", "pval_WH", "std_error")
#   dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
# }
# dt_recap_loc$Annees <- "2012 - 2020"
# dt_recap_loc
# l <- c("variable","std_error", "Estimate")
# dt_recap_copy[,..l]
# 
# dt_recap_loc_elections <- copy(dt_recap_loc)
# # sous_leg2007comm <- copy(leg2007comm[,..liste_tot_rows])

# 100*colMeans(sous_leg2007comm[,..liste_partis_opposition])
# 100*colMeans(sous_leg2007comm[,..liste_partis_majorite])



# leg2007comm[, pVERIF_pvoix := pvoixOPPOS + pvoixMAJO]
# 
# l <- c("nomcommune","pVERIF_pvoix", "pvoixOPPOS", "pvoixMAJO", "inscrits", "votants", "exprimes")
# leg2007comm[,..l]
# max(leg2007comm$VERIF_pvoix)
# 
# leg2007comm[,..liste_tot_rows]
# 
# leg2007comm[VERIF_pvoix >= 1.15]

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