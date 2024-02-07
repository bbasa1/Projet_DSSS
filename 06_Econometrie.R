################################################################################
################################################################################
#### CE SCRIPT RASSEMBLE TOUTES LES FONCTIONS QUI FONT DE L'ECONOMETRIE   ######
################################################################################
################################################################################

Faire_regression_evolution_traitement <- function(data_loc, liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression = FALSE){
  # Fait toutes les régressions linéaire de la forme X = Traitement, Y = Evolution des var socio-eco
  
  # Création d'un data.tabla vierge pour stocker les résultats
  dt_recap_loc <- data.table(Estimate = numeric(),
                             pvalue = numeric(),
                             variable = character(),
                             Annees = character())
  
  
  for(var in liste_var_reg_12_20){ # Les variables évolutions 2012 --> 2020
    var_20 <- paste(var, "20", sep = "")
    var_12 <- paste(var, "12", sep = "")
    
    data_loc[, Evolution := get(var_20) - get(var_12)]
    
    # Est-ce qu'on pondère la regression ou non
    if(Ponderer_regression){
      model <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP, na.action=na.exclude) 
    }else{
      model <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H'], na.action=na.exclude)
    }
    
    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    df_loc$variable <- var
    l <- c("Estimate", "pvalue", "variable")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  dt_recap_loc$Annees <- "2012 - 2020"
  
  for(var in liste_var_reg_13_20){# Puis les variables qui n'existent qu'après 2013
    var_20 <- paste(var, "20", sep = "")
    var_12 <- paste(var, "13", sep = "")
    
    data_loc[, Evolution := get(var_20) - get(var_12)]
    model <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP, na.action=na.exclude)
    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    df_loc$variable <- var
    l <- c("Estimate", "pvalue", "variable")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  
  dt_recap_loc[is.na(Annees), Annees := "2013 - 2020"]
  return(dt_recap_loc)
}



Ajout_pval_BH <- function(data_loc, alpha){
  # Cette fonction calcule les pvalue corrigées au sens de la procédure de Benjamini-Hochberg 
  
  data_loc <- data_loc[order(pvalue)] # On trie
  data_loc$ordre_pval <- 1:nrow(data_loc) 
  data_loc$facteur_BH <- nrow(data_loc)/data_loc$ordre_pval # On met une colonne facteur
  data_loc$pval_BH <- data_loc$facteur_BH * data_loc$pvalue # On calcule la pval corrigée
  data_loc$signif_BH <- FALSE
  data_loc[pval_BH <= alpha, signif_BH := TRUE]
  return(data_loc)
}

Ajout_pval_Bonf <- function(data_loc, alpha){
  # Cette fonction calcule les pvalue corrigées au sens de la procédure de Bonferroni
  
  data_loc <- data_loc[order(pvalue)] # On trie
  data_loc$ordre_pval <- 1:nrow(data_loc)
  data_loc$facteur_Bonf <- nrow(data_loc)
  data_loc$pval_Bonf <- data_loc$facteur_Bonf * data_loc$pvalue # On calcule la pval corrigée
  data_loc$signif_Bonf <- FALSE
  data_loc[pval_Bonf <= alpha, signif_Bonf := TRUE]
  return(data_loc)
}
