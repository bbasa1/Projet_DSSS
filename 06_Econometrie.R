################################################################################
################################################################################
#### CE SCRIPT RASSEMBLE TOUTES LES FONCTIONS QUI FONT DE L'ECONOMETRIE   ######
################################################################################
################################################################################

Faire_regression_evolution_traitement <- function(data_loc, liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression = FALSE, liste_var_demographie){
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
  
  
  for(var in liste_var_demographie){# Puis les variables qui n'existent qu'après 2013
    var_20 <- paste("P20", var, sep = "_")
    var_12 <- paste("P12", var, sep = "_")
    
    data_loc[, Evolution := get(var_20) - get(var_12)]
    model <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP, na.action=na.exclude)
    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    df_loc$variable <- var
    l <- c("Estimate", "pvalue", "variable")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  dt_recap_loc[is.na(Annees), Annees := "2012 - 2020 (Démo)"]
  
  
  return(dt_recap_loc)
}



Faire_regression_IV <- function(data_loc, var_instru,  liste_var_reg_12_20, liste_var_reg_13_20, Ponderer_regression = FALSE, liste_var_demographie, modeliser_relatif = FALSE, var_clustering = "LIBCOM", var_controle = ""){
  # Fait toutes les régressions linéaire de la forme X = Traitement, Y = Evolution des var socio-eco
  
  # Création d'un data.tabla vierge pour stocker les résultats, en particulier les tests propres aux régressions IV (test de qualité de l'instrument, test de Wu-Hausman)
  dt_recap_loc <- data.table(Estimate = numeric(),
                             pvalue = numeric(),
                             variable = character(),
                             Annees = character(),
                             pval_weak = numeric(), #weak instrument test
                             pval_WH = numeric(),
                             std_error = numeric()) 
  
  
  for(var in liste_var_reg_12_20){ # Les variables évolutions 2012 --> 2020
    var_20 <- paste(var, "20", sep = "")
    var_12 <- paste(var, "12", sep = "")
    
    if(modeliser_relatif){
      data_loc[get(var_12) != 0, Evolution := 100*(get(var_20) - get(var_12))/get(var_12)]
      data_loc[get(var_12) == 0, Evolution := NaN]
    }else{
      data_loc[, Evolution := get(var_20) - get(var_12)]
    }

    # Est-ce qu'on pondère la regression ou non
    if(Ponderer_regression){
      model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP) 
    }else{
      if(var_controle != ""){
        model <- ivreg(Evolution ~ beneficiaire + get(var_controle) | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }else{
      model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }
    }

    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    setnames(df_loc, "Std. Error", "std_error")
    df_loc$variable <- var
    # df_loc_weak <- as.data.table(summary(model)$diagnostics)[1,]
    # df_loc$pval_weak <- df_loc_weak$`p-value`
    # df_loc_WH <- as.data.table(summary(model)$diagnostics)[2,]
    # df_loc$pval_WH <- df_loc_WH$`p-value`
    # df_loc$Wald_stat <- summary(model)$waldtest[1]
    # df_loc$Wald_pval <- summary(model)$waldtest[2]
    
    # Ajout std error clusterisées
    clustered_se <- coeftest(model, vcov. = vcovHC(model, type = "HC1", cluster = var_clustering))
    # Calcul des intervalles de confiance à 95%
    conf_int <- confint(clustered_se)
    ligne_IC <- conf_int[2,]
    df_loc$std_error <- (df_loc$Estimate - ligne_IC[1])/1.96
    
    # l <- c("Estimate", "pvalue", "variable", "pval_weak", "pval_WH", "std_error", "Wald_stat", "Wald_pval")    
    l <- c("Estimate", "pvalue", "variable", "std_error")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  dt_recap_loc$Annees <- "2012 - 2020"
  
  for(var in liste_var_reg_13_20){# Puis les variables qui n'existent qu'après 2013
    var_20 <- paste(var, "20", sep = "")
    var_12 <- paste(var, "13", sep = "")
    
    if(modeliser_relatif){
      data_loc[get(var_12) != 0, Evolution := 100*(get(var_20) - get(var_12))/get(var_12)]
      data_loc[get(var_12) == 0, Evolution := NaN]
    }else{
      data_loc[, Evolution := get(var_20) - get(var_12)]
    }
    
    
    if(Ponderer_regression){
      model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP) 
    }else{
      if(var_controle != ""){
        model <- ivreg(Evolution ~ beneficiaire + get(var_controle) | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }else{
        model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }
      }
    
    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    setnames(df_loc, "Std. Error", "std_error")
    df_loc$variable <- var

    # df_loc_weak <- as.data.table(summary(model)$diagnostics)[1,]
    # df_loc$pval_weak <- df_loc_weak$`p-value`
    # df_loc_WH <- as.data.table(summary(model)$diagnostics)[2,]
    # df_loc$pval_WH <- df_loc_WH$`p-value`
    # df_loc$Wald_stat <- summary(model)$waldtest[1]
    # df_loc$Wald_pval <- summary(model)$waldtest[2]
    
    # Ajout std error clusterisées
    clustered_se <- coeftest(model, vcov. = vcovHC(model, type = "HC1", cluster = var_clustering))
    # Calcul des intervalles de confiance à 95%
    conf_int <- confint(clustered_se)
    ligne_IC <- conf_int[2,]
    df_loc$std_error <- (df_loc$Estimate - ligne_IC[1])/1.96
    
    
    
    # l <- c("Estimate", "pvalue", "variable", "pval_weak", "pval_WH", "std_error", "Wald_stat", "Wald_pval")    
    l <- c("Estimate", "pvalue", "variable", "std_error")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  dt_recap_loc[is.na(Annees), Annees := "2013 - 2020"]
  
  
  
  for(var in liste_var_demographie){# Puis les variables de démographie
    var_20 <- paste("P20", var, sep = "_")
    var_12 <- paste("P12", var, sep = "_")

    if(modeliser_relatif){
      data_loc[get(var_12) != 0, Evolution := 100*(get(var_20) - get(var_12))/get(var_12)]
      data_loc[get(var_12) == 0, Evolution := NaN]
    }else{
      data_loc[, Evolution := get(var_20) - get(var_12)]
    }
    
    
    if(Ponderer_regression){
      model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP) 
    }else{
      if(var_controle != ""){
        model <- ivreg(Evolution ~ beneficiaire + get(var_controle) | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }else{
        model <- ivreg(Evolution ~ beneficiaire | get(var_instru), data = data_loc[TYP_IRIS_20 == 'H'])
      }
      }
    
    df_loc <- as.data.table(summary(model)$coefficients)[2,]
    setnames(df_loc, "Pr(>|t|)", "pvalue")
    setnames(df_loc, "Std. Error", "std_error")
    df_loc$variable <- var
    
    # df_loc_weak <- as.data.table(summary(model)$diagnostics)[1,]
    # df_loc$pval_weak <- df_loc_weak$`p-value`
    # df_loc_WH <- as.data.table(summary(model)$diagnostics)[2,]
    # df_loc$pval_WH <- df_loc_WH$`p-value`
    # df_loc$Wald_stat <- summary(model)$waldtest[1]
    # df_loc$Wald_pval <- summary(model)$waldtest[2]
    
    
    # Ajout std error clusterisées
    clustered_se <- coeftest(model, vcov. = vcovHC(model, type = "HC1", cluster = var_clustering))
    # Calcul des intervalles de confiance à 95%
    conf_int <- confint(clustered_se)
    ligne_IC <- conf_int[2,]
    df_loc$std_error <- (df_loc$Estimate - ligne_IC[1])/1.96
    

    # l <- c("Estimate", "pvalue", "variable", "pval_weak", "pval_WH", "std_error", "Wald_stat", "Wald_pval")    
    l <- c("Estimate", "pvalue", "variable", "std_error")
    dt_recap_loc <- rbindlist(list(dt_recap_loc, df_loc[,..l]), fill=TRUE)
  }
  dt_recap_loc[is.na(Annees), Annees := "2012 - 2020 (Démo)"]
  
  dt_recap_loc <- ajout_label_variables_filosofi(dt_recap_loc)
  dt_recap_loc$Estimate_min <- dt_recap_loc$Estimate - 1.96*dt_recap_loc$std_error
  dt_recap_loc$Estimate_max <- dt_recap_loc$Estimate + 1.96*dt_recap_loc$std_error
  dt_recap_loc$Estimate_95 <- paste("[", round(dt_recap_loc$Estimate_min, 2), " ; ", round(dt_recap_loc$Estimate_max, 2), "]", sep = "")
  
  
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