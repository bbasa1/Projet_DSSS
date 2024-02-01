################################################################################
################################################################################
#### CE SCRIPT RASSEMBLE TOUTES LES FONCTIONS QUI FONT DE L'ECONOMETRIE   ######
################################################################################
################################################################################

Faire_regression_evolution_traitement <- function(data_loc, liste_var_reg_12_20, liste_var_reg_13_20){
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
    model <- lm(Evolution ~ beneficiaire, data = data_loc[TYP_IRIS_20 == 'H'], weights = P20_POP, na.action=na.exclude)
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