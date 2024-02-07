################################################################################
################################################################################
#### CE SCRIPT SERT A PREPARER LA BASE DE TRAVAIL, PUIS A FAIRE LES TRACER #####
################################################################################
################################################################################

preparation_table_stat_par_annee <- function(data_loc, colonne_trace){
  # Prépare la table pour faire une stat des comparative entre IRIS bénéficiaires et communes non bénéficiaires du GPE
  # En entrée filo_merged
  # En sortie une table melted avec la moyenne faite sur une année et une variable entre tous les IRIS bénéficiares/non bénéficiaires, et la diff entre les deux
  table_RD <- data.table("beneficiaire" = c(0, 1))
  for(annee in 2012:2020){
    colonne <- paste(colonne_trace, substr(as.character(annee), 3, 4), sep = '')
    sous_dt <- data_loc[, mean(get(colonne), na.rm = TRUE), by = beneficiaire]
    setnames(sous_dt, 'V1', colonne)
    table_RD <- merge(table_RD, sous_dt, 'beneficiaire')
  }
  
  # On ajoute l'écart entre les deux lignes
  diff <- as.data.table(lapply(table_RD, diff, lag = 1))
  diff$beneficiaire <- "Differences"
  table_RD <- rbindlist(list(table_RD, diff)) 
  
  
  table_RD_for_plot <- melt(data = table_RD, 
                            id.vars = "beneficiaire",
                            measure.vars  = names(table_RD)[names(table_RD) %like% colonne_trace],
                            variable.name = "variable",
                            value.name    = "value"
  )
  
  table_RD_for_plot$variable <- as.character(table_RD_for_plot$variable)
  table_RD_for_plot$annee <- 2000 + as.numeric(substr(table_RD_for_plot$variable, nchar(table_RD_for_plot$variable[1]) - 2 + 1, nchar(table_RD_for_plot$variable[1]))) # On récupère l'année en nombre
  return(table_RD_for_plot)
}


ajout_label_variables_filosofi <- function(data_loc){
  # Pour faire des variables plus compréhensibles dans les sorties
  ### ATTENTION nécessite que ce soit la colonne "variable", et créé la colonne "variable_label"
  
  data_loc[, variable_label := factor(
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
}

trace_var_annee <- function(data_loc, colonne_trace, label_colonne_trace, titre_save, titre, label_color){
  # Fait le tracé des et la droite de régression pour chaque groupe
  p <- ggplot(data_loc, aes(x = annee, y = value, color = beneficiaire)) +
    geom_point() +
    scale_colour_discrete() + 
    labs(
      x = "Année", 
      y = label_colonne_trace,
      color = label_color,
      title = titre
    ) +
    geom_smooth(method = "lm", se=FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          text = element_text(size = 25))
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}


trace_pval_BH <- function(data_loc, alpha, titre_save, titre, scale_y = "identity"){
  # Fait le tracé des pvalues et leur significativité au sens de la procédure de BH
  p <- ggplot(data_loc) +
    geom_point(aes(x = ordre_pval, y = pvalue, color = signif_BH), size = 8) +
    geom_line(aes(x = ordre_pval, y = alpha*ordre_pval/nrow(data_loc))) +
    labs(
      x = " ", 
      y = "pvalue",
      color = paste("Significatif à ", 100*alpha, "% \n(au sens B-H)", sep = ""),
      title = titre
    ) +
    scale_y_continuous(trans=scale_y) +
    theme(text = element_text(size = 25))
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}

trace_pval_Bonf <- function(data_loc, alpha, titre_save, titre, scale_y = "identity"){
  # Fait le tracé des pvalues et leur significativité au sens de la procédure de Bonferroni
  p <- ggplot(data_loc) +
    geom_point(aes(x = ordre_pval, y = pvalue, color = signif_BH), size = 8) +
    geom_line(aes(x = ordre_pval, y = alpha/nrow(data_loc))) +
    labs(
      x = " ", 
      y = "pvalue",
      color = paste("Significatif à ", 100*alpha, "% \n(au sens Bonf.)", sep = ""),
      title = titre
    ) +
    scale_y_continuous(trans=scale_y) +
    theme(text = element_text(size = 25))
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}
