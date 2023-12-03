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