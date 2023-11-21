# Pour explorer les données
### Pour l'instant sur les revenus DECLARES : Avant redistribution



path <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS"

path_data <- paste(path, "Data", sep = "/")

filo_2020 <- read.csv(paste(path_data, "BASE_TD_FILO_DEC_IRIS_2020.csv", sep = "/"), header = TRUE, sep=";" )
filo_2012 <- 
  
  
read.table(file = paste(path_data, "BASE_TD_FILO_DEC_IRIS_2012.xls", sep = "/"),sep = "\t", header=TRUE)
