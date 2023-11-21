################################################################################
##### Le principal programme du projet DSSS ####################################
################################################################################
# Le dossier général
repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Projet_DSSS" # Benjamin


# Les sous-dossiers
repo_prgm <- paste(repgen, "Stage_2A_IWEPS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")


source(paste(repo_prgm , "01_packages.R" , sep = "/"))
