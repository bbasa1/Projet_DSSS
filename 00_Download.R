setwd(repo_data)

# Filosofi 2012 -> 2020
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/2507751/BASE_TD_FILO_DEC_IRIS_2012.xls",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2012.xls")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/2673683/BASE_TD_FILO_DEC_IRIS_2013.xls",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2013.xls")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/3288151/BASE_TD_FILO_DEC_IRIS_2014.xls",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2014.xls")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/4217503/BASE_TD_FILO_DEC_IRIS_2015.xls",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2015.xls")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/4295611/BASE_TD_FILO_DEC_IRIS_2016.xls",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2016.xls")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/4479212/BASE_TD_FILO_DEC_IRIS_2017.xlsx",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2017.xlsx")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/5055909/BASE_TD_FILO_DEC_IRIS_2018.zip",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2018.csv")
unzip("Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2018.zip", 
      exdir = "Filosofi_Annees")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/6049648/BASE_TD_FILO_DEC_IRIS_2019.zip",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2019.csv")
unzip("Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2019.zip", 
      exdir = "Filosofi_Annees")
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/7233950/BASE_TD_FILO_DEC_IRIS_2020_CSV.zip",
              destfile = "Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020.csv")
unzip("Filosofi_Annees/BASE_TD_FILO_DEC_IRIS_2020_CSV.zip", 
      exdir = "Filosofi_Annees")

# Gares GPE

download.file(url = "https://www.data.gouv.fr/fr/datasets/r/0b4c886d-a3a6-486d-8500-477d67d710cd",
              destfile = "Gares_GPE/GPE_GARE_LOCALISATION.zip")
unzip("Gares_GPE/GPE_GARE_LOCALISATION.zip", 
      exdir = "Gares_GPE")

# Contours IRIS

download.file(url = "https://wxs.ign.fr/1yhlj2ehpqf3q6dt6a2y7b64/telechargement/inspire/CONTOURS-IRIS-2020-01-01$CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/file/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01.7z",
              destfile = "CONTOURS-IRIS-2020/CONTOURS-IRIS-2020.zip"
              )
unzip("CONTOURS-IRIS-2020/CONTOURS-IRIS-2020.zip", 
      exdir = "CONTOURS-IRIS-2020"
      )

# Données IDFM sur le réseau actuel : juste les stations et leurs lignes associées suffisent, pas besoin du tracé des lignes elles-mêmes

download.file(url = "https://data.iledefrance-mobilites.fr/explore/dataset/arrets-lignes/download/?format=shp&timezone=Europe/Berlin&lang=fr",
              destfile = "Lignes IDF/arrets.zip"
)
unzip("Lignes IDF/arrets.zip", 
      exdir = "Lignes IDF"
)

### Tables de référence des IRIS
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/2017499/reference_IRIS_geo2020.zip",
              destfile = "Suivi_IRIS_Annees/reference_IRIS_geo2020.zip")
unzip("Suivi_IRIS_Annees/reference_IRIS_geo2020.zip", 
      exdir = "Suivi_IRIS_Annees"
)       
download.file(url = "https://www.insee.fr/fr/statistiques/fichier/2017499/reference_IRIS_geo2016.zip",
              destfile = "Suivi_IRIS_Annees/reference_IRIS_geo2016.zip")
unzip("Suivi_IRIS_Annees/reference_IRIS_geo2016.zip", 
      exdir = "Suivi_IRIS_Annees"
)    
# Sitadel pas IRIS mais COM

# download.file(url = "https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/datafiles/8b35affb-55fc-4c1f-915b-7750f974446a/csv?millesime=2023-11&withColumnName=true&withColumnDescription=true&withColumnUnit=false",
#               destfile = "Sitadel/Sitadel_logements.csv")
# download.file(url = "https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/datafiles/f8f0700f-806c-40a7-83b1-f21cf507e7c4/csv?millesime=2023-11&withColumnName=true&withColumnDescription=true&withColumnUnit=false",
#               destfile = "Sitadel/Sitadel_locaux.csv")
