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

### Tables de passage des IRIS


download.file(url = "https://www.data.gouv.fr/fr/datasets/r/59042639-564e-4b31-9a27-4a4e8fd5fee3",
              destfile = "Passages IRIS/passage_iris_2122.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/69bb1301-311a-47fb-96f6-83a1e96cde48",
              destfile = "Passages IRIS/passage_iris_2021.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/95ce6332-75d3-4a2c-9173-b74515e9723d",
              destfile = "Passages IRIS/passage_iris_1920.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/80cc1fa3-2339-4b04-9abc-d0ca84116921",
              destfile = "Passages IRIS/passage_iris_1819.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/d3be809c-6f87-4064-901e-604c4c811ebc",
              destfile = "Passages IRIS/passage_iris_1718.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/69db3f70-4c73-4452-818e-7a8e65d01ff8",
              destfile = "Passages IRIS/passage_iris_1617.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/d7bdfe3a-297d-4548-b7c6-31f258c00582",
              destfile = "Passages IRIS/passage_iris_1516.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/6c860535-fd63-48ef-9be8-75b409ca5d5f",
              destfile = "Passages IRIS/passage_iris_1415.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/4917a6ee-735f-49fd-92fe-acc061b79383",
              destfile = "Passages IRIS/passage_iris_1314.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/r/c4e3e7d7-9e93-46d8-ba99-049b7d7688d3",
              destfile = "Passages IRIS/passage_iris_1213.csv"
)
download.file(url = "https://www.data.gouv.fr/fr/datasets/historique-des-codes-iris/#/resources/8f3c93f2-f5b2-42e6-9659-b91278f68254",
              destfile = "Passages IRIS/synthetique.csv"
)
# Sitadel pas IRIS mais COM

# download.file(url = "https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/datafiles/8b35affb-55fc-4c1f-915b-7750f974446a/csv?millesime=2023-11&withColumnName=true&withColumnDescription=true&withColumnUnit=false",
#               destfile = "Sitadel/Sitadel_logements.csv")
# download.file(url = "https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/datafiles/f8f0700f-806c-40a7-83b1-f21cf507e7c4/csv?millesime=2023-11&withColumnName=true&withColumnDescription=true&withColumnUnit=false",
#               destfile = "Sitadel/Sitadel_locaux.csv")

