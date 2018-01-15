library(jsonlite) # Pour steam_in

fichier_dataset = "dataset.json"

# Importation du dataset
data <- stream_in(file(fichier_dataset))

# Récupération des 10000 premiers échantillons
light_data <- data[10000,]
