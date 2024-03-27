#leaflet_map.R

# Récupérer le répertoire contenant le fichier actuel
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Définir le répertoire de travail sur le répertoire actuel
setwd(current_directory)

rm(list=ls())
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(stringi)
library(rgdal)
library(rgeos)
library(htmltools)
library(ggplot2)
library(htmlwidgets)
library(webshot)

df_original <- read.csv("data/registre-national-installation-production-stockage-electricite-agrege_ODRE.csv", sep = ";")
geo_data_departement = st_read("geo_data//departements/departement.shp")
geo_data_iris <- st_read("geo_data/iris/iris-2013-01-01.shp") 
geo_data_epci <- st_read("geo_data//epci/EPCI_SHAPEFILE.shp")

# Create a subset
filter <- c("codeiris","commune", "filiere","puismaxinstallee")
df_iris <- df_original[,filter]

# Clean data
Encoding(df_iris$filiere)<-'latin1'
df_iris$filiere <- stri_trans_general(df_iris$filiere, "Any-Latin; Latin-ASCII")

# Convert the IRIS code from both dataframe in character
df_iris$codeiris <- as.character(df_iris$codeiris)
geo_data_iris$DCOMIRIS <- as.character(geo_data_iris$DCOMIRIS)

# Group the subset by the IRIS code of each towns/municipalities (commune)
summarized_df <- df_iris %>% 
  group_by(codeiris) %>% 
  summarise(SumPowMaxInstalled = sum(coalesce(puismaxinstallee, 0)))

# Clear duplicated values in the shapefile, and make a subset 
geo_data_iris <- geo_data_iris[!duplicated(geo_data_iris$DCOMIRIS),]
geo_data_iris <- geo_data_iris[,c("DCOMIRIS","geometry")]

# Merge both into one dataframe
merged_df <- merge(summarized_df, geo_data_iris, by.x = "codeiris", by.y = "DCOMIRIS", all.x = TRUE)
merged_df <- merged_df[-1,]

# Convert dataframe into sf object
merged_sf <- st_as_sf(merged_df)

# Convert MULTIPOLYGONS objects into points for futur mapping
merged_centroids <- st_centroid(merged_sf)

# Simplify the geometry to reduce the size of the map
geo_data_departement <- st_simplify(geo_data_departement, dTolerance = 200)

titles <- htmltools::tags$div(
  htmltools::tags$h3("Repartition des installations électriques en France (31/08/2023)"),
  htmltools::tags$p("21740 enregistrements (Regroupement des installations à la commune (IRIS))")
)

merged_centroids_filtered <- merged_centroids %>%
  filter(SumPowMaxInstalled > 0)

normalize <- function(x) {
  scaled_values <- (x - min(x)) / (max(x) - min(x))
  return(scaled_values * 10)  # Adjust the multiplier as needed
}

# Apply normalization to SumPowMaxInstalled
merged_centroids_filtered$normalized_power <- normalize(merged_centroids_filtered$SumPowMaxInstalled)

map <- leaflet() %>%
  setView(lng = 4.3508, lat = 46.8566, zoom = 6 ) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png", attribution = NULL) %>%
  addPolygons(
    data = geo_data_departement,
    fillColor = "midnightblue",
    fillOpacity = 0.4,
    color = "white", 
    weight = 1,
    label = ~nom, 
    group = "Departements" 
  ) %>%
  addCircleMarkers(
    data = merged_centroids_filtered,
    fillColor = "yellow",
    fillOpacity = 0.5,  # Set a constant value that suits your preferences
    radius = 1,
    label = ~codeiris,
    group = "Communes",
    stroke = FALSE,
  ) %>% addControl(
    titles,
    position = "topright"
  )


saveWidget(map, file = "leaflet_map.html")

webshot("leaflet_map.html", file = "leaflet_map.png", zoom = 2)


print(map)





################################# AGGREGATE BY EPCI ####################################


# Create a second subset to enlarge the scale, let's aggregate the data by EPCI code instead of IRIS
filter <- c("codeepci","epci", "filiere","puismaxinstallee")
df_epci <- df_original[,filter]

# Clean data
Encoding(df_epci$filiere)<-'latin1'
df_epci$filiere <- stri_trans_general(df_epci$filiere, "Any-Latin; Latin-ASCII")

# Convert the EPCI code from both dataframe in character
df_epci$epci <- as.character(df_epci$codeepci)
geo_data_epci$CODE_EPCI <- as.character(geo_data_epci$CODE_EPCI)

# Group the subset by the EPCI code zones in France.
summarized_df_epci <- df_epci %>%
  group_by(codeepci) %>%
  summarise(SumPowMaxInstalled = sum(coalesce(puismaxinstallee, 0)))


geo_data_epci <- geo_data_epci[,c("CODE_EPCI","NOM_EPCI","geometry")]
geo_data_epci <- st_transform(geo_data_epci, crs = st_crs(4326))

# Merge both into one dataframe
merged_df_epci <- merge(summarized_df_epci, geo_data_epci, by.x = "codeepci", by.y = "CODE_EPCI", all.x = TRUE)

# Convert dataframe into sf object
merged_sf_epci <- st_as_sf(merged_df_epci)
geo_data_epci <- st_transform(geo_data_epci, crs = st_crs(4326))


# Convert MULTIPOLYGONS objects into points for futur mapping
merged_centroids_epci <- st_centroid(merged_sf_epci)


titles_epci <- htmltools::tags$div(
  htmltools::tags$h3("Repartition des installations électriques en France (31/08/2023)"),
  htmltools::tags$p("2205 enregistrements (Regroupement des installations par code EPCI")
)

merged_centroids_epci <- merged_centroids_epci[!is.na(merged_centroids_epci$SumPowMaxInstalled),]

palette_colors <- colorNumeric(palette = "YlOrRd", domain = merged_centroids_epci$SumPowMaxInstalled)

map_epci <- leaflet() %>%
  setView(lng = 4.3508, lat = 46.8566, zoom = 6) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png", attribution = NULL) %>%
  addPolygons(
    data = geo_data_departement,
    fillColor = "midnightblue",
    fillOpacity = 0.4,
    color = "white",
    weight = 1,
    label = ~nom,
    group = "Departements"
  ) %>%
  addCircles(
    data = merged_centroids_epci,
    fillColor = ~palette_colors(SumPowMaxInstalled),
    fillOpacity = 0.8,
    color = "yellow",
    radius = ~log(SumPowMaxInstalled),
    label = ~NOM_EPCI,
    stroke = FALSE,
  ) %>% 
  addControl(
    titles_epci,
    position = "topright"
  )

map_epci