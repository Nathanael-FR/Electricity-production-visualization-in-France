# shiny_app.R

# Récupérer le répertoire contenant le fichier actuel
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Définir le répertoire de travail sur le répertoire actuel
setwd(current_directory)

rm(list=ls())

library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(shiny)
library(RColorBrewer)
library(htmltools)
library(magrittr)

geo_data = st_read("geo_data/prod-region-annuelle-filiere_ODRE//prod-region-annuelle-filiere.shp")

geo_data <- geo_data %>%
  rename(
    nucleaire = production_,
    thermique = production_.1,
    hydraulique = production_.2,
    eolienne = production_.3,
    solaire = production_.4,
    bioenergie = production_.5) %>%
    dplyr::select(-code_insee_)

# Mapping 

df_2022 <- geo_data %>% filter(annee == 2022)
df_total_2022 <- df_2022 %>%
  rowwise() %>%
  mutate(total = sum(c(nucleaire, thermique, hydraulique, eolienne, solaire, bioenergie), na.rm = TRUE))


####################################################################################################

# Mapping for all the electricity produced in 2022




color_palette2 <- colorNumeric(
  palette = c("YlOrRd"),
  domain = df_total_2022$total,
  na.color = "gray"
)

map_region_total <- leaflet(df_total_2022) %>%
  #addProviderTiles(provider = providers$OpenStreetMap.France) %>%
  addPolygons(
    data = df_total_2022,
    fillColor = ~color_palette2(total),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste(sep ="<br/>",
                   "Région : ", region, "| Electricité (GWh) :", total),
    labelOptions = labelOptions(noHide = FALSE, direction = "auto")) %>%
  addLegend( 
    pal = color_palette2,
    values = ~total,
    title = "Electricité produite (GWh)",
    position = "bottomleft",
    na.label = "NA",)


################################################################################################

colors = c("Greens","Reds","Blues","YlGnBu","Oranges","YlGn","YlOrRd")
energies <- c("nucleaire", "thermique", "hydraulique", "eolienne", "solaire", "bioenergie")
unique_years <- unique(geo_data$annee)
years <- unique_years[order(unique_years, decreasing = TRUE)]


create_map <- function(energy_index, df, year) {
  
  energy_name <- energies[energy_index]  
  df[[energy_name]] <- ifelse(is.na(df[[energy_name]]), 0, df[[energy_name]])
  
  color_pal <- colorNumeric(
    palette = colors[energy_index],
    domain = df[[energy_name]],  
  )
  
  leaflet(df) %>%
    addPolygons(
      data = df,
      fillColor = ~color_pal(df[[energy_name]]),  # Utilisation de color_pal
      color = "white",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      label = ~paste("Région : ", region, "|", energy_name, "(GWh) :", df[[energy_name]]),
      labelOptions = labelOptions(noHide = FALSE, direction = "auto")
    ) %>%
    addLegend(
      pal = color_pal,  # Utilisation du nom de l'énergie pour accéder à la palette appropriée
      values = ~df[[energy_name]],  # Utilisation du nom de l'énergie pour accéder aux valeurs appropriées
      title = paste("Electricité produite par </br>", energy_name, "en", year, "(GWh)"),
      position = "bottomleft"
    )
}


# maps <- lapply(seq_along(energies), function(i) create_map(i, df_2022, color_palettes))

maps_by_year <- lapply(years, function(year) {

    year_data <- geo_data[geo_data$annee == year,]
  maps_by_energy <- lapply(seq_along(energies), function(i) create_map(i, year_data, year))
  
  return(maps_by_energy)
})


###################################################################################################
text_below_map <- HTML("<p>Données : OpenData Réseaux Energies (ODRE)</p>")

title_panel <- tags$h2(
  style = "font-family: Open Sans, sans-serif; font-weight: bold; font-size: 40px;",
  "Production d'électricité en France à l'échelle régionale (2008-2022)"
)

ui <- fluidPage(
  title_panel,
  sidebarLayout(
    sidebarPanel(
      selectInput("energy_type", "Choisissez le type d'énergie", 
                  choices = c("Nucléaire", "Thermique", "Hydraulique", "Éolienne", "Solaire", "Bioénergie"), 
                  selected = "Nucléaire"),
      selectInput("year", "Choisissez l'année",
                  choices = years,  # Utilisez la liste des années disponibles
                  selected = max(years))
    ),
    mainPanel(
      leafletOutput("map"),
      text_below_map  # Ajoute le texte sous la carte
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    selected_energy_index <- match(input$energy_type, c("Nucléaire", "Thermique", "Hydraulique", "Éolienne", "Solaire", "Bioénergie"))
    selected_date_index <- match(input$year, years)
    
    if (!is.na(selected_energy_index) && !is.na(selected_date_index)) {
      selected_map <- maps_by_year[[selected_date_index]][[selected_energy_index]]
      return(selected_map)
    }
  })
}


# # Run the Shiny app
shinyApp(ui, server)