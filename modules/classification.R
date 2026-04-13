classification_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Kelp Restoration Classification Levels"),
    leafletOutput(ns("classification_map"), height = 800)
  )
}

classification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    site_classification <- read_csv("data/UPDATED_prioritization_All_CA_2024.csv")
    
    site_classification_sf <- st_as_sf(site_classification, coords = c(1, 2), crs = 4326)
    
    site_classification_sf$class <- factor(site_classification_sf$class_updated_2024,
                                           levels = c("Very_low", "Low", "Mid", "High"),
                                           ordered = TRUE)
    
    coords <- st_coordinates(site_classification_sf)
    half_height <- 0.00135
    half_width <- 0.0017
    
    pal_class <- colorFactor(
      palette = c("Very_low" = "#01a9f3", "Low" = "#12d045", "Mid" = "#ffca18", "High" = "#ed1c25"),
      domain = site_classification_sf$class,
      ordered = TRUE
    )
    
    output$classification_map <- renderLeaflet({
      leaflet(site_classification_sf) %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        # Restoration Class
        addRectangles(
          lng1 = coords[, "X"] - half_width,
          lat1 = coords[, "Y"] - half_height,
          lng2 = coords[, "X"] + half_width,
          lat2 = coords[, "Y"] + half_height,
          fillOpacity = 1,
          stroke = FALSE,
          color = ~pal_class(class),
          group = 'Restoration Class'
        ) %>%
        addLegend(
          pal = pal_class,
          values = ~class,
          title = 'Kelp Restoration Class',
          opacity = 1,
          group = 'Restoration Class'
        )
    })
  })
}

