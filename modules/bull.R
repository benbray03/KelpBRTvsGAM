bull_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Bull Kelp (North and Central)"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("model"), "Select Earth System Model:",
                    choices = c("GFDL" = "gfdl", "HADL" = "hadl", "IPSL" = "ipsl"),
                    selected = "gfdl"),
        sliderInput(ns("year"), "Select Year:",
                    min = 2021, max = 2100, value = 2021, step = 1, sep = ""),
        selectInput(ns("variable"), "Select Variable to Map:",
                    choices = c(
                      "Depth" = "depth",
                      "Nitrate Surface Annual" = "no3_surface_annual",
                      "Nitrate Surface Summer" = "no3_surface_summer",
                      "Nitrate Mean Annual" = "no3_mean_annual",
                      "Tau Along-shore Surface Annual" = "taualong_surface_annual",
                      "Tau Along-shore Surface Upwelling" = "taualong_surface_upwelling",
                      "Temperature Surface Summer" = "temp_surface_summer",
                      "Temperature Mean Summer" = "temp_mean_summer",
                      "Bull Kelp Log" = ".pred"),
                    selected = ".pred"),
        br(), br(),
        h4("Mean of Selected Variable at All Sites Over Time"),
        br(),
        plotOutput(ns("timeSeriesPlot"), height = "400px")
      ),
      mainPanel(
        leafletOutput(ns("map"), height = 800)
      )
    )
  )
}

bull_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    bull_model <- read_csv("data/bull_northcentral_future_predictions.csv")
    
    mpa <- st_read("data/California_Marine_Protected_Areas_[ds582].shp") %>%
      st_transform(crs = 4326) %>% 
      mutate(centroid = st_centroid(geometry)) %>%
      filter(st_coordinates(centroid)[, 2] > 34.5)
    
    process_model_data <- function(model_name) {
      bull_model %>%
        filter(model == model_name) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    }
    
    bull_by_model <- list(
      gfdl = process_model_data("gfdl"),
      hadl = process_model_data("hadl"),
      ipsl = process_model_data("ipsl")
    )
    
    all_data <- do.call(rbind, bull_by_model)
    
    all_palettes <- list(
      depth = colorNumeric(viridisLite::mako(256, direction = -1), domain = all_data$depth, na.color = "transparent"),
      no3_surface_annual = colorNumeric("viridis", domain = all_data$no3_surface_annual, na.color = "transparent"),
      no3_surface_summer = colorNumeric("viridis", domain = all_data$no3_surface_summer, na.color = "transparent"),
      no3_mean_annual = colorNumeric("viridis", domain = all_data$no3_mean_annual, na.color = "transparent"),
      taualong_surface_annual = colorNumeric(viridisLite::plasma(256, direction = -1), domain = all_data$taualong_surface_annual, na.color = "transparent"),
      taualong_surface_upwelling = colorNumeric(viridisLite::plasma(256), domain = all_data$taualong_surface_upwelling, na.color = "transparent"),
      temp_surface_summer = colorNumeric(viridisLite::turbo(256), domain = all_data$temp_surface_summer, na.color = "transparent"),
      temp_mean_summer = colorNumeric(viridisLite::turbo(256), domain = all_data$temp_mean_summer, na.color = "transparent"),
      .pred = colorNumeric(viridisLite::turbo(256), domain = all_data$.pred, na.color = "transparent")
    )
    
    filtered_data <- reactive({
      df <- bull_by_model[[input$model]] %>%
        filter(year == input$year)
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -123, lat = 38, zoom = 7) %>%
        addPolygons(
          data = mpa,
          color = "#2b83ba",
          weight = 1,
          opacity = 1,
          fillOpacity = 0,
          label = ~paste0("MPA: ", NAME),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.1, bringToFront = TRUE),
          group = "MPAs"
        )
    })
    
    output$timeSeriesPlot <- renderPlot({
      df <- bull_by_model[[input$model]]
      
      if (nrow(df) == 0 || all(is.na(df[[input$variable]]))) return(NULL)
      
      ts_summary <- df %>%
        group_by(year) %>%
        summarise(
          mean_value = mean(.data[[input$variable]], na.rm = TRUE),
          sd_value = sd(.data[[input$variable]], na.rm = TRUE)
        )
      
      ggplot(ts_summary, aes(x = year, y = mean_value)) +
        geom_line(color = "#7570b3", size = 1) +
        geom_ribbon(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), alpha = 0.2, fill = "#7570b3") +
        labs(title = paste("Mean of", input$variable, "for", input$model, "Model"),
             x = "Year", y = input$variable) +
        geom_point(data = ts_summary %>% filter(year == input$year),
                   aes(x = year, y = mean_value), color = "#d95f02", size = 3) +
        geom_text(data = ts_summary %>% filter(year == input$year),
                  aes(x = year, y = mean_value, label = round(mean_value, 2)),
                  vjust = -1, color = "#d95f02", fontface = "bold") +
        scale_x_continuous(limits = c(2021, max(ts_summary$year, na.rm = TRUE))) +
        theme_minimal()
    })
    
    observe({
      df <- filtered_data()
      var_name <- input$variable
      
      if (nrow(df) == 0 || all(is.na(df[[var_name]]))) return()
      
      pal <- all_palettes[[var_name]]
      coords <- st_coordinates(df)
      
      leafletProxy(ns("map"), data = df) %>%
        clearGroup("nerlue_tiles") %>%
        clearControls() %>%
        addRectangles(
          lng1 = coords[, "X"] - 0.0165,
          lat1 = coords[, "Y"] - 0.0165,
          lng2 = coords[, "X"] + 0.0165,
          lat2 = coords[, "Y"] + 0.0165,
          color = "transparent",
          weight = 0,
          fillOpacity = 0.8,
          fillColor = ~pal(df[[var_name]]),
          label = ~paste0("Site: ", site, " ", "Depth: ", round(depth, 1), " ", "Log Bull Kelp density: ", round(.pred, 2)),
          group = "nerlue_tiles"
        ) %>%
        addLegend(
          pal = pal,
          values = all_data[[var_name]],
          title = input$variable,
          opacity = 1
        )
    })
  })
}
