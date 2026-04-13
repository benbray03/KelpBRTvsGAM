bull_5_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Bull Kelp"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("year"), "Select Year:",
                    min = 2005, max = 2100, value = 2025, step = 5, sep = ""),
        selectInput(ns("variable"), "Select Variable to Map:",
                    choices = c(
                      "Bull Kelp Log"             = "nerlue_by_rock",
                      "Surface Nitrate (Summer)"  = "no3_surface_summer",
                      "Along-shore Wind Stress"   = "taualong_surface_upwelling",
                      "Cross-shore Wind Stress"   = "taucross_surface_upwelling",
                      "Temp at Deepest Upwelling" = "temp_deepest_upwelling",
                      "Depth"                     = "depth",
                      "Urchin Log"                = "urchin_log"
                    ),
                    selected = "nerlue_by_rock"),
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

bull_5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    bull_model <- read_csv("brt_projections_by_rock/bull_nc_future_projections_300res_ensemble_5yr.csv")
    ts_data    <- read_csv("brt_projections_by_rock/bull_nc_5yr_barplot.csv")
    
    mpa <- st_read("shp/California_Marine_Protected_Areas_[ds582].shp") %>%
      st_transform(crs = 4326) %>% 
      mutate(centroid = st_centroid(geometry)) %>%
      filter(st_coordinates(centroid)[, 2] > 34.5)
    
    all_palettes <- list(
      nerlue_by_rock = colorNumeric(viridisLite::turbo(256), domain = bull_model$nerlue_by_rock,            na.color = "transparent"),
      prob_rock     = colorNumeric(viridisLite::turbo(256), domain = bull_model$prob_rock,                 na.color = "transparent"),
      no3_surface_summer  = colorNumeric(viridisLite::turbo(256), domain = bull_model$no3_surface_summer,   na.color = "transparent"),
      taualong_surface_upwelling = colorNumeric(
        scales::div_gradient_pal(low="#2166ac", mid="white", high="#d73027")(seq(0,1,length=256)),domain = bull_model$taualong_surface_upwelling, na.color = "transparent"),
      taucross_surface_upwelling = colorNumeric(scales::div_gradient_pal(low="#2166ac", mid="white", high="#d73027")(seq(0,1,length=256)), domain = bull_model$taucross_surface_upwelling,
        na.color = "transparent"
      ),
      temp_deepest_upwelling = colorNumeric(viridisLite::turbo(256),domain = bull_model$temp_deepest_upwelling,na.color = "transparent"),
      depth = colorNumeric(viridisLite::mako(256, direction = -1),    domain = bull_model$depth,na.color = "transparent"),
      urchin_log = colorNumeric(viridisLite::turbo(256),domain = bull_model$urchin_log,                na.color = "transparent")
    )
    
    filtered_data <- reactive({
      bull_model %>%
        filter(year == input$year) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
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
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                      textsize = "13px", direction = "auto"),
          highlightOptions = highlightOptions(weight = 2, color = "#666", 
                                              fillOpacity = 0.1, bringToFront = TRUE),
          group = "MPAs"
        ) %>%
        addLayersControl(                          # move layers control here so it's static too
          overlayGroups = c("MPAs", "nerlue_tiles"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    output$timeSeriesPlot <- renderPlot({
      scale_factor <- max(ts_data$total_abundance, na.rm = TRUE) / max(ts_data$model_variance, na.rm = TRUE)
      
      ggplot(ts_data, aes(x = year_bin, y = total_abundance)) +
        geom_col(fill = "#7570b3", alpha = 0.8, width = 4) +
        geom_errorbar(
          aes(
            ymin = total_abundance - (model_variance * scale_factor),
            ymax = total_abundance + (model_variance * scale_factor)
          ),
          width = 2,
          color = "#d95f02",
          linewidth = 0.8
        ) +
        scale_x_continuous(breaks = seq(2005, 2100, by = 5)) +
        labs(
          title = "Total Bull Kelp Abundance by 5-Year Period",
          x = "Year",
          y = "Total Abundance",
          caption = "Error bars = inter-model variance"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    observe({
      df <- filtered_data()
      var_name <- input$variable
      
      if (nrow(df) == 0 || all(is.na(df[[var_name]]))) return()
      
      pal <- all_palettes[[var_name]]
      coords <- st_coordinates(df)
      
      labels <- paste0(
        "Site ID: ", df[["site_id"]],
        " | Cell ID: ", df[["cell_id"]],
        " | ", var_name, ": ", round(df[[var_name]], 3)
      )
      
      leafletProxy(ns("map")) %>%        # no `data =` here — pass coords explicitly below
        clearGroup("nerlue_tiles") %>%   # only clears points, MPAs untouched
        clearControls() %>%
        addCircleMarkers(
          lng = coords[, "X"],
          lat = coords[, "Y"],
          radius = 4,
          color = "transparent",
          weight = 0,
          fillOpacity = 0.8,
          fillColor = pal(df[[var_name]]),
          label = labels,
          group = "nerlue_tiles"
        ) %>%
        addLegend(
          pal = pal,
          values = bull_model[[var_name]],
          title = var_name,
          opacity = 1,
          layerId = "legend"             # KEY: named legend gets replaced, not appended
        )
    })
  })
}
