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
                    min = 2001, max = 2100, value = 2021, step = 1, sep = ""),
        selectInput(ns("variable"), "Select Variable to Map:",
                    choices = c(
                      "Bull Kelp Log"             = "nerlue_by_rock",
                      "Probability Rock"          = "prob_rock",
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

bull_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    bull_model <- arrow::read_parquet("brt_projections_by_rock/bull_northcentral_future_projections_300res.parquet")
    
    mpa <- st_read("shp/California_Marine_Protected_Areas_[ds582].shp") %>%
      st_transform(crs = 4326) %>% 
      mutate(centroid = st_centroid(geometry)) %>%
      filter(st_coordinates(centroid)[, 2] > 34.5)
    
    # Pass bull_model explicitly to avoid scoping issues
    process_model_data <- function(data, model_name) {
      data %>%
        filter(model == model_name) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    }
    
    bull_by_model <- list(
      gfdl     = process_model_data(bull_model, "gfdl"),
      hadl     = process_model_data(bull_model, "hadl"),
      ipsl     = process_model_data(bull_model, "ipsl")
    )
    
    all_data <- do.call(rbind, bull_by_model)
    
    all_palettes <- list(
      nerlue_by_rock            = colorNumeric(viridisLite::turbo(256),   domain = all_data$nerlue_by_rock,            na.color = "transparent"),
      prob_rock                 = colorNumeric(viridisLite::turbo(256),   domain = all_data$prob_rock,                 na.color = "transparent"),
      no3_surface_summer        = colorNumeric(viridisLite::turbo(256),   domain = all_data$no3_surface_summer,        na.color = "transparent"),
      taualong_surface_upwelling = colorNumeric(
        scales::div_gradient_pal(low="#2166ac", mid="white", high="#d73027")(seq(0,1,length=256)),
        domain = all_data$taualong_surface_upwelling,
        na.color = "transparent"
      ),
      taucross_surface_upwelling = colorNumeric(
        scales::div_gradient_pal(low="#2166ac", mid="white", high="#d73027")(seq(0,1,length=256)),
        domain = all_data$taucross_surface_upwelling,
        na.color = "transparent"
      ),
      temp_deepest_upwelling    = colorNumeric(viridisLite::turbo(256),   domain = all_data$temp_deepest_upwelling,    na.color = "transparent"),
      depth                     = colorNumeric(viridisLite::mako(256),    domain = all_data$depth,                     na.color = "transparent"),
      urchin_log                = colorNumeric(viridisLite::turbo(256),   domain = all_data$urchin_log,                na.color = "transparent")
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
      req(input$model, input$variable)
      
      df <- bull_by_model[[input$model]]
      
      if (is.null(df) || nrow(df) == 0 || all(is.na(df[[input$variable]]))) return(NULL)
      
      ts_summary <- df %>%
        st_drop_geometry() %>%          # important — group_by/summarise on sf objects can be slow or error
        group_by(year) %>%
        summarise(
          mean_value = mean(.data[[input$variable]], na.rm = TRUE),
          sd_value   = sd(.data[[input$variable]],   na.rm = TRUE)
        )
      
      ggplot(ts_summary, aes(x = year, y = mean_value)) +
        geom_ribbon(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                    alpha = 0.2, fill = "#7570b3") +
        geom_line(color = "#7570b3", size = 1) +
        geom_point(data = ts_summary %>% filter(year == input$year),
                   aes(x = year, y = mean_value), color = "#d95f02", size = 3) +
        geom_text(data = ts_summary %>% filter(year == input$year),
                  aes(x = year, y = mean_value, label = round(mean_value, 2)),
                  vjust = -1, color = "#d95f02", fontface = "bold") +
        scale_x_continuous(limits = c(2001, max(ts_summary$year, na.rm = TRUE))) +
        labs(title = paste(input$variable, "-", input$model, "model"),
             x = "Year", y = input$variable) +
        theme_minimal()
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
          values = all_data[[var_name]],
          title = var_name,
          opacity = 1,
          layerId = "legend"             # KEY: named legend gets replaced, not appended
        )
    })
    
    # observe({
    #   df <- filtered_data()
    #   var_name <- input$variable
    #   
    #   if (nrow(df) == 0 || all(is.na(df[[var_name]]))) return()
    #   
    #   pal <- all_palettes[[var_name]]
    #   coords <- st_coordinates(df)
    #   
    #   labels <- paste0(
    #     "Site ID: ", df[["site_id"]],
    #     " | Cell ID: ", df[["cell_id"]],
    #     " | ", var_name, ": ", round(df[[var_name]], 3)
    #   )
    #   
    #   leafletProxy(ns("map"), data = df) %>%
    #     clearGroup("nerlue_tiles") %>%
    #     clearControls() %>%
    #     addCircleMarkers(
    #       lng = coords[, "X"],
    #       lat = coords[, "Y"],
    #       radius = 2,
    #       color = "transparent",
    #       weight = 0,
    #       fillOpacity = 0.5,
    #       fillColor = pal(df[[var_name]]),
    #       label = labels,
    #       group = "nerlue_tiles"
    #     ) %>%
    #     # addRectangles(
    #     #   lng1 = coords[, "X"] - 0.00165,
    #     #   lat1 = coords[, "Y"] - 0.00165,
    #     #   lng2 = coords[, "X"] + 0.00165,
    #     #   lat2 = coords[, "Y"] + 0.00165,
    #     #   color = "transparent",
    #     #   weight = 0,
    #     #   fillOpacity = 0.8,
    #     #   fillColor = pal(df[[var_name]]),
    #     #   label = labels,
    #     #   group = "nerlue_tiles"
    #     # ) %>%
    #     addLegend(
    #       pal = pal,
    #       values = all_data[[var_name]],
    #       title = var_name,
    #       opacity = 1
    #     )
    # })
  })
}
