
    # Create User Interface
    ui <- navbarPage(
      "Future Mapping NYC",   
      tabPanel("Home", 
               fluidRow(
                 column(c(212), 
                        Title = "Title", 
                        h4("Some words go here")), 
                 selectizeInput(
                   inputId = "tab1_id",
                   label = "Label",
                   choices = unique(df$clean_choices),
                   selected = df$clean_choices[[1]], 
                   multiple = F), 
                 checkboxInput(
                   inputId = "tab1_checkbox_id", 
                   label = "Label", 
                   choices = unique(df$clean_choices), 
                   value = F
                 )
               )
              )
    )

    # Link UI with Reactive Server
    server <- function(input, output, session) {
      output$mymap <- renderLeaflet({
        leaflet(NY_Geometries, options = 
                  leafletOptions(minZoom = min_zoom)) %>% 
          addTiles() %>%
          addPolygons(color = ~pal3(df_as_sf@data$value),
                      label = ~paste(
                        df_as_sf@data$geom_name, "</br>",
                        "Some Total", signif(df_as_sf@data$value, digits = 6)),
                      highlight = highlightOptions(weight = 1,
                                                   color = "black",
                                                   bringToFront = TRUE)) %>%
          addLegend(max_val_pal3:max_val_pal3, 
                    position = "bottomright", 
                    pal = pal3) %>% 
          addCircleMarkers(lng = tibble_points_1$long, 
                           lat = tibble_points_1$lat, 
                           popup = tibble_points_1$popup_name,
                           color = "purple", radius = 2) %>%
          addCircleMarkers(lng = tibble_points_2$long, 
                           lat = tibble_points_2$lat, 
                           popup = tibble_points_2$campground,
                           color = "green", radius = 2) %>%
          
          addCircleMarkers(lng = tibble_points_3_campgrounds$long, lat = tibble_points_3_campgrounds$lat, 
                           popup = tibble_points_3_campgrounds$campground,
                           color = "green", radius = 2) %>%
          
          
          addCircleMarkers(lng = tibble_points_4_watchsites$long, lat = tibble_points_4_watchsites$lat, 
                           popup = tibble_points_4_watchsites$site,
                           color = "orange", radius = 2)
        
      })
    }



