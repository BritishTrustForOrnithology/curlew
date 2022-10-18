# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# # ====== Load packages  ============
# 
# package_details <- c("sf","tidyverse","RColorBrewer","viridisLite","rcartocolor","lubridate", "leaflet", "shiny")
# list.of.packages <- package_details
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages, library, character.only=TRUE)
# 
# seed_number <- 1
# 
# 
# 
# # ====== Load data  ============
# 
# # Define UI ----
# ui <- fluidPage(
# 
#   # Application title
#   titlePanel("WWRG tagged Curlew movements"),
# 
#   #Sidebar with a slider input for number of bins
#   # sidebarLayout(
#   #   position = "right",
#   #   sidebarPanel(
#   #     sliderInput("bins",
#   #                 "Number of bins:",
#   #                 min = 1,
#   #                 max = 8,
#   #                 value = 20),
#   #     
#   #     # add WWRG logo
#   #     img(src = "rstudio.png", height = 140, width = 400)
#   #   ),
# # 
# #     mainPanel(
# #       p("Data visualisation explorer for Wash Wader Ringing Group (WWRG) GPS-tagged Curlew. This visualisation shows an interactive map of all individuals.")
# #       )
# #   ),
# 
# absolutePanel(top = 10, right = 10,
#               dateRangeInput("daterange", "Date range:",
#                              start = "2021-10-10",
#                              end   = Sys.Date()
#                              ),
# ),
#   
#   leafletOutput("map", width = "100%", height = "100%"),
#   
# )
# 
# # Define server logic ----
# server <- function(input, output) {
#   
#   all_tags <- read.csv("movebank_wwrg_curlew_20211026.csv", header = TRUE, stringsAsFactors = FALSE)
#   all_tags$new_datetime <- as.POSIXct(strptime(all_tags$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
#   all_tags$new_datetime_min <- format(all_tags$new_datetime,format='%Y-%m-%d %H:%M')
# 
#   first_date <- min(all_tags$new_datetime) %>% as.Date()
#   last_date <- max(all_tags$new_datetime) %>% as.Date()
# 
#   tide_dt <- read.csv("Tides_Bulldog_Bcn_20210929_20211029_0.csv", header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12]
#   names(tide_dt) <- c("site_name", "timestamp", "observed_m", "predicted_m", "surge_m", "msl_m", "residual_m", "sd_m", "status", "quality_percent", "quality_flag")
#   tide_dt$new_datetime <- as.POSIXct(strptime(tide_dt$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
#   tide_dt$new_datetime_min <- format(tide_dt$new_datetime,format='%Y-%m-%d %H:%M')
# 
#   tide_dt <- tide_dt %>%
#     filter(new_datetime >= first_date & new_datetime <= last_date)
# 
#   tag_tide_merged_dt <- merge(all_tags, tide_dt, by = "new_datetime_min", all.x = TRUE) %>% filter(!is.na(observed_m))
# 
#   # filter to date input
#   # Reactive expression for the data subsetted to what the user selected
#   filteredData <- reactive({
#     all_tags[all_tags$new_datetime >= input$daterange[1] & all_tags$new_datetime <= input$daterange[2],]
#   })
# 
#   # 1. split dataset into list by tag;
#   # 2. convert each tag dataset to a points sf object
#   dt_map_list <-
#     split(all_tags, all_tags$individual.local.identifier, drop=TRUE) #%>%
#   # lapply(., function(d) st_as_sf(d, coords = c("location.long","location.lat"), crs=4326))
# 
# 
#   # -----  set colour palette  ------
# 
#   make.palette <- function(categories) {
#     n_all_categories <- length(categories)
#     pal_all <- c(viridisLite::viridis(n_all_categories))
#     return(pal_all)
#   }
# 
#   all_indvs <- unique(all_tags$individual.local.identifier)
#   col_pal <- make.palette(all_indvs)
# 
# 
#   # -----  create the leaflet mapping object  ------
# 
#   output$map <- renderLeaflet({
# 
# 
#     # create base map
#     leaflet_map <- leaflet(options = leafletOptions(
#       zoomSnap = 0.5,
#       zoomDelta = 0.5
#     )) %>%
#       addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB Positron")  %>%
#       addProviderTiles("CartoDB.Positron", group = "CartoDB Positron with labels")  %>%
#       addProviderTiles("CartoDB.DarkMatterNoLabels", group="CartoDB Dark Matter") %>%
#       addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>%
#       addTiles(group = "OpenStreetMap")
# 
#     sapply(names(dt_map_list), function(df) {
# 
#       # sets unique colour for each list element according to pre-defined palette
#       pal_colour <- col_pal[which(all_indvs %in% df)]
# 
# 
#       leaflet_map <<- leaflet_map %>%
#         addCircleMarkers(data = dt_map_list[[df]],
#                          lng = dt_map_list[[df]]$location.long,
#                          lat = dt_map_list[[df]]$location.lat,
#                          # group = paste(df, "points"),
#                          group = paste(df),
#                          radius = 1,
#                          fill = TRUE,
#                          fillColor = pal_colour,
#                          fillOpacity = 0.8,
#                          stroke = TRUE,
#                          color = pal_colour,
#                          label = ~new_datetime,
#                          options = popupOptions(closeButton = TRUE)
#         ) %>%
#         addPolylines(data = dt_map_list[[df]],
#                      lng = dt_map_list[[df]]$location.long,
#                      lat = dt_map_list[[df]]$location.lat,
#                      group = paste(df),
#                      color = pal_colour,
#                      weight = 2
#         )
# 
#     })
# 
# 
#     # add extra controls for layers, legends, etc
#     leaflet_map <- leaflet_map %>%
#       addLayersControl(
#         baseGroups = c("CartoDB Positron", "CartoDB Positron with labels", "CartoDB Dark Matter", "Esri WorldImagery", "OpenStreetMap"),
#         overlayGroups = c(names(dt_map_list)),
#         options = layersControlOptions(collapsed = FALSE)
#       ) %>%
#       addLegend("bottomleft",
#                 colors = col_pal,
#                 labels = all_indvs,
#                 title = "Tag ID & colour marks",
#                 opacity = 5
#       )
#       })
# 
# }
# 
# # Run the app ----
# shinyApp(ui = ui, server = server)


# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)

# library(shiny)
# library(leaflet)
# 
# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()
# 
# ui <- fluidPage(
#   leafletOutput("mymap"),
#   p(),
#   actionButton("recalc", "New points")
# )
# 
# server <- function(input, output, session) {
#   
#   points <- eventReactive(input$recalc, {
#     cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
#   }, ignoreNULL = FALSE)
#   
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$Stamen.TonerLite,
#                        options = providerTileOptions(noWrap = TRUE)
#       ) %>%
#       addMarkers(data = points())
#   })
# }
# 
# shinyApp(ui, server)

library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateRangeInput("daterange", "Date range:",
                               start = "2021-10-10",
                               end   = Sys.Date()
                )
  )
)


all_tags <- read.csv("movebank_wwrg_curlew_20211026.csv", header = TRUE, stringsAsFactors = FALSE)
all_tags$new_datetime <- as.POSIXct(strptime(all_tags$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

# first_date <- "2021-10-22"
# last_date <- "2021-10-25"
# 
# all_tags <- all_tags %>%
#   filter(new_datetime >= first_date & new_datetime <= last_date)




server <- function(input, output, session) {
  
  # # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   all_tags[all_tags$new_datetime >= input$daterange[1] & all_tags$new_datetime <= input$daterange[2],]
  # })
  # 
  factpal <- colorFactor(viridisLite::viridis(10), as.factor(all_tags$individual.local.identifier))
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    mymap <- leaflet(all_tags) %>% 
      addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB Positron")  %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB Positron with labels")  %>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group="CartoDB Dark Matter") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      fitBounds(~min(location.long), ~min(location.lat), ~max(location.long), ~max(location.lat))
    
    for(i in unique(all_tags$individual.local.identifier)) {
      sub_dt <- all_tags[all_tags$individual.local.identifier %in% i,] # subset the data based on each type
      mymap <- mymap %>%

        addCircleMarkers(
          data = sub_dt,
          lng = ~location.long,
          lat = ~location.lat,
          # group = paste(df, "points"),
          group = i,
          radius = 1,
          fill = TRUE,
          fillColor = ~factpal(individual.local.identifier),
          fillOpacity = 0.8,
          stroke = TRUE,
          color = ~factpal(individual.local.identifier),
          label = ~new_datetime,
          options = popupOptions(closeButton = TRUE)
        )

    }


    mymap %>% addLayersControl(
      baseGroups = c("CartoDB Positron", "CartoDB Positron with labels", "CartoDB Dark Matter", "Esri WorldImagery", "OpenStreetMap"),
      overlayGroups = unique(all_tags$individual.local.identifier),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
      addLegend("bottomleft",
                pal = factpal,
                values = ~individual.local.identifier,
                title = "Tag ID & colour marks",
                opacity = 5
      )
    
  })
  
  # factpal <- colorFactor(viridisLite::viridis(10), as.factor(all_tags$individual.local.identifier))
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  
  # observe({
  #   
  #   
  #   for(i in unique(filteredData()$individual.local.identifier)) {
  #     
  #     sub_dt <- filteredData()[filteredData()$individual.local.identifier %in% i,] # subset the data based on each type
  #     
  #     leafletProxy("map", data = filteredData()) %>%
  #       clearShapes() %>%
  #       
  #       # addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #       #            fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #       # ) %>%
  #       
  #       addCircleMarkers(
  #         lng = ~location.long,
  #         lat = ~location.lat,
  #         group = i,
  #         radius = 1,
  #         fill = TRUE,
  #         fillColor = ~factpal(individual.local.identifier),
  #         fillOpacity = 0.8,
  #         stroke = TRUE,
  #         color = ~factpal(individual.local.identifier),
  #         label = ~new_datetime,
  #         options = popupOptions(closeButton = TRUE)
  #       ) 
  #     # %>%
  #     #   
  #     #   addLayersControl(
  #     #     baseGroups = c("CartoDB Positron", "CartoDB Positron with labels", "CartoDB Dark Matter", "Esri WorldImagery", "OpenStreetMap"),
  #     #     overlayGroups = c(levels(as.factor(all_tags$individual.local.identifier))),
  #     #     options = layersControlOptions(collapsed = FALSE)
  #     #   ) #%>%
  #     # addLegend("bottomleft",
  #     #           colors = col_pal,
  #     #           labels = all_indvs,
  #     #           title = "Tag ID & colour marks",
  #     #           opacity = 5
  #     # )
  #   }
  #     
  #     
  #   })
  
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
  }
  
  shinyApp(ui, server)
  