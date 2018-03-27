library(shiny)
library(ggplot2)
library(ggmap)
library(leaflet)

# Define server logic
shinyServer(function(input, output) {

  output$maplocation <- renderLeaflet({

    if (input$upload == "system" & input$city_name == "Champaign, IL" & input$type == "Healthy/Unhealthy"){

      map_data = read.csv("../../extdata/champaign.csv", header=TRUE)

      if ((input$healthy == TRUE & input$unhealthy == TRUE) | (input$healthy == FALSE & input$unhealthy == FALSE)){

        if (input$class == "All"){

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == TRUE & input$unhealthy == FALSE){

        map_data = map_data[which(map_data$Initial.Ranking == "Healthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/veggie2.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == FALSE & input$unhealthy == TRUE){

        map_data = map_data[which(map_data$Initial.Ranking == "Unhealthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/fastfood.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "Champaign, IL" & input$type == "Health Rank"){

      map_data = read.csv("../../extdata/champaign.csv", header=TRUE)

      if (input$class == "All"){

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking > 7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "American"){

        map_data = subset(map_data, grepl("American", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Chinese"){

        map_data = subset(map_data, grepl("Chinese", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Mexican"){

        map_data = subset(map_data, grepl("Mexican", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Korean"){

        map_data = subset(map_data, grepl("Korean", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Italian"){

        map_data = subset(map_data, grepl("Italian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Indian"){

        map_data = subset(map_data, grepl("Indian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "South Haven, MI" & input$type == "Healthy/Unhealthy"){

      map_data = read.csv("../../extdata/southHaven.csv", header=TRUE)

      if ((input$healthy == TRUE & input$unhealthy == TRUE) | (input$healthy == FALSE & input$unhealthy == FALSE)){

        if (input$class == "All"){

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == TRUE & input$unhealthy == FALSE){

        map_data = map_data[which(map_data$Initial.Ranking == "Healthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/veggie2.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == FALSE & input$unhealthy == TRUE){

        map_data = map_data[which(map_data$Initial.Ranking == "Unhealthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/fastfood.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "South Haven, MI" & input$type == "Health Rank"){

      map_data = read.csv("../../extdata/southHaven.csv", header=TRUE)

      if (input$class == "All"){

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking > 7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "American"){

        map_data = subset(map_data, grepl("American", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Chinese"){

        map_data = subset(map_data, grepl("Chinese", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Mexican"){

        map_data = subset(map_data, grepl("Mexican", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Korean"){

        map_data = subset(map_data, grepl("Korean", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Italian"){

        map_data = subset(map_data, grepl("Italian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Indian"){

        map_data = subset(map_data, grepl("Indian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

    }


    else if (input$upload == "system" & input$city_name == "Hinsdale, IL" & input$type == "Healthy/Unhealthy"){

      map_data = read.csv("../../extdata/hinsdale.csv", header=TRUE)

      if ((input$healthy == TRUE & input$unhealthy == TRUE) | (input$healthy == FALSE & input$unhealthy == FALSE)){

        if (input$class == "All"){

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == TRUE & input$unhealthy == FALSE){

        map_data = map_data[which(map_data$Initial.Ranking == "Healthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/veggie2.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == FALSE & input$unhealthy == TRUE){

        map_data = map_data[which(map_data$Initial.Ranking == "Unhealthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/fastfood.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "Hinsdale, IL" & input$type == "Health Rank"){

      map_data = read.csv("../../extdata/hinsdale.csv", header=TRUE)

      if (input$class == "All"){

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking > 7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "American"){

        map_data = subset(map_data, grepl("American", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Chinese"){

        map_data = subset(map_data, grepl("Chinese", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Mexican"){

        map_data = subset(map_data, grepl("Mexican", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Korean"){

        map_data = subset(map_data, grepl("Korean", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Italian"){

        map_data = subset(map_data, grepl("Italian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Indian"){

        map_data = subset(map_data, grepl("Indian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "Charleston, IL" & input$type == "Healthy/Unhealthy"){

      map_data = read.csv("../../extdata/charleston.csv", header=TRUE)

      if ((input$healthy == TRUE & input$unhealthy == TRUE) | (input$healthy == FALSE & input$unhealthy == FALSE)){

        if (input$class == "All"){

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == TRUE & input$unhealthy == FALSE){

        map_data = map_data[which(map_data$Initial.Ranking == "Healthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/veggie2.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

      else if (input$healthy == FALSE & input$unhealthy == TRUE){

        map_data = map_data[which(map_data$Initial.Ranking == "Unhealthy"),]

        restIcons <- makeIcon(
          iconUrl = "../../extdata/fastfood.png",
          iconWidth = 24, iconHeight = 24)

        if (input$class == "All"){

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "American"){

          map_data = subset(map_data, grepl("American", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Chinese"){

          map_data = subset(map_data, grepl("Chinese", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Mexican"){

          map_data = subset(map_data, grepl("Mexican", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Korean"){

          map_data = subset(map_data, grepl("Korean", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Italian"){

          map_data = subset(map_data, grepl("Italian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

        else if (input$class == "Indian"){

          map_data = subset(map_data, grepl("Indian", map_data$Type))

          leaflet(data=map_data) %>% addTiles() %>%
            addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

        }

      }

    }

    else if (input$upload == "system" & input$city_name == "Charleston, IL" & input$type == "Health Rank"){

      map_data = read.csv("../../extdata/charleston.csv", header=TRUE)

      if (input$class == "All"){

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking > 7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "American"){

        map_data = subset(map_data, grepl("American", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Chinese"){

        map_data = subset(map_data, grepl("Chinese", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Mexican"){

        map_data = subset(map_data, grepl("Mexican", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Korean"){

        map_data = subset(map_data, grepl("Korean", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Italian"){

        map_data = subset(map_data, grepl("Italian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Indian"){

        map_data = subset(map_data, grepl("Indian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

    }

    else if (input$upload == "upload" & input$type == "Healthy/Unhealthy"){

      inFile = input$file_upload

      if (is.null(inFile))
        return(NULL)

      else {

        map_data = read.csv(inFile$datapath, header = TRUE)

        if ((input$healthy == TRUE & input$unhealthy == TRUE) | (input$healthy == FALSE & input$unhealthy == FALSE)){

          restIcons <- icons(
            iconUrl = ifelse(map_data$Initial.Ranking == "Unhealthy",
                             "../../extdata/fastfood.png",
                             "../../extdata/veggie2.png"),
            iconWidth = 24, iconHeight = 24)

          if (input$class == "All"){

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "American"){

            map_data = subset(map_data, grepl("American", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Chinese"){

            map_data = subset(map_data, grepl("Chinese", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Mexican"){

            map_data = subset(map_data, grepl("Mexican", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Korean"){

            map_data = subset(map_data, grepl("Korean", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Italian"){

            map_data = subset(map_data, grepl("Italian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Indian"){

            map_data = subset(map_data, grepl("Indian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

        }

        else if (input$healthy == TRUE & input$unhealthy == FALSE){

          map_data = map_data[which(map_data$Initial.Ranking == "Healthy"),]

          restIcons <- makeIcon(
            iconUrl = "../../extdata/veggie2.png",
            iconWidth = 24, iconHeight = 24)

          if (input$class == "All"){

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "American"){

            map_data = subset(map_data, grepl("American", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Chinese"){

            map_data = subset(map_data, grepl("Chinese", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Mexican"){

            map_data = subset(map_data, grepl("Mexican", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Korean"){

            map_data = subset(map_data, grepl("Korean", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Italian"){

            map_data = subset(map_data, grepl("Italian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Indian"){

            map_data = subset(map_data, grepl("Indian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

        }

        else if (input$healthy == FALSE & input$unhealthy == TRUE){

          map_data = map_data[which(map_data$Initial.Ranking == "Unhealthy"),]

          restIcons <- makeIcon(
            iconUrl = "../../extdata/fastfood.png",
            iconWidth = 24, iconHeight = 24)

          if (input$class == "All"){

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "American"){

            map_data = subset(map_data, grepl("American", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Chinese"){

            map_data = subset(map_data, grepl("Chinese", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Mexican"){

            map_data = subset(map_data, grepl("Mexican", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Korean"){

            map_data = subset(map_data, grepl("Korean", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Italian"){

            map_data = subset(map_data, grepl("Italian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

          else if (input$class == "Indian"){

            map_data = subset(map_data, grepl("Indian", map_data$Type))

            leaflet(data=map_data) %>% addTiles() %>%
              addMarkers(~address_long,~address_lat, popup=~pop_ups, icon = restIcons)

          }

        }
      }
    }

    else if (input$upload == "upload" & input$type == "Health Rank"){

      inFile = input$file_upload

      if (is.null(inFile))
        return(NULL)

      map_data = read.csv(inFile$datapath, header = TRUE)

      if (input$class == "All"){

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "American"){

        map_data = subset(map_data, grepl("American", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Chinese"){

        map_data = subset(map_data, grepl("Chinese", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Mexican"){

        map_data = subset(map_data, grepl("Mexican", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Korean"){

        map_data = subset(map_data, grepl("Korean", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Italian"){

        map_data = subset(map_data, grepl("Italian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

      else if (input$class == "Indian"){

        map_data = subset(map_data, grepl("Indian", map_data$Type))

        if (input$rank == "All"){

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Below 3"){

          map_data = map_data[which(map_data$Health.Ranking <= 3),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Between 3 and 7"){

          map_data = map_data[which((map_data$Health.Ranking > 3) & (map_data$Health.Ranking <= 7)),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

        else if (input$rank == "Above 7"){

          map_data = map_data[which(map_data$Health.Ranking >  7),]

          getColor <- function(map_data) {
            sapply(map_data$Health.Ranking, function(Health.Ranking) {
              if(Health.Ranking <= 3) {
                "red"
              } else if((Health.Ranking > 3) & (Health.Ranking <= 7)) {
                "orange"
              } else if (Health.Ranking > 7){
                "green"
              } })
          }
          icons <- awesomeIcons(
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(map_data))

          leaflet(data=map_data) %>% addTiles() %>%
            addAwesomeMarkers(~address_long, ~address_lat, icon=icons, popup = ~pop_ups)

        }

      }

    }

    else if (input$upload == "typein"){

      return(NULL)

    }

  })

  output$mytable <- renderDataTable({

    if (input$upload == "system"){

      return(NULL)

    }

    else if (input$upload == "upload"){

      return(NULL)

    }

    else if (input$upload == "typein"){

      if (input$city_name_data == "Champaign, IL"){

        table_data = read.csv("../../extdata/champaign.csv", header = TRUE)

        filtered_data = table_data[,c(1,2,8,9,6)]

        filtered_data

      }

      else if (input$city_name_data == "Hinsdale, IL"){

        table_data = read.csv("../../extdata/hinsdale.csv", header = TRUE)

        filtered_data = table_data[,c(1,2,8,9,6)]

        filtered_data

      }

      else if (input$city_name_data == "Charleston, IL"){

        table_data = read.csv("../../extdata/charleston.csv", header = TRUE)

        filtered_data = table_data[,c(1,2,8,9,6)]

        filtered_data

      }

      else if (input$city_name_data == "South Haven, MI"){

        table_data = read.csv("../../extdata/southHaven.csv", header = TRUE)

        filtered_data = table_data[,c(1,2,8,9,6)]

        filtered_data

      }

    }



  })

})
