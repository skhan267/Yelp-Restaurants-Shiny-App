library(shiny)
library(ggplot2)
library(ggmap)
library(leaflet)

# Define UI for application
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("maplocation", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                radioButtons(inputId = "upload",
                             label = "Choose One",
                             choices = c("Use System's Data" = "system", "Upload Your Own Data" = "upload", "Get Data for City" = "typein"),
                             selected = "system"),
                conditionalPanel(condition = "input.upload == 'system'",
                                 selectInput(inputId = "city_name",
                                             label = "Choose A City",
                                             choices = c("-Choose A City Below-","Champaign, IL", "Charleston, IL", "Hinsdale, IL", "South Haven, MI"),
                                             selected = "-Choose A City Below-")),
                conditionalPanel(condition = "((input.upload == 'system') | (input.upload == 'upload'))",
                                 selectInput(inputId = "type",
                                             label = "Map Type",
                                             choices = c("Healthy/Unhealthy", "Health Rank"),
                                             selected = "Health Rank")),
                conditionalPanel(condition = "((input.upload == 'system') | (input.upload == 'upload'))",
                                 selectInput(inputId = "class",
                                             label = "Restaurant Type",
                                             choices = c("All", "American", "Mexican", "Chinese", "Korean", "Italian", "Indian"),
                                             selected = "All")),
                conditionalPanel(condition = "(input.type == 'Healthy/Unhealthy') & ((input.upload == 'system') | (input.upload == 'upload'))",
                                 checkboxInput(inputId = "healthy",
                                               label = "Healthy Region",
                                               value = TRUE),
                                 checkboxInput(inputId = "unhealthy",
                                               label = "Unhealthy Region",
                                               value = TRUE)),
                conditionalPanel(condition = "(input.type == 'Health Rank') & ((input.upload == 'system') | (input.upload == 'upload'))",
                                 selectInput(inputId = "rank",
                                             label = "Choose Healthiness",
                                             choices = c("All", "Below 3", "Between 3 and 7", "Above 7"),
                                             selected = "All")),
                conditionalPanel(condition = "input.upload == 'upload'",
                                 fileInput(inputId = "file_upload",
                                           label = "Choose a CSV file",
                                           accept = ".csv")),
                conditionalPanel(condition = "input.upload == 'typein'",
                                 selectInput(inputId = "city_name_data",
                                             label = "Choose A data frame for a city",
                                             choices = c("-Choose a city below-", "Champaign, IL", "Hinsdale, IL", "Charleston, IL", "South Haven, MI"),
                                             selected = "-Choose a city below-")),
                conditionalPanel(condition = "input.upload == 'typein'",
                                 dataTableOutput('mytable'))


  )
))
