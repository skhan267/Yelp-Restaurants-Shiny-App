#' Health Rank Shiny App
#'
#' Run the Health Rank Application in Shiny Platform

runHealthrankapp <- function (){
  shiny::runApp(
    system.file('shiny-apps', 'healthrankapp',
                package='healthrank'))
}

