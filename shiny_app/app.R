library(shiny)
source('inner_code/function.R')
source('inner_code/myUI.R')
source('inner_code/myServer.R')


shinyApp(
  ui = myui,
  server = myserver
)