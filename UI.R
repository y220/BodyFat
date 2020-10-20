library(shiny)
f <- function(x) { x[1]^2 + x[2]^3 }
ui <- fluidPage(
  titlePanel("BodyFat Calculator"),
  textOutput("Developer",container = h5),
  hr(),
  ## Code you should write. No unique answer
  textOutput("String",container = h5),
  fluidRow(
    column(2,
           numericInput("n1", label = h3("Age"), value = 5),
           selectInput("unit", "Unit of Measurement", 
              choices = c("year")),
           textOutput("Range1",container = h6)),
    column(2,
           numericInput("n2", label = h3("Height"), value = 2),
           selectInput("unit", "Unit of Measurement", 
              choices = c("rock", "pressure", "cars"))),
    column(2,
           numericInput("n2", label = h3("Weight"), value = 4),
           selectInput("unit", "Unit of Measurement", 
                       choices = c("rock", "pressure", "cars")))
    ),
  hr(),
  fluidRow(
  column(6,
         textOutput("Bodyfat",container=h3),
         verbatimTextOutput("value"))
  ),
  textOutput("Warning",container = h5)
)
server <- function(input, output) {
  # `value` will in the output
  output$Developer = renderText({
    "Developer: Yukun Fang  (yfang67@wisc.edu) "
  })
  output$String = renderText({
    "This app is used to calculate your percentage of bady fat by the following statistics of your body:"
  })
  output$Range1 =  renderText({
    "Range : 0~100"
  })
  output$Bodyfat = renderText({
    "BodyFat"
  })
  output$value = renderPrint({
    f(c(input$n1, input$n2))
  })
  output$Warning = renderText({
    "If BodyFat returns NA, it means your input is out of range. If you find any bug or question, please contact the deverloper."
  })
}
shinyApp(ui = ui, server = server)