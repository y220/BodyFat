library(shiny)
f <- function(input) { 
    #if conditioning
    result=0;
    result=0.02*(input$Age)-0.13*(input$Weight)-0.26*(input$Height)+0.18*(input$Adiposity)+0.30*(input$Chest)+0.54*(input$Abdomen)+0.11*(input$Hip)+0.09*(input$Thigh)-41.16 
    return(result)
}
ui <- fluidPage(
  titlePanel("BodyFat Calculator"),
  hr(),
  uiOutput("Developer1",container = h5),
  uiOutput("Developer2",container = h5),
  uiOutput("Developer3",container = h5),
  hr(),
  ## Code you should write. No unique answer
  textOutput("String",container = h5),
  fluidRow(
    column(3,
           numericInput("Age", label = h3("Age"), value = 23),
           selectInput(inputId = "age", "Unit of Measurement", 
              choices = c("year"))),
    column(3,
           numericInput("Weight", label = h3("Weight"), value = 154.25),
           selectInput("weight", "Unit of Measurement", 
              choices = c("lbs", "kg", "g"))),
    column(3,
           numericInput("Height", label = h3("Height"), value = 67.75),
           selectInput("height", "Unit of Measurement", 
                       choices = c("inch", "cm", "m"))),
    column(3,
           numericInput("Adiposity", label = h3("Adiposity"), value = 23.7),
           selectInput("adiposity", "Unit of Measurement", 
                       choices = c("bmi"))),
    column(3,
           numericInput("Chest", label = h3("Chest"), value = 93.1),
           selectInput("chest", "Unit of Measurement", 
                       choices = c("cm","inch"))),
    column(3,
           numericInput("Abdomen", label = h3("Abdomen"), value = 85.2),
           selectInput("abdomen", "Unit of Measurement", 
                       choices = c("cm","inch" ))),
    column(3,
           numericInput("Hip", label = h3("Hip"), value = 94.5),
           selectInput("hip", "Unit of Measurement", 
                       choices = c( "cm","inch"))),
    column(3,
           numericInput("Thigh", label = h3("Thigh"), value = 59),
           selectInput("thigh", "Unit of Measurement", 
                       choices = c("cm","inch"))),
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
  url1 <- a("yfang67@wisc.edu", href="http://yfang67@wisc.edu")
  output$Developer1 = renderUI({
    tagList("Yukun Fang ", url1 ,": Design and create UI." )  
  })
  url2 <- a("mchen373@wisc.edu", href="http://mchen373@wisc.edu")
  output$Developer2 = renderUI({
    tagList("Mengkun Chen", url2 ,": Write the formula of the bodyfat." )  
  })  
  url3 <- a("yfang67@wisc.edu", href="http://yfang67@wisc.edu")
  output$Developer3 = renderUI({
    tagList("Jiayi Shen ", url3 ,": Create different measurement units for different variables." )  
  })  
  output$String = renderText({
    "This app is used to calculate your percentage of bady fat by the following statistics of your body:"
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
  output$value = renderPrint({
    f(input)
  })
}
shinyApp(ui = ui, server = server)