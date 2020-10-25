myserver <- function(input, output) {
  url1 <- a("yfang67@wisc.edu", href="http://yfang67@wisc.edu")
  output$Developer1 = renderUI({
    tagList("Yukun Fang ", url1 ,": Design and create UI." )  
  })
  url2 <- a("mchen373@wisc.edu", href="http://mchen373@wisc.edu")
  output$Developer2 = renderUI({
    tagList("Mengkun Chen", url2 ,": Write the formula of the bodyfat." )  
  })  
  url3 <- a("yfang67@wisc.edu", href="http://yfang67@wisc.edu")
  # output$Developer3 = renderUI({
  #   tagList("Jiayi Shen ", url3 ,": Create different measurement units for different variables." )  
  # })  
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
