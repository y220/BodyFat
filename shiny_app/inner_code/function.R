f <- function(input) { 
  #weight
  weight = 0
  if(input$weight=='g'){ weight = (input$Weight)* 453.592}
  else if (input$weight=='kg') { weight = (input$Weight)*0.453592 }
  else  {weight = input$Weight}
  #height
  height = 0
  if(input$height=='cm'){ height = (input$Height)* 2.54}
  else if (input$height=='m') { height = (input$Height)*0.0254 }
  else  {height = input$Height}
  #chest
  chest = 0
  if(input$chest =='inch'){ chest = (input$Chest)* 0.393701}
  else  {chest = input$Chest}
  #abdomen
  abdomen = 0
  if(input$abdomen =='inch'){ abdomen = (input$Abdomen)* 0.393701}
  else  {abdomen = input$Abdomen}
  #hip
  hip = 0
  if(input$hip == 'inch'){ hip = (input$Hip)* 0.393701}
  else  { hip = input$Hip }
  #thigh
  thigh = 0
  if(input$thigh == 'inch'){ thigh = (input$Thigh)* 0.393701}
  else  { thigh = input$Thigh }
  result = 0
  result=0.02*(input$Age)-0.13*weight-0.26*height+0.18*(input$Adiposity)+0.30*chest+0.54*abdomen+0.11*hip+0.09*thigh-41.16 
  return( result )
}