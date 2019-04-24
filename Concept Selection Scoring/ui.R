
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
# for editable table
if(!require(rhandsontable)){
  install.packages("rhandsontable")
  library(rhandsontable)
}
# for rader plot
if(!require(fmsb)){
  install.packages("fmsb")
  library(fmsb)
}

ui <- fluidPage(
  
  titlePanel("Concept Selection Process", windowTitle = "ESA R Shiny"),
  br(),
  
  sidebarLayout(
    mainPanel(width = 7,
              h2("Interactive Table"),
              h5("Change relative weights (0-1) and/or game scores (0-5). Cells will turn red if invalid values given."),
              rHandsontableOutput("score"),
              
              #h5("Results table"),
              rHandsontableOutput("result")
              
    ),
    sidebarPanel(
      width = 5,
      h4("Radar plot of absolute scores"),
      plotOutput('radar', width = 730, height = 600)
      
      #h5("Radar plot of weighted scores"),
      #plotOutput('w_score',  width = 730, height = 600)
    )
  )
)