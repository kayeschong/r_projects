if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
library(dplyr)
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
fluidPage(
  
  headerPanel("Monte carlo simulation for stock price"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Variables for simulation inputs
      titlePanel("Press Generate data first, then play animation"),
      
      hr(),
      
      numericInput(inputId = "mean", 
                   label = "mean:", 
                   min = 0, max = 10, 
                   value = 1),
      
      numericInput(inputId = "sd", 
                   label = "standard deviation:", 
                   min = 0, max = 5, 
                   value = 0.15),
      
      numericInput(inputId = "days", 
                   label = "days:", 
                   min = 1, max = 1000, 
                   value = 500),
      
      numericInput(inputId = "startPrice", 
                   label = "Start Price:", 
                   min = 0, max = 10, 
                   value = 1),
      
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 200,
                  value = 100),
      
      sliderInput(inputId = "repeats",
                  label = "Number of Repeats:",
                  min = 1,
                  max = 500,
                  value = 100),
      
      # Create separation to be neater
      hr(),
      
      # Button to load paramters and generate dataframe
      actionButton("make_df", 
                   "Generate data"
      )
    ),
    
    mainPanel(
      uiOutput("frameSlider"),
      plotOutput(outputId = "closeprice"),
      plotOutput(outputId = "distPlot")
      
    )
  )
)