function(input, output) {p

  colours <- reactive(
    rep(rainbow(5), input$repeats/5 + 1)[1:input$repeats]
    )
  
  # function to generate dataframes
  generate_changes <- function(start_price, days, mean, sd) {
    changes <- rnorm(days,mean=mean,sd=sd)
    price <- cumprod(c(start_price, changes))[-1]
    day <- seq(days)
    df <- data.frame(day, price, changes)
    return(df)
  }
  
  # dataframe generater when make_df button is pressed. Formats to allow plotting by ggplot
  new_df <- eventReactive(
    eventExpr = input$make_df,
    valueExpr = {
      df <- data.frame(NULL)
      for (i in 1:input$repeats) {
            df = rbind(df, generate_changes(input$startPrice, input$days, input$mean, input$sd))
      }
      return(df)
    }
  )
  
  # Slider that allows for animation
  output$frameSlider <- renderUI(sliderInput("frame", label= h3("Run animation"), 
                                              min=1, max = input$repeats, 
                                              value=1, step=1, sep=NULL,
                                              animate = animationOptions(interval = 100)
                                              )
                                 )
  
  # something online to smoothen frames, increase debounce value to make smoother (but more skips)
  frame <- reactive(input$frame) %>% debounce(20)
  
  # initialize plot
  close <- reactiveValues()
  close$plot <- ggplot(mapping = aes(x = day, y = price)) + 
    labs(title="BAYZ closing price (sample path)") +
    theme_bw()
  
  # generate closeprice plot, TODO: find a way to reset plot when animation ends, currently keeps adding layers
  output$closeprice <- renderPlot( {
    req(new_df())
    close$plot <- isolate(close$plot) + geom_line(data = new_df()[c(1:input$days + (frame()-1)*input$days),], colour=colours()[frame()]) #does not reassign to plot above
    close$plot + annotate("text", x= -Inf, y= Inf, hjust = -1, vjust = 2, label=paste("plot no: ", frame()),size=6)
    }
    )
  
  # generate histogram of prices
  output$distPlot <- renderPlot({
    req(new_df())
    ggplot(data=new_df()[seq(1, nrow(new_df()), input$days),], aes(x=price)) + 
      geom_histogram(color="black", fill="white", bins=input$bins)
  })
  
}

