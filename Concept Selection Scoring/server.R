if(!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
# for editable table
if(!require(rhandsontable)) {
  install.packages("rhandsontable")
  library(rhandsontable)
}
# for rader plot
if(!require(fmsb)) {
  install.packages("fmsb")
  library(fmsb)
}
# Score Table data
# Set defaults
def_RWeight <- c(.2, .175, .175, .1, .15, .1, .1, 1)
def_MindHealth <- c(3, 3, 3, 3, 3, 3, 3, NA)
def_WR1 <- def_RWeight * def_MindHealth
def_WR1[8] <- sum(def_WR1[1:7])
def_Discover <- c(3, 3, 3, 4, 3, 4, 2, NA)
def_WR2 <- def_RWeight * def_Discover
def_WR2[8] <- sum(def_WR2[1:7])
def_Pathways <- c(4, 2, 1, 4, 2, 1, 3, NA)
def_WR3 <- def_RWeight * def_Pathways
def_WR3[8] <- sum(def_WR3[1:7])
def_HiS <- c(3, 3, 4, 2, 4, 4, 4, NA)
def_WR4 <- def_RWeight * def_HiS
def_WR4[8] <- sum(def_WR4[1:7])
def_RtH <- c(4, 2, 3, 2, 3, 3, 2, NA)
def_WR5 <- def_RWeight * def_RtH
def_WR5[8] <- sum(def_WR5[1:7])

# Initial Table
df <- data.frame(input_1 = def_RWeight,
                 input_2 = def_MindHealth,
                 input_3 = def_WR1,
                 input_4 = def_Discover,
                 input_5 = def_WR2,
                 input_6 = def_Pathways,
                 input_7 = def_WR3,
                 input_8 = def_HiS,
                 input_9 = def_WR4,
                 input_10 = def_RtH,
                 input_11 = def_WR5
)

rownames(df) <- c("Educational",
                  "Variety of Question styles",
                  "Interuser interaction",
                  "Ease of understanding Gameplay",
                  "Engagement",
                  "Competitiveness",
                  "Challenge",
                  "Total")

colnames(df) <- c("Relative weight",
                  "MindHealth Score (Benchmark)",
                  "Weighted Rating 1",
                  "Discover Score",
                  "Weighted Rating 2",
                  "Pathways Score",
                  "Weighted Rating 3",
                  "Health is Wealth Score",
                  "Weighted Rating 4",
                  "Road to Heal Score",
                  "Weighted Rating 5")

rating_index <- c(3, 5, 7, 9, 11)

# Results table
# Find initial rankings, recommended value, create table
rankings <- rank(-df["Total", c(rating_index)])
recommend <- ifelse(rankings == 1, "Yes", "No")
rank_df <- data.frame(rbind(rankings,recommend))
rownames(rank_df) <- c("Rank", 
                       "Recommended Game")
colnames(rank_df) <- c("MindHealth Game (Benchmark)",
                       "Discover Game",
                       "Pathways Game",
                       "Health is Wealth Game",
                       "Road to Heal Game")

function(input, output) {
  
  # Initialize reactive concept table
  concept_table <- reactive({df})
  
  # Take input values and create new dataframe
  edited_concept <- reactive({
    # check for changes
    if (is.null(input$score)) {
      return(concept_table())
    } else if (!identical(concept_table(),input$score)) {
      # convert updated table into dataframe
      calculate <- as.data.frame(hot_to_r(input$score))
      
      # calculate each weighted score
      calculate[c(1:7), rating_index] <- calculate[c(1:7), rating_index - 1] * calculate[c(1:7), 1]
      # calculate each final score
      calculate[8, rating_index] <- colSums(calculate[c(1:7), rating_index])
      # calculate overall weights percentage
      calculate[8, 1] <- sum(calculate[c(1:7), 1])
      # reassign values to prevent errors
      concept_table <<- reactive({calculate})
      return(calculate)
    }
  })
  
  # create interative table for score
  output$score <- renderRHandsontable({
    rhandsontable(edited_concept(), rowHeaderWidth = 250, colWidths = 75, rowHeights = 40, width = 1075) %>%
      hot_row(c(8,9,10), readOnly = TRUE) %>%
      hot_col(rating_index, readOnly = TRUE) %>%
      hot_col(col = "Relative weight", format = "0%") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      # check valid weights
      # check valid total
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (row == 7) {td.style.background = 'whitesmoke';}
               if (col == 0) {
               if (row <= 6) {
               if (value < 0 | value > 1) {
               td.style.background = 'pink';
               }
               } else if (row == 7) {
               if (value != 1){
               td.style.background = 'pink';
               }
               }
               } else if (col % 2 == 1) {
               if (row <= 6) {
               if (value < 0 | value > 5) {
               td.style.background = 'pink';
               }
               }
               } else if (col % 2 == 0) {
               td.style.background = 'whitesmoke';
               }
               }")
  })
  
  # Initialize reactive results table
  result_table <- reactive({rank_df})
  
  # Take input values and create new dataframes
  edited_result <- reactive({
    if (is.null(input$score)) {
      return(result_table())
    } else if (!identical(concept_table(),input$score)) {
      # get updated table values as dataframe
      concept <- as.data.frame(hot_to_r(input$score))
      calculate <- as.data.frame(hot_to_r(input$result))
      
      # calculate new ranking
      edited_rankings <- rank(-concept["Total", c(rating_index)])
      # convert Truth table to Yes/No
      edited_recommend <- ifelse(edited_rankings == 1, "Yes", "No")
      # Somehow can't get it to work unless make new dataframe
      calculate <- data.frame(rbind(edited_rankings, edited_recommend))
      rownames(calculate) <- c("Rank", 
                               "Recommended Game")
      colnames(calculate) <- c("MindHealth Game (Benchmark)",
                               "Discover Game",
                               "Pathways Game",
                               "Health is Wealth Game",
                               "Road to Heal Game")
      # reassign values to prevent errors
      result_table <<- reactive({calculate})
      return(calculate)
    }
  })
  
  # Display recommended game
  output$result <- renderRHandsontable({
    rhandsontable(edited_result(), rowHeaderWidth = 325, colWidths = 150, rowHeights = 40, width = 1075, readOnly = TRUE) %>%
      # Find recommended game
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value == 1 | value == 'Yes') {
               td.style.background = 'lightgreen';
               }
               }")
  })
  
  # radar plot of absolute scores (out of 5)
  output$radar <- renderPlot({
    if (is.null(input$score)) {
      return(result_table())
    } else if (!identical(concept_table(),input$score)) {
      # get updated table values as dataframe
      concept <- as.data.frame(hot_to_r(input$score))
      rad <- data.frame(t(concept[c(1:7), c(rating_index - 1)]))
      rownames(rad) <- c("MindHealth Game (Benchmark)",
                         "Discover Game",
                         "Pathways Game",
                         "Health is Wealth Game",
                         "Road to Heal Game")
      colnames(rad) <- c("Educational",
                         "Variety of Question styles",
                         "Interuser interaction",
                         "Ease of understanding Gameplay",
                         "Engagement",
                         "Competitiveness",
                         "Challenge")
      # add max and min for radar plot
      rad <- rbind(rep(5, 7) , rep(0, 7) , rad)
      # create colors for each game
      color_border = c(rgb(0.2, 0.5, 0.5, 0.9), #blue
                       rgb(0.8, 0.2, 0.5, 0.9), #red
                       rgb(0.7, 0.5, 0.1, 0.9), #brown-yellow
                       rgb(0.5, 0.8, 0.2, 0.95), #green
                       rgb(0.4, 0.1, 0.9, 0.9)) #purple
      color_fill = c(rgb(0.2, 0.5, 0.5, 0.1), #blue
                     rgb(0.8, 0.2, 0.5, 0.1), #red
                     rgb(0.7, 0.5, 0.1, 0.1), #brown-yellow
                     rgb(0.5, 0.8, 0.2, 0.15), #green
                     rgb(0.4, 0.1, 0.9, 0.1)) #purple
      
      radarchart(
        rad,
        axistype = 1,
        seg = 5,
        pcol = color_border,
        pfcol = color_fill,
        plwd = 3,
        plty = 1,
        cglcol = "grey",
        cglty = 2,
        axislabcol = "grey",
        caxislabels = seq(0, 5, 1),
        #label scaling
        vlcex = 1
      )
      legend(
        x = -1.8,
        y = 1.3,
        legend = rownames(rad[-c(1, 2),]),
        bty = "n",
        # colour legend symbols
        pch = 20,
        col = color_border,
        #text scaling
        cex = 1
      )
    }
  })
  
  # radar plot of weighted scores
  output$w_score <- renderPlot({
    if (is.null(input$score)) {
      return(result_table())
    } else if (!identical(concept_table(),input$score)) {
      # get updated table values as dataframe
      concept <- as.data.frame(hot_to_r(input$score))
      rad <- data.frame(t(concept[c(1:7), c(rating_index)]))
      rownames(rad) <- c("MindHealth Game (Benchmark)",
                         "Discover Game",
                         "Pathways Game",
                         "Health is Wealth Game",
                         "Road to Heal Game")
      colnames(rad) <- c("Educational",
                         "Variety of Question styles",
                         "Interuser interaction",
                         "Ease of understanding Gameplay",
                         "Engagement",
                         "Competitiveness",
                         "Challenge")
      # add max and min for radar plot
      rad <- rbind(rep(1, 7) , rep(0, 7) , rad)
      # create colors for each game
      color_border = c(rgb(0.2, 0.5, 0.5, 0.9), #blue
                       rgb(0.8, 0.2, 0.5, 0.9), #red
                       rgb(0.7, 0.5, 0.1, 0.9), #brown-yellow
                       rgb(0.5, 0.8, 0.2, 0.9), #green
                       rgb(0.4, 0.1, 0.9, 0.9)) #purple
      color_fill = c(rgb(0.2, 0.5, 0.5, 0.1), #blue
                     rgb(0.8, 0.2, 0.5, 0.1), #red
                     rgb(0.7, 0.5, 0.1, 0.1), #brown-yellow
                     rgb(0.5, 0.8, 0.2, 0.1), #green
                     rgb(0.4, 0.1, 0.9, 0.1)) #purple
      
      radarchart(
        rad,
        axistype = 1,
        seg = 5,
        pcol = color_border,
        pfcol = color_fill,
        plwd = 3,
        plty = 1,
        cglcol = "grey",
        cglty = 2,
        axislabcol = "grey",
        caxislabels = seq(0, 1, 0.2),
        #label scaling
        vlcex = 1
      )
      legend(
        x = -1.8,
        y = 1.3,
        legend = rownames(rad[-c(1, 2),]),
        bty = "n",
        # colour legend symbols
        pch = 20,
        col = color_border,
        #text scaling
        cex = 1
      )
    }
  })
}