# Farkel
########
library(magrittr)
library(ggplot2)

# Functions
###########

# Calculate score of rolling six dice
scoreGame <- function(dice) {
  # Create dice roll
  roll_freq <- c(dice[1], dice[2], dice[3], dice[4], dice[5], dice[6]) %>%
    factor(levels=1:6) %>%
    table() %>%
    as.vector()
  # Score the dice roll
  score <- ifelse(sum(roll_freq > 5) == 1, 3000, # Six of a kind (zero left over)
          +ifelse(sum(roll_freq > 4) == 1, 2000, # Five of a kind (one left over)
          +ifelse(sum(roll_freq > 3) == 1, 1000, # Four of a kind (two left over)
          +ifelse(sum(roll_freq > 2) == 2, 2500, # Two triplets (zero left over)
          +ifelse(sum(roll_freq > 1) == 3, 1500, # Three pairs (zero left over)
          +ifelse(sum(roll_freq > 0) == 6, 1500, # Straight 1-6 (zero left over)
          +ifelse(sum(roll_freq[1] == 3), 300,   # Three ones (three left over)
          +sum(seq(6) * (roll_freq == 3) * 100))))))))      # Three of a kind   (three left over)     
  score <- score + ifelse(sum(roll_freq > 0) == 6, 0, ifelse(roll_freq[1] < 3, roll_freq[1] * 100, 0) + ifelse(roll_freq[5] < 3, roll_freq[5] * 50, 0))      # Ones and fives  
}

# Create a vector of only those dice in a roll that contribute to the score
onlyScoringDice <- function(dice) {
  # Create dice roll
  roll_freq <- c(dice[1], dice[2], dice[3], dice[4], dice[5], dice[6]) %>%
    factor(levels=1:6) %>%
    table() %>%
    as.vector()
  # Count non-scoring dice
  scorers <- vector()
  if(sum(roll_freq > 5) == 1 |
     +sum(roll_freq > 2) == 2 | 
     +sum(roll_freq > 1) == 3 |
     +sum(roll_freq > 0) == 6) {scorers = roll_freq}
  if(sum(scorers) == 0) {
    scorers[1] = roll_freq[1] 
    scorers[2] = ifelse(roll_freq[2] >= 3, roll_freq[2], 0)
    scorers[3] = ifelse(roll_freq[3] >= 3, roll_freq[3], 0)
    scorers[4] = ifelse(roll_freq[4] >= 3, roll_freq[4], 0)
    scorers[5] = roll_freq[5] 
    scorers[6] = ifelse(roll_freq[6] >= 3, roll_freq[6], 0)
  }
  scorers
}

# Create vector of dice rolls from a frequency table of a roll
unravel <- function(roll_freq) {
  c <- c()
  for (i in 1:6) {
    c <- c(c,rep(i, roll_freq[i]))
  }
  c
}

# Create a data frame of all possible dice rolls given n dice
randomRoll <- function(n) {
  expand.grid(replicate(n, seq(6), simplify = FALSE))
}

# Game Calculator
#################

#Create histogram of all possible roll combinations
all_scores <- apply(randomRoll(6), 1, scoreGame) # Find all possible combinations of dice rolls
mean(all_scores) # Score the average



shinyServer(function(input, output) {
    
    my_roll <- reactive({
      c(input$die1, input$die2, input$die3, input$die4, input$die5, input$die6)
    })
    
    my_scores2 <- reactive({

            b <- randomRoll(6-sum(onlyScoringDice(my_roll()))) # Create data frame of possible rolls of non-scoring dice
            dat <- cbind(b, t(unravel(onlyScoringDice(my_roll())))) # Add new roll to scoring dice
              my_scores <- c()
              for (i in 1:nrow(dat)) {
                my_scores[i] <- scoreGame(dat[i,])
              }
       #       my_scores <- apply(d, 1, FUN = scoreGame()) # Score the new rolls
              my_scores2 <- ifelse(my_scores == scoreGame(my_roll()), 0, my_scores) # Set to zero all rolls that don't increase score
    })
    
    new_expected <- reactive({
      b <- randomRoll(6-sum(onlyScoringDice(my_roll())))
      if (nrow(b) != 0) {
        new_expected <- round(mean(my_scores2()))
      } else {
        new_expected <- "[All dice were scored]"
        }
    })
    
    zero_scores <- reactive({
      length(my_scores2()[my_scores2() == 0]) / length(my_scores2())
    })
      
#     new_expected <- reactive({
#       
#       b <- randomRoll(6-sum(onlyScoringDice(my_roll()))) # Create data frame of possible rolls of non-scoring dice
#       
#       if (nrow(b) != 0) {
#         dat <- cbind(b, t(unravel(onlyScoringDice(my_roll())))) # Add new roll to scoring dice
#         my_scores <- c()
#         for (i in 1:nrow(dat)) {
#           my_scores[i] <- scoreGame(dat[i,])
#         }
#         #       my_scores <- apply(d, 1, FUN = scoreGame()) # Score the new rolls
#         my_scores2 <- ifelse(my_scores == scoreGame(my_roll()), 0, my_scores) # Set to zero all rolls that don't increase score
#         new_expected <- round(mean(my_scores2))
#       } else {
#         new_expected <- "[All dice were scored]"
#       }
#     })
    
    output$your_roll <- renderText({
      paste("You rolled a ", input$die1, input$die2, input$die3, input$die4, input$die5, input$die6)
    })
    output$score1 <- renderText({
        paste("The score of your first roll is ", scoreGame(c(input$die1, input$die2, input$die3, input$die4, input$die5, input$die6)))
    })
    output$number_of_dice <- renderText({
      paste(sum(onlyScoringDice(my_roll())), "dice contributed to your score")
    })
    output$new_expected <- renderText({
      paste("After re-rolling all non-scoring dice, your expected score is", new_expected())
    })
    output$probability_of_zero <- renderText({
      paste("You have a", round(zero_scores() * 100, 1), "percent probability of losing all your points")
    })
    output$plot1 <- renderPlot({
        d <- density(all_scores)
        plot(d, main = "Density Plot of Possible Scores", yaxt='n')
        polygon(d, col="grey", border="black")
        abline(v = scoreGame(c(input$die1, input$die2, input$die3, input$die4, input$die5, input$die6)))
#         ggplot() + aes(all_scores) + # Plot a histogram of all possible combinations
#           geom_histogram(fill='grey', binwidth = 50) + 
#           geom_vline(xintercept = scoreGame(c(input$die1, input$die2, input$die3, input$die4, input$die5, input$die6)), color = 'red', size = 1.1) + 
#           labs(title = "Score Histogram") +
#           labs(x = "Score", y = "Count")
        })
  }
)