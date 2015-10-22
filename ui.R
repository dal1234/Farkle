shinyUI(pageWithSidebar(
  headerPanel('Farkel Score Calculator'),
  sidebarPanel(
    numericInput('die1', 'First Die', 1,
                 min = 1, max = 6),
    numericInput('die2', 'Second Die', 2,
                 min = 1, max = 6),
    numericInput('die3', 'Third Die', 3,
                 min = 1, max = 6),
    numericInput('die4', 'Fourth Die', 4,
                 min = 1, max = 6),
    numericInput('die5', 'Fifth Die', 6,
                 min = 1, max = 6),
    numericInput('die6', 'Sixth Die', 6,
                 min = 1, max = 6),
    submitButton("Submit"),
    width = 2),
  mainPanel(
    fluidRow(
      column(8, 
           h4("Rules"),
           h5('Farkel is a dice game of chance.  You roll six dice and receive a score based on the chart to the right.'),
           h5('After you roll, you have the option to keep the dice that contribute to your score and re-roll the remaining dice.'),
           h5("If your re-roll adds to your score, you keep the higher score.  If you don't roll additional points, your score goes to zero."),
           h4("Directions"),
           h5("Set the dice roll using the text boxes to the left.  Click on 'Submit' and see your score below.")
      ),
      column(4,
           img(src = 'scoring.png', width = 350, height = 200)
      )
    ),
    fluidRow(
      column(12,
          h4("Scoring"),
          textOutput("your_roll"),
          textOutput("score1"),
          textOutput("number_of_dice"),
          textOutput("new_expected"),
          textOutput("probability_of_zero"),
          plotOutput('plot1')
      )
    )
  )
))

