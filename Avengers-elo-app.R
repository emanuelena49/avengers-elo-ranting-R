# libs
library(shiny)
library(dplyr)
library(ggplot2)

# import ELO Ready Dataset
fights_elo_format_s <- read.csv("fights_elo_format.csv")
# import charater Dataset
charaters <- read.csv("charaters.csv") %>% select(id, charater)

# Progressive Elo ----

##  Elo rating system (with all steps)
# INPUT
# games: a game *matrix* with columns White, Black and Score
#        Players are integer numbers starting at 1
#        The matrix is sorted in chronological order
# zeta: logistic parameter
# k: update factor
# OUTPUT
# r: rating vector
elo_progressive = function(games, z = 400, k = 25) {
  
  # number of players 
  # (players are integer numbers starting at 1)
  n = max(c(games[, "White"], games[, "Black"]))
  
  # number of games
  m = nrow(games)
  
  # rating vector
  r = matrix(0, n, m+1)
  
  # iterate through games
  for (i in 2:m+1) {
    score = games[i, "Score"]
    white = games[i, "White"]
    black = games[i, "Black"]
    
    # compute update
    spread = r[white, i-1] - r[black, i-1]
    mu = 1 / (1 + 10^(-spread / z))
    update = k * (score - mu)
    
    # "carry on" old values
    for(j in 1:n) {
      
      r[j, i] = r[j, i-1]
    }
    
    # update ratings
    r[white, i] = r[white, i-1] + update
    r[black, i] = r[black, i-1] - update
  }
  return(r)
}

scores <- elo_progressive(as.data.frame(fights_elo_format_s))
n_charaters <- dim(scores)[1]
n_steps <- dim(scores)[2]

# Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Avengers ELO Rating"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # doc:  https://shiny.rstudio.com/articles/action-buttons.html
    # actionButton("playButton", "Play")
    
    # Input: Slider for the time ----
    sliderInput(inputId = "time",
                label = "Time",
                min = 1,
                max = n_steps,
                value = n_steps,
                step = 1 # (vedi dataset)
    ), 
    
    sliderInput(inputId = "charaters",
                label = "Number of Charater",
                min = 1,
                max = n_charaters,
                value = 6,
                step = 1 # (vedi dataset)
    )
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: ... ----
    plotOutput(outputId = "myPlot")
    
  )
)


# Define server logic----
server <- function(input, output) {
  
  # define reaction to button click event
  # (START THE ANIMATION)
  # observeEvent(input$playButton, {
    # TODO ... 
  # })
  
  
  output$myPlot <- renderPlot({
    
    # extract the scores at this specific time
    s <- scores[, input$time]
    # make it as tibble
    st <- tibble::enframe(s) %>% rename(id=name, score=value) %>% arrange(-score)
    # join with charater list
    st <- st %>% left_join(charaters, by=c('id'))
    
    # charaters to print
    ctp <- input$charaters
    
    ggplot(data=st %>% head(n=ctp), mapping = aes(
        x=reorder(charater, -score), 
        y=score, 
        fill=charater, 
        )) +
      geom_bar(stat="identity") + 
      ylim(min(st$score), max(st$score))
    
  })
}

shinyApp(ui, server)
  