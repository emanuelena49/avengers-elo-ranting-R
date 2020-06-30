# libs
library(shiny)
library(shinythemes)
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
  r = matrix(NA, n, m+1)
  
  # iterate through games
  for (i in 2:m+1) {
    score = games[i, "Score"]
    white = games[i, "White"]
    black = games[i, "Black"]
    
    # get vals
    white_r = ifelse(is.na(r[white, i-1]), 0, r[white, i-1])
    black_r = ifelse(is.na(r[black, i-1]), 0, r[black, i-1])
    
    # compute update
    spread = white_r - black_r
    mu = 1 / (1 + 10^(-spread / z))
    update = k * (score - mu)
    
    # "carry on" old values
    for(j in 1:n) {
      
      r[j, i] = r[j, i-1]
    }
    
    # update ratings
    r[white, i] = white_r + update
    r[black, i] = black_r - update
  }
  return(r)
}

scores <- elo_progressive(as.data.frame(fights_elo_format_s))
n_charaters <- dim(scores)[1]
n_steps <- dim(scores)[2]

# Define UI
ui <- fluidPage(
  
  # App theme ----
  theme = shinytheme("cyborg"), 
  
  # App title ----
  titlePanel("Avengers ELO Rating"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
    
      # doc:  https://shiny.rstudio.com/articles/action-buttons.html
      # actionButton("playButton", "Play")
      
      # Input: Slider for the time ----
      sliderInput(inputId = "time",
                  label = "Scontro",
                  min = 1,
                  max = n_steps,
                  value = n_steps,
                  step = 1 # (vedi dataset)
      ), 
      
      sliderInput(inputId = "charaters",
                  label = "Numero di personaggi",
                  min = 1,
                  max = n_charaters,
                  value = 6,
                  step = 1 # (vedi dataset)
      )
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Text label  ----
      textOutput(outputId = "myLabel"),
      
      # Output: Barplot ----
      plotOutput(outputId = "myPlot")
    )
    
  ),
  
  
)


# Define server logic----
server <- function(input, output) {
  
  # define reaction to button click event
  # (START THE ANIMATION)
  # observeEvent(input$playButton, {
    # TODO ... 
  # })
  
  output$myLabel <- renderText({ 
    fight <- fights_elo_format_s %>% filter(row_number()==input$time-1) %>% select(movie,comment)
    movie <- fight %>% {.[1,1]}
    comment <- fight %>% {.[1,2]}
    
    paste(
      "Film:\t", movie, 
      ",\tScena:\t", comment, 
      sep=""
      )
  })
  
  
  output$myPlot <- renderPlot({
    
    # extract the scores at this specific time
    s <- scores[, input$time]
    # make it as tibble
    st <- tibble::enframe(s) %>% rename(id=name, score=value) %>% filter(!is.na(score)) %>% arrange(-score)
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
      
      ylim(min(st$score), max(st$score)) +
      
      ylab("ELO score") + xlab("Personaggio")
    
  })
  
  
  
}

shinyApp(ui, server)
  