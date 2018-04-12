#installed.packages("shiny")
library(shiny)
library(shinyjs)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        h2("Input panel"),
        textInput("query", "Enter movie title: ", ""),
        radioButtons("retweets", "Remove retweets?",
                    c("Yes" = TRUE,
                      "No" = FALSE)),
        radioButtons("preprocess", "Preprocess training data?",
                     c("Yes" = TRUE,
                       "No" = FALSE)),
        actionButton("enter", "Enter")
      ),
      mainPanel(
        h1("Tweet deck panel"),
        tableOutput("processed_tweets"),
        plotOutput("pie")
      )
    )
  )
)