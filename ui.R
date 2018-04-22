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
                     c("No" = FALSE,
                       "Yes" = TRUE)),
        actionButton("enter", "Enter")
      ),
      mainPanel(
        h1("Tweet deck panel"),
        plotOutput("bar"),
        plotOutput("pie"),
        plotOutput("cloud"),
        tableOutput("processed_tweets")
      )
    )
  )
)