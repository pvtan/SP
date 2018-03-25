#installed.packages("shiny")
library(shiny)

shinyUI(
  fluidPage(
    titlePanel(title = "MoviSenti"),
    sidebarLayout(
      sidebarPanel(
        h2("Side bar panel"),
        textInput("query", "Enter movie title: ", ""),
        radioButtons("retweets", "Remove retweets?",
                    c("Yes" = TRUE,
                      "No" = FALSE)),
        actionButton("enter", "Enter")
      ),
      mainPanel(
        h1("Main panel"),
        dataTableOutput("tweets"),
        tableOutput("processed_tweets"),
        plotOutput("pie")
      )
    )
  )
)