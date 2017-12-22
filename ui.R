#installed.packages("shiny")
library(shiny)

shinyUI(
  fluidPage(
    titlePanel(title = "MoviSenti"),
    sidebarLayout(
      sidebarPanel(
        h2("Side bar panel")
      ),
      mainPanel(
        h1("Main panel"),
        textInput("query", "Enter movie title: ", "")
      )
    )
  )
)