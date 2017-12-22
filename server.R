#installed.packages("shiny")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("hunspell")

library(shiny)
library(base64enc)
library(twitteR)
source("config.R")

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
      movie <- input$query
      bad_words <- hunspell_find(movie)
      sugg <- hunspell_suggest(bad_words[[1]])
      print(bad_words[[1]])
      sugg <- unlist(sugg)
      print(sugg)
      if (!is.null(sugg)) { 
        movie <- gsub(bad_words[[1]], sugg[[1]], movie) 
      }
      print(movie)
      
      setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
      
      tweets <- searchTwitter(input$query, n=100, lang="en")
      tweets.df <- twListToDF(tweets)
      
      output$tweets <- renderDataTable(tweets.df["text"])
    })
  }
)