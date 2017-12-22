#installed.packages("shiny")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("hunspell")

library(shiny)
library(base64enc)
library(twitteR)
library(hunspell)

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
      movie <- input$query
      bad_words <- hunspell_find(movie)
      sugg <- hunspell_suggest(bad_words[[1]])
      print(bad_words[[1]])
      sugg <- unlist(sugg[[1]])
      print(sugg)
      movie <- gsub(bad_words[[1]], sugg[[1]], movie)
      print(movie)
      
      api_key <- "6vLKhDh7UJdO1vMwYOnDbrr91"
      api_secret <- "6UY2wIWDTSrOgpQVziA9sUFlPBIDhodcUmmz2KheFLkXc0xJF8"
      access_token <- "1328460499-EmARRTuftN6dqehvZ7CgeNKqIpB0JIWoT3EvjKi"
      access_token_secret <- "1Sz7gKFQS8Gz0Y1zln7XO9gXIWccsw9hgrBeuoFhUdyrS"
      setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
      
      tweets <- searchTwitter(input$query, n=100, lang="en")
      tweets.df <- twListToDF(tweets)
      
      output$tweets <- renderDataTable(tweets.df["text"])
    })
  }
)