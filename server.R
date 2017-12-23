#installed.packages("shiny")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("hunspell")
#install.packages("tm")
#install.packages("SnowballC")

library(shiny)
library(base64enc)
library(twitteR)
library(hunspell)
library(tm)
library(SnowballC)
source("config.R")

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
      #spell checker
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
      
      #retrieve tweets 
      #from https://shiny.rstudio.com/articles/progress.html
      withProgress(message = 'Retrieving tweets', value = 0, {
        setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
        tweets <- searchTwitter(input$query, n=100, lang="en")
        n <- 10 # Number of times we'll go through the loop
        for (i in 1:n) {
          incProgress(1/n) # Increment the progress bar, and update the detail text.
          Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
        }
      })
      
      tweets.df <- twListToDF(tweets)
      #to remove emojis
      tweets.df["text"] <- sapply(tweets.df["text"],
                          function(row) iconv(row, "latin1", "ASCII", sub=""))
      output$tweets <- renderDataTable(tweets.df["text"])
      tweet_vector <- unlist(tweets.df["text"], use.names=FALSE)
      corpus <- (VectorSource(tweet_vector))
      corpus <- Corpus(corpus)
      corpus <- tm_map(corpus, content_transformer(removePunctuation)) #remove punctuations
      output$processed_tweets <- renderTable(corpus$content)
    })
  }
)