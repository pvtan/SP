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

#function to remove contractions in an English-language source
#function from https://github.com/mkfs/misc-text-mining/blob/master/R/wordcloud.R
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc) # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("wont", "will not", doc)
  doc <- gsub("dont't", "do not", doc)
  doc <- gsub("dont", "do not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'s", "", doc) # 's could be 'is' or could be possessive: it has no expansion
  return(doc)
}

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
        tweets <- searchTwitter(input$query, n=100, lang="en", resultType="recent")
        n <- 10 # Number of times we'll go through the loop
        for (i in 1:n) {
          incProgress(1/n) # Increment the progress bar, and update the detail text.
          Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
        }
      })
      
      tweets.df <- twListToDF(tweets)
      tweets.df$screenName <- tolower(tweets.df$screenName)
      movie <- input$query
      movie <- tolower(movie)
      tweets.df <- tweets.df[- grep(movie, tweets.df$screenName),]
      print(tweets.df)
      output$tweets <- renderDataTable(tweets.df["text"])
      tweets.df["text"] <- sapply(tweets.df["text"],
                          function(row) iconv(row, "latin1", "ASCII", sub="")) #to remove emojis
      tweet_vector <- unlist(tweets.df["text"], use.names=FALSE)
      corpus <- (VectorSource(tweet_vector))
      corpus <- Corpus(corpus)
      for (i in 1:length(corpus$content)) {
        #from https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
        corpus[[i]]$content = gsub("&amp", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("@\\w+", "", corpus[[i]]$content) #remove handles
        corpus[[i]]$content = gsub("http.+", "", corpus[[i]]$content) #remove links
        corpus[[i]]$content = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("RT", "", corpus[[i]]$content) #remove RT labels
      }
      corpus <- tm_map(corpus, content_transformer(tolower)) #case normalize
      corpus <- tm_map(corpus, fix.contractions)
      corpus <- tm_map(corpus, content_transformer(removePunctuation)) #remove punctuations
      corpus <- tm_map(corpus, content_transformer(removeWords), 
                       stopwords("english")) #remove stop words
      corpus_copy <- corpus
      corpus <- tm_map(corpus, stemDocument)
      #corpus <- tm_map(corpus, stemCompletion, dictionary=corpus_copy)
      for (i in 1:length(corpus$content)) {
        correction <- PlainTextDocument(stripWhitespace
                                        (paste
                                          (stemCompletion
                                            (unlist
                                              (strsplit
                                                (as.character(corpus[[i]]$content)," ")),
                                                dictionary=corpus_copy, type="shortest"),
                                                sep="", collapse=" ")))
        #if(correction[[1]] != "NA") corpus[[i]]$content <- correction[[1]]
      }
      output$processed_tweets <- renderTable(corpus$content)
    })
  }
)