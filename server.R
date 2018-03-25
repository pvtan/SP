#installed.packages("shiny")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("hunspell")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("devtools")
#devtools::install_github("bmschmidt/wordVectors")
#install.packages("text2vec")
#install.packages("caret")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("psych")
#install.packages("lime")

library(shiny)
library(base64enc)
library(twitteR)
library(hunspell)
library(tm)
#library(SnowballC)
#library(wordVectors)
#library(magrittr)
#library(text2vec)
library(caret)
library(e1071)
library(lime)
library(RTextTools)
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
  doc <- gsub("therell", "there will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub(" im", " I am", doc)
  doc <- gsub("'s", "", doc) # 's could be 'is' or could be possessive: it has no expansion
  return(doc)
}

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
      #spell checker
      movie <- spellCheck()
      #retrieve tweets 
      tweetsRetrieved <- retrieveTweets(movie)
      #option to remove retweets
      if(input$retweets) tweetsRetrieved <- removeRetweets(tweetsRetrieved)
      
      #convert to data frame
      tweets.df <- twListToDF(tweetsRetrieved)
      write.csv(tweets.df, file="unprocessedTweets.csv")
      write.csv(tweets.df$id, file="idTweets.txt")
      
      tweets.df <- removeMovieFoundInScreenname(movie, tweets.df)
      output$tweets <- renderDataTable(tweets.df["text"])
      
      if(length(tweets.df) == 0) {
        print("There are no tweets to undergo pre-processing.")
      } else {
        tweets.df <- preprocessTweets(tweets.df)
        output$processed_tweets <- renderTable(tweets.df)
        
        #write.csv(corpus$content, file="tweets.csv")
        #from https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd
        #convert tweets to a vector
        #if (file.exists("tweets.bin")) file.remove("tweets.bin")
        #model = train_word2vec("tweets.txt","tweets.bin",
        #                        vectors=100,threads=4,window=12,iter=5,negative_samples=0)
        #print(model %>% closest_to("good"))
        
        df <- readDataSet("tweets.csv")
        
        useSVMWithMatrix(df)
        useSVMWithoutMatrix(df)
      }
    })
    
    spellCheck <- function() {
      movie <- input$query
      bad_words <- hunspell_find(movie)
      sugg <- hunspell_suggest(bad_words[[1]])
      print(bad_words[[1]])
      sugg <- unlist(sugg)
      print(sugg)
      if (!is.null(sugg)) { 
        movie <- gsub(bad_words[[1]], sugg[[1]], movie) 
        print("Did you mean ", movie, "?")
      }
      
      return(movie)
    }
    
    retrieveTweets <- function(movieQuery) {
      #from https://shiny.rstudio.com/articles/progress.html
      withProgress(message = 'Retrieving tweets', value = 0, {
        setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
        tweets <- searchTwitter(input$query, n=500, lang="en", resultType="recent")
        n <- 10 # Number of times we'll go through the loop
        for (i in 1:n) {
          incProgress(1/n) # Increment the progress bar, and update the detail text.
          Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
        }
      })
      
      return(tweets)
    }
    
    removeRetweets <- function(tweets) {
      tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
    }
    
    removeMovieFoundInScreenname <- function(movieName, tweets) {
      tweets$screenName <- tolower(tweets$screenName)
      movie <- tolower(movieName)
      filterOut <- tweets[grep(movie, tweets$screenName),]
      if(nrow(filterOut) != 0) {
        tweets <- tweets[- grep(movie, tweets$screenName),] #remove movie name found in screen name
      }
      
      return(tweets)
    }
    
    preprocessTweets <- function(tweets.df) {
      #to remove emojis
      tweets.df["text"] <- sapply(tweets.df["text"],
                                  function(row) iconv(row, "latin1", "ASCII", sub="")) 
      tweet_vector <- unlist(tweets.df["text"], use.names=FALSE)
      corpus <- Corpus(VectorSource(tweet_vector))
      for (i in 1:length(corpus$content)) {
        #from https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
        corpus[[i]]$content = gsub("&amp", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("@\\w+", "", corpus[[i]]$content) #remove handles
        corpus[[i]]$content = gsub("#\\w+", "", corpus[[i]]$content) #remove hashtags
        corpus[[i]]$content = gsub("http.+", "", corpus[[i]]$content) #remove links
        corpus[[i]]$content = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("RT", "", corpus[[i]]$content) #remove RT labels
      }
      corpus <- tm_map(corpus, content_transformer(tolower)) #case normalize
      corpus <- tm_map(corpus, fix.contractions)
      corpus <- tm_map(corpus, content_transformer(removePunctuation)) #remove punctuations
      corpus <- tm_map(corpus, content_transformer(removeWords), 
                       stopwords("english")) #remove stop words
      dict <- ""
      for (i in 1:length(corpus$content)) dict <- paste(dict, corpus[[i]]$content, sep=" ")
      dict <- strsplit(dict, "\\s+")
      dict <- unlist(dict)
      words <- c()
      for (i in 1:length(corpus$content)) {
        tw <- strsplit(corpus[[i]]$content, "\\s+")
        tw <- unlist(tw)
        tw <- stemCompletion(tw, dict)
        words <- c(words, tw)
        tw <- paste(tw, collapse=' ')
        corpus[[i]]$content <- tw
      }
      
      return(corpus$content)
    }
    
    readDataSet <- function(filename) {
      dataSet <- read.csv(filename)
      
      dataSet$id <- as.character(dataSet$id)
      dataSet$review <- as.character(dataSet$review)
      dataSet$sentiment <- as.factor(dataSet$sentiment)
      
      return(dataSet)
    }
    
    useSVMWithMatrix <- function(df) {
      train_data <- df[(1:33), ]
      test_data <- df[-(1:33), ]
      
      dtMatrix <- create_matrix(df$review, weighting=weightTfIdf)
      container <- create_container(dtMatrix, df$sentiment, trainSize=1:33, testSize= 34:66, virgin=FALSE)
      svm_model <- train_model(container, "SVM", kernel="linear", cost=1)
      results <- classify_model(container, svm_model)
      print(results)
    }
    
    useSVMWithoutMatrix <- function(df) {
      train_data <- df[(1:33), ]
      test_data <- df[-(1:33), ]
      
      #svm_model <- svm(
      #              train_data$sentiment ~ train_data$review, 
      #              data = train_data,
      #              kernel = "linear",
      #              #cross=10,
      #              cost=10, 
      #              scale=FALSE)
      
      #pred_train <- predict(svm_model, test_data)
      #print(mean(pred_train==test_data$sentiment))
      #print(pred_train)
      
      #model <- train(review ~ sentiment, data = train_data, method = "svmLinear2")
      #from https://github.com/thomasp85/lime
      #explainer <- lime(train_data, model)
      #explanation <- explain(test_data, explainer, n_labels = 3, n_features = 1)
      #head(explanation)
      #plot_features(explanation)
      
      #tab <- table(pred=pred_train, true=test_data$sentiment)
      #results <- confusionMatrix(tab)
      #results <- as.matrix(results)
      #print(results)
      #x <- c(
      #  sum(results["negative", "negative"], results["negative", "neutral"], results["negative", "positive"]),
      #  sum(results["neutral", "negative"], results["neutral", "neutral"], results["neutral", "positive"]),
      #  sum(results["positive", "negative"], results["positive", "neutral"], results["positive", "positive"])
      #)
      #labels <- c("Negative", "Neutral", "Positive")
      # Plot the chart.
      #output$pie <- renderPlot(pie(x,labels))
    }
  }
)