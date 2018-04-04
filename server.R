#installed.packages("shiny")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("hunspell")
#install.packages("tm")
#install.packages("devtools")
#devtools::install_github("bmschmidt/wordVectors")
#install.packages("text2vec")
#install.packages("caret")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("psych")
#install.packages("lime")
#install.packages("lexicon")

library(shiny)
library(base64enc)
#library(twitteR)
library(rtweet)
library(hunspell)
library(tm)
#library(wordVectors)
#library(magrittr)
#library(text2vec)
library(caret)
library(e1071)
library(lime)
library(RTextTools)
library(lexicon)
library(stringr)
library(corpus)
library(DT)
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
      data(hash_sentiment_sentiword)
      print(hash_sentiment_sentiword[1:5,])
      #spell checker
      
      tw <- strsplit(input$query, "\\s+")
      tw <- unlist(tw)
      for(i in 1:length(tw)) {
        tw[i] <- spellCheck(tw[i])
      }
      movie <- paste(tw, collapse=" ") 
      movieHashtag <- paste0("#", paste(tw, collapse="")) 
      movieQuery <- paste0(movie, " OR ", movieHashtag)
      
      print(paste0("Searching for ", movieQuery))
       
      tweetsRetrieved <- retrieveTweets(movie)
      tweets.df <- data.frame(lapply(tweetsRetrieved, as.character), stringsAsFactors=FALSE)
      
      #remove movie in screenName
      tweets.df <- removeMovieFoundInScreenname(movie, tweets.df)
      
      #write to file
      write.csv(tweets.df, file="unprocessedTweets.csv")
      
      #pre-processing of tweets
      if(length(tweets.df) == 0) {
        print("There are no tweets to undergo pre-processing.")
      } else {
        processedTweets <- preprocessTweets(tweets.df)
        compare <- cbind(tweets.df["text"], processedTweets)
        output$processed_tweets <- renderTable(compare)
        write.csv(compare, file="results.csv")
        
        #df <- readDataSet("tweets.csv")
        
        #useSVMWithMatrix(df)
        #useSVMWithoutMatrix(df)
      }
    })
    
    #from https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html
    spellCheck <- function(term) {
      # if the term is spelled correctly, leave it as-is
      if (hunspell_check(term, dict = dictionary("en_US"))) {
        return(term)
      }
      
      suggestions <- hunspell::hunspell_suggest(term)[[1]]
      
      # if hunspell found a suggestion, use the first one
      if (length(suggestions) > 0) {
        return(suggestions[[length(suggestions)]])
      } else {
        # otherwise, use the original term
        return(term)
      }
    }
    
    retrieveTweets <- function(movieQuery) {
      #from https://shiny.rstudio.com/articles/progress.html
      withProgress(message = 'Retrieving tweets', value = 0, {
        twitter_token <- create_token(app = "MyTwitterAppSP", consumer_key = api_key, consumer_secret = api_secret)
        if(input$retweets) {
          tweets <- search_tweets(movieQuery, n = 300, include_rts = FALSE, lang = "en")
        } else {
          tweets <- search_tweets(movieQuery, n = 300, include_rts = TRUE, lang = "en")
        }
        n <- 10 # Number of times we'll go through the loop
        for (i in 1:n) {
          incProgress(1/n) # Increment the progress bar, and update the detail text.
          Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
        }
      })
      
      return(tweets)
    }
    
    removeMovieFoundInScreenname <- function(movieName, tweetsRetrieved) {
      tweets <- tweetsRetrieved
      tweets$screen_name <- tolower(tweets$screen_name)
      movie <- tolower(movieName)
      filterOut <- tweets[grep(movie, tweets$screen_name),]
      if(nrow(filterOut) != 0) {
        tweets <- tweets[-grep(movie, tweets$screen_name),] #remove movie name found in screen name
      }
      
      return(tweets)
    }
    
    preprocessTweets <- function(tweets.df) {
      #to remove emojis
      tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, "latin1", "ASCII", sub="")) 
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
      #corpus <- tm_map(corpus, fix.contractions)
      corpus <- tm_map(corpus, content_transformer(removePunctuation)) #remove punctuations
      corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english")) #remove stop words
      
      #expand acronyms
      print(length(corpus$content))
      for (i in 1:length(corpus$content)) { #for every tweet
        tw <- strsplit(corpus[[i]]$content, "\\s+")
        tw <- unlist(tw)
        loop <- length(tw)
        expTweet <- ""
        for (j in 1:loop) { #iterate per word
          tw[j] <- tolower(tw[j])
          replacement <- slang[slang$x==tw[j], "expansion"]
          if(!is.na(replacement[1])) {
            tw[j] <- replacement[1]
            expTweet <- paste0(expTweet, " ", replacement[1])
          } else {
            expTweet <- paste0(expTweet, " ", as.String(tw[j]))
          }
        }
        corpus[[i]]$content <- expTweet
      }
      
      #slang <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/noslang.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
      
      #check for spelling
      for (i in 1:length(corpus$content)) { #for every tweet
        tweet <- gsub("\\s+", " ", str_trim(corpus[[i]]$content))
        tw <- strsplit(tweet, "\\s+")
        tw <- unlist(tw)
        #print(tw)
        if(length(tw) > 0) {
          for (j in 1:length(tw)) { #iterate per word
            #tw[j] <- wordStem(c(tw[j]), language = "english")
            #tw[j] <- spellCheck(tw[j])
            tw[j] <- gsub("([[:alpha:]])\\1{2,}", "\\2", as.String(tw[j]))
            token <- text_tokens(c(tw[j]), stemmer = "en")
            tw[j] <- (unlist(token))[1]
            tw[j] <- spellCheck(as.String(tw[j]))
          } 
        }
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