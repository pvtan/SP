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
#install.packages("coreNLP",INSTALL_opts="--no-multiarch")

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
#library(corpus)
library(DataCombine)
library(NLP)
library(openNLP)
library(dplyr)
#library(coreNLP)
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

contractions <- data.frame(word = c("won't", "wont", "dont't", "dont", "'ll", "therell", "'re", "'ve", "I'm", " im ", "'s", "can't", "cant", "didnt", "n't"),
                           expansion = c("will not", "will not", "do not", "do not"," will", "there will", " are", " have", "I am", " I am ", "", "can not", "can not", "did not", " not"))

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
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
        preprocessTweets(tweets.df, "results.csv")
        if(input$preprocess) {
          tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/syuzhetTrainingData.csv", 
                                   header=TRUE, 
                                   sep=",", 
                                   stringsAsFactors = FALSE)
          tweets.df <- preprocessTweets(tweetDataset, "preprocessedDataSet.csv")
        } else {
          tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/trainingData.csv", 
                                   header=TRUE, 
                                   sep=",", 
                                   stringsAsFactors = FALSE)
          training <- floor(nrow(tweetDataset) * 0.8)
          useSVMWithoutMatrix(tweetDataset, training)
          #useSVMWithMatrix(tweetDataset, training)
          print(training)
        }
        
        #useSVMWithMatrix(df)
        #useSVMWithoutMatrix(df)
      }
    })
    
    preprocessTweets <- function(tweets.df, filename) {
      processedTweets <- tweets.df
      processedTweets <- cleanTweets(processedTweets)
      processedTweets <- fixContractions(processedTweets)
      processedTweets <- decodeEmojis(processedTweets) 
      processedTweets <- removePunctuations(processedTweets)
      processedTweets <- expandAcronyms(processedTweets) 
      processedTweets <- fixContractions(processedTweets)
      processedTweets <- shortenLongWords(processedTweets)
      processedTweets <- applyPOSTag(processedTweets)
      #processedTweets <- spellCorrection(processedTweets)
      compare <- cbind("orig" = tweets.df["text"], "text" = processedTweets["text"])
      compare <- compare[!is.na(compare$text), ]
      cld2Col <- sapply(compare$text, function(row) cld2::detect_language(text = row, plain_text = FALSE))
      compare <- cbind(compare, "cld2Col" = cld2Col)
      compare <- compare[(compare$cld2Col=="en"), ]
      colnames(compare) <- c("original", "text", "cld2")
      compare <- compare[!is.na(compare$text), ]
      compare <- compare[, !(colnames(compare) %in% c("cld2"))]
      print(nrow(compare))
      output$processed_tweets <- renderTable(compare)
      write.csv(compare, file = filename)
      return(compare)
    }
    
    #from https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html
    spellCheck <- function(term) {
      if (hunspell_check(term, dict = dictionary("en_US"))) { # if the term is spelled correctly, leave it as-is
        return(term)
      }
      
      suggestions <- hunspell::hunspell_suggest(term)[[1]]
      
      if (length(suggestions) > 0) {
        return(suggestions[[length(suggestions)]]) # if hunspell found a suggestion, use the last one
      } else {
        return(term) # otherwise, use the original term
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
    
    fixContractions <- function(tweets.df) {
      removedContractions <- FindReplace(data = tweets.df, Var = "text", 
                  replaceData = contractions,
                  from = "word", to = "expansion", 
                  exact = FALSE) #DataCombine
      return(removedContractions)
    }
    
    decodeEmojis <- function(tweets.df) {
      #from https://raw.githubusercontent.com/today-is-a-good-day/Emoticons/master/emDict.csv
      emoticons <- read.csv("dict/newEmDict.csv", header = T)
      tweets.df$text <- sapply(tweets.df$text, function(row) iconv(row, from = "", to = "ASCII", sub = "byte"))
      tweets.df <- FindReplace(data = tweets.df, Var = "text", 
                               replaceData = emoticons,
                               from = "R_Encoding", to = "Description", 
                               exact = FALSE) #DataCombine
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub(">", "", row))
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("<", " ", row))
      tweets.df <- FindReplace(data = tweets.df, Var = "text", 
                               replaceData = emoticons,
                               from = "Bytes", to = "Description", 
                               exact = FALSE) #DataCombine
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("( )*e2 [8-9][0-9] [a-zA-Z0-9][a-zA-Z0-9]( )*", "", row))
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("f0(\\s[a-zA-Z0-9][a-zA-Z0-9]){2, }( )*", "", row))
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("e0(\\s[a-zA-Z0-9][a-zA-Z0-9]){2, }( )*", "", row))
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("e[3-6d-f](\\s[a-zA-Z0-9][a-zA-Z0-9]){2, }( )+", "", row))
      #tweets.df$text <- sapply(tweets.df$text, function(row) gsub("e6(\\s[a-zA-Z0-9][a-zA-Z0-9]){1, }", "", row))
      #tweets.df$text <- sapply(tweets.df$text, function(row) gsub("ed(\\s[a-zA-Z0-9][a-zA-Z0-9]){1, }", "", row))
      #tweets.df$text <- sapply(tweets.df$text, function(row) gsub("ef(\\s[a-zA-Z0-9][a-zA-Z0-9]){1, }", "", row))
      return(tweets.df)
    }
    
    expandAcronyms <- function(tweets.df) {
      #expand acronyms
      slang <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/noslang.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
      print(nrow(tweets.df))
      tweetData <- tweets.df
      tweets <- unlist(tweetData$text)
      for (i in 1:length(tweets)) { #for every tweet
        tw <- strsplit(tweets[i], "\\s+")
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
        tweets[i] <- expTweet
      }
      tweetData$text <- tweets
      return(tweetData)
    }
    
    shortenLongWords <- function(tweets.df) {
      tweets <- tweets.df
      tweets$text <- sapply(tweets$text, function(row) gsub("([[:alpha:]])\\1+", "\\1\\1", row))
      return(tweets)
    }
    
    removePunctuations <- function(tweets.df) {
      tweets <- tweets.df
      tweets$text <- removePunctuation(tweets$text)
      return(tweets)
    }
    
    spellCorrection <- function(tweets.df) {
      #check for spelling
      tweetDeck <- tweets.df
      tweets <- unlist(tweetDeck$text)
      for (i in 1:length(tweets)) { #for every tweet
        tweet <- gsub("\\s+", " ", str_trim(tweets[i]))
        tw <- strsplit(tweet, "\\s+")
        tw <- unlist(tw)
        if(length(tw) > 0) {
          for (j in 1:length(tw)) { #iterate per word
            #tw[j] <- wordStem(c(tw[j]), language = "english")
            #tw[j] <- spellCheck(tw[j])
            token <- text_tokens(c(tw[j]), stemmer = "en")
            tw[j] <- (unlist(token))[1]
            tw[j] <- spellCheck(as.String(tw[j]))
          } 
        }
        
        tweets[i] <- paste(tw, collapse = " ")
      }
      tweetDeck$text <- tweets
      return(tweetDeck)
    }
    
    cleanTweets <- function(tweets.df) {
      #to remove unreadable/extra characters
      #tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, "ASCII", "latin1", sub="")) 
      tweets <- tweets.df
      tweet_vector <- unlist(tweets$text, use.names=FALSE)
      corpus <- Corpus(VectorSource(tweet_vector))
      
      for (i in 1:length(corpus$content)) {
        #from https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
        corpus[[i]]$content = gsub("&amp", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("@\\w+", "", corpus[[i]]$content) #remove handles
        corpus[[i]]$content = gsub("#\\w+", "", corpus[[i]]$content) #remove hashtags
        corpus[[i]]$content = gsub("http.+", "", corpus[[i]]$content) #remove links
        #corpus[[i]]$content = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub(":", "", corpus[[i]]$content) #remove RT labels
      }
      
      tweets["text"] <- corpus$content
      return(tweets)
    }
    
    applyPOSTag <- function(tweets.df) {
      tweets <- tweets.df
      sent_token_annotator = Maxent_Sent_Token_Annotator()
      word_token_annotator = Maxent_Word_Token_Annotator()
      pos_tag_annotator = Maxent_POS_Tag_Annotator()
      tweets$text <- sapply(tweets$text, function(row) row <- POSTag(row, sent_token_annotator, word_token_annotator, pos_tag_annotator))
      return(tweets)
    }
    
    POSTag <- function(x, sent_token_annotator, word_token_annotator, pos_tag_annotator) {
      s <- as.String(x)
      if(grepl("^\\s*$", s)) return(NA) #if there are no tokens in sentence
      a2 = NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
      a3 = NLP::annotate(s, pos_tag_annotator, a2)
      a3w = subset(a3, type == "word")
      POStags = sapply(a3w$features, `[[`, "POS")
      if(length(s[a3w]) == 1) {
        tags <- data.frame(Tokens = c(s[a3w]), Tags = POStags)
      } else {
        tags <- data.frame(Tokens = s[a3w], Tags = POStags) 
      }
      tags$Tags_mod = grepl("NN|JJ|VB|RB", tags$Tags)
      chunk = vector()  
      chunk[1] = as.numeric(tags$Tags_mod[1])
      for (i in 2:nrow(tags)) {
        if(is.na(tags$Tags_mod[i]) || is.null(tags$Tags_mod[i])) return(NA)
        if(!tags$Tags_mod[i]) {
          chunk[i] = 0
        } else if (tags$Tags_mod[i] == tags$Tags_mod[i-1]) {
          chunk[i] = chunk[i-1]
        } else {
          chunk[i] = max(chunk) + 1
        }
      }
      text_chunk <- split(as.character(tags$Tokens), chunk)
      tag_pattern <- split(as.character(tags$Tags), chunk)
      names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
      res = text_chunk[grepl("NN-JJ|NN-VB|VB-JJ|NN.-NN|JJ|NN|VB", names(text_chunk))]
      res <- unlist(res, use.names=FALSE)
      res <- paste(res, collapse=" ")
      gc()
      return(res) 
    }
    
    readDataSet <- function(filename) {
      dataSet <- read.csv(filename)
      
      dataSet$id <- as.character(dataSet$id)
      dataSet$review <- as.character(dataSet$review)
      dataSet$sentiment <- as.factor(dataSet$sentiment)
      
      return(dataSet)
    }
    
    useSVMWithMatrix <- function(df, index) {
      dtMatrix <- create_matrix(df$text, weighting=weightTfIdf)
      container <- create_container(dtMatrix, df$b, trainSize=1:index, testSize=(index+1):nrow(df), virgin=FALSE)
      svm_model <- train_model(container, "SVM", kernel="linear", cost=1)
      results <- classify_model(container, svm_model)
      start <- index+1
      end <- nrow(df)
      print(nrow(results))
      accuracy <- recall_accuracy(df[(start:end), "b"], results$SVM_LABEL)
      print(accuracy)
    }
    
    useSVMWithoutMatrix <- function(df,index) {
      train_data <- df[(1:index), ] #this must be 80 20
      test_data <- df[-(1:index), ]
      
      svm_model <- svm(
                    #train_data$text ~ factor(train_data$b), 
                    b~.,
                    data = train_data[,2:3],
                    kernel = "linear",
                    #cross=10,
                    cost = 62.5, 
                    gamma = 0.5,
                    scale = FALSE)
      str(train_data)
      str(test_data)
      pred_train <- predict(svm_model, test_data[,2:3])
      print(mean(pred_train==test_data$b))
      print(pred_train)
      
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