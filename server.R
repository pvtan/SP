library(shiny)
library(base64enc)
library(rtweet)
library(hunspell)
library(tm)
library(caret)
library(e1071)
library(lime)
library(RTextTools)
library(lexicon)
library(stringr)
library(DataCombine)
library(NLP)
library(openNLP)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
source("config.R")

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
        if(substring(input$query, 1, 1) == "#") { #if hashtag
          movieHashtag <- input$query
          movieQuery <- paste0(movieHashtag)
          print(paste0("Searching for ", movieQuery))
        } else { #if title
          tw <- strsplit(input$query, "\\s+")
          tw <- unlist(tw)
          movie <- paste(tw, collapse="")
          movieHashtag <- paste0("#", movie, collapse="")
          movieQuery <- paste0(input$query, " OR ", movieHashtag)
          print(paste0("Searching for ", movieQuery))
        }
      
        
        tweetsRetrieved <- retrieveTweets(movieQuery)
        print(paste0(nrow(tweetsRetrieved), " tweets retrieved", collapse = ""))
        
        if(nrow(tweetsRetrieved) != 0) {
          tweets.df <- data.frame(lapply(tweetsRetrieved, as.character), stringsAsFactors=FALSE)
          #remove movie in screenName
          tweets.df <- removeMovieFoundInScreenname(substring(movieQuery, 2, nchar(movieQuery)), tweets.df)
          #write to file
          write.csv(tweets.df, file="unprocessedTweets.csv")
          
          #pre-processing of tweets
          withProgress(message = 'Preprocessing tweets', value = 0, {
            processedTweets <- preprocessTweets(tweets.df, "results.csv")
            n <- 100 # Number of times we'll go through the loop
            for (i in 1:n) {
              incProgress(1/n) # Increment the progress bar, and update the detail text.
              Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
            }
          })
          
          load("dtMatrix.RData")
          load("svmTrain.RData")
          matrix <- RTextTools::create_matrix(processedTweets$text, originalMatrix=dtMatrix)
          print(matrix)
          processedTweets$b <- rep(c(1), times=nrow(processedTweets))
          cont <- RTextTools::create_container(matrix, 
                                               processedTweets$b,
                                               testSize = 1:nrow(processedTweets), 
                                               virgin = TRUE)
          print(example1_model)
          results <- classify_model(cont, example1_model)
          
          lookUp1 <- setNames(c("negative", "neutral", "positive"), c(-1, 0 , 1))
          results <- cbind(processedTweets, 'sentiment' = results[, "SVM_LABEL"])
          results["sentiment"] <- lapply(results["sentiment"], function(i) lookUp1[i])
          results <- results[, -which(names(results) %in% c("b"))]
          output$processed_tweets <- renderTable(results)
          
          createPieChart(results$sentiment)
          matrix <- as.matrix(matrix)
          createWordCloud(processedTweets$text, matrix)
        } else {
          print("There are no tweets retrieved!")
        }
    })
    
    createPieChart <- function(movie_result) {
      labels <- c("Negative", "Neutral", "Positive")
      # Plot the chart.
      print(table(movie_result))
      output$pie <- renderPlot(pie(table(movie_result), labels))
      pie(table(movie_result), labels)
      output$bar <- renderPlot(barplot(table(movie_result), 
                                       xlab="Sentiment", names.arg = labels))
      barplot(table(movie_result), 
              xlab="Sentiment", names.arg = labels)
    }
    
    createWordCloud <- function(raw, m) {
      v <- sort(rowSums(m),decreasing=TRUE)
      summary(v)
      output$cloud <- renderPlot({
        wordcloud(raw,
                  min.freq = 1,
                  max.words=50, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
      })
    }
    
    preprocessTweets <- function(tweets.df, filename) {
      processedTweets <- tweets.df
      processedTweets <- cleanTweets(processedTweets)
      processedTweets <- fixContractions(processedTweets)
      processedTweets <- decodeEmojis(processedTweets) 
      processedTweets <- removePunctuationsAndNumbers(processedTweets)
      processedTweets <- expandAcronyms(processedTweets) 
      processedTweets <- fixContractions(processedTweets)
      processedTweets <- shortenLongWords(processedTweets)
      processedTweets <- applyPOSTag(processedTweets)
      compare <- cbind("orig" = tweets.df["text"], "text" = processedTweets["text"])
      compare <- compare[!is.na(compare$text), ]
      cld2Col <- sapply(compare$text, function(row) cld2::detect_language(text = row, plain_text = FALSE))
      compare <- cbind(compare, "cld2Col" = cld2Col)
      compare <- compare[(compare$cld2Col=="en"), ]
      colnames(compare) <- c("original", "text", "cld2")
      compare <- compare[!is.na(compare$text), ]
      compare <- compare[, !(colnames(compare) %in% c("cld2"))]
      print(nrow(compare))
      write.csv(compare, filename)
      return(compare)
    }
    
    retrieveTweets <- function(movieQuery) {
      #from https://shiny.rstudio.com/articles/progress.html
      withProgress(message = 'Retrieving tweets', value = 0, {
        twitter_token <- create_token(app = "MyTwitterAppSP", consumer_key = api_key, consumer_secret = api_secret)
        if(input$retweets) {
          tweets <- search_tweets(movieQuery, n = 500, include_rts = FALSE, lang = "en")
        } else {
          tweets <- search_tweets(movieQuery, n = 500, include_rts = TRUE, lang = "en")
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
      removedContractions <- tweets.df
      removedContractions$text <- textclean::replace_contraction(tweets.df$text)
      
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
      tweets.df$text <- sapply(tweets.df$text, function(row) gsub("( )+e[3-6d-f](\\s[a-zA-Z0-9][a-zA-Z0-9]){2, }( )*", "", row))
      return(tweets.df)
    }
    
    expandAcronyms <- function(tweets.df) {
      #expand acronyms
      slang <- read.csv("dict/noslang.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
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
    
    removePunctuationsAndNumbers <- function(tweets.df) {
      tweets <- tweets.df
      tweets$text <- textclean::replace_ordinal(tweets$text)
      tweets$text <- sapply(tweets.df$text, (function(x) {return (gsub("[[:punct:]]"," ", x))}))
      tweets$text <- removeNumbers(tweets$text)
      tweets$text <- sapply(tweets$text, function(row) gsub("( )+[a-zA-Z]{1}( )+", " ", row))
      return(tweets)
    }
    
    cleanTweets <- function(tweets.df) {
      #to remove unreadable/extra characters
      tweets <- tweets.df
      tweet_vector <- unlist(tweets$text, use.names=FALSE)
      corpus <- Corpus(VectorSource(tweet_vector))
      
      for (i in 1:length(corpus$content)) {
        #from https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
        corpus[[i]]$content = gsub("&amp", "", corpus[[i]]$content)
        corpus[[i]]$content = gsub("@\\w+", "", corpus[[i]]$content) #remove handles
        corpus[[i]]$content = gsub("#\\w+", "", corpus[[i]]$content) #remove hashtags
        corpus[[i]]$content = gsub("http.+", "", corpus[[i]]$content) #remove links
        corpus[[i]]$content = gsub(":", "", corpus[[i]]$content) #remove colons
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
  }
)