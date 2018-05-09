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

#function to remove contractions in an English-language source
#function from https://github.com/mkfs/misc-text-mining/blob/master/R/wordcloud.R
contractions <- data.frame(word = c("won't", "wont", "don't", "dont", "'ll", "therell", "'re", "'ve", "I'm", " im ", "'s", "can't", "cant", "didnt", "n't"),
                           expansion = c("will not", "will not", "do not", "do not"," will", "there will", " are", " have", "I am", " I am ", "", "can not", "can not", "did not", " not"))

shinyServer(
  function(input, output) {
    observeEvent(input$enter, {
      #check if hastag or title
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
      
      #spell checker
      #tw <- strsplit(input$query, "\\s+")
      #tw <- unlist(tw)
      #for(i in 1:length(tw)) {
      #  tw[i] <- spellCheck(tw[i])
      #}
      #movie <- paste(tw, collapse=" ") 
      
      tweetsRetrieved <- retrieveTweets(movieQuery)
      tweets.df <- data.frame(lapply(tweetsRetrieved, as.character), stringsAsFactors=FALSE)
      #remove movie in screenName
      tweets.df <- removeMovieFoundInScreenname(movie, tweets.df)
      #write to file
      write.csv(tweets.df, file="unprocessedTweets.csv")
      
      #pre-processing of tweets
      if(length(tweets.df) == 0) {
        print("There are no tweets to undergo pre-processing.")
      } else {
        if(input$preprocess) {
          tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/syuzhetTrainingData1.csv", 
                                   header=TRUE, 
                                   sep=",", 
                                   stringsAsFactors = FALSE)
          tweets.df <- preprocessTweets(tweetDataset, "preprocessedDataSet1.csv")
        } else {
          withProgress(message = 'Preprocessing tweets', value = 0, {
            processedTweets <- preprocessTweets(tweets.df, "results.csv")
            n <- 100 # Number of times we'll go through the loop
            for (i in 1:n) {
              incProgress(1/n) # Increment the progress bar, and update the detail text.
              Sys.sleep(0.1) # Pause for 0.1 seconds to simulate a long computation.
            }
          })
          
          load("C:/Users/Paula Tan/Documents/SP/dtMatrix.RData")
          load("C:/Users/Paula Tan/Documents/SP/svmTrain.RData")
          matrix <- RTextTools::create_matrix(processedTweets$text, originalMatrix=dtMatrix)
          print(matrix)
          processedTweets$b <- rep(c(1), times=nrow(processedTweets))
          cont <- RTextTools::create_container(matrix, 
                                               processedTweets$b,
                                               testSize = 1:nrow(processedTweets), 
                                               virgin = TRUE)
          print(example1_model)
          results <- classify_model(cont, example1_model)
          
          #svm_model <- readRDS("C:/Users/Paula Tan/Documents/svm_model.rds")
          #load("C:/Users/Paula Tan/Documents/tdm.RData")
          #movie_corpus <- Corpus(VectorSource(processedTweets$text))
          #to_save <- processedTweets$text
          #movie_tdm <- DocumentTermMatrix(movie_corpus, control=list(dictionary = Terms(tdm)))
          #movie_tdm <- as.matrix(movie_tdm)
          #movie_result <- predict(svm_model, newdata = movie_tdm)
          results <- cbind(processedTweets, 'sentiment' = results[, "SVM_LABEL"])
          results <- results[, -which(names(results) %in% c("b"))]
          output$processed_tweets <- renderTable(results)
          
          createPieChart(results$sentiment)
          matrix <- as.matrix(matrix)
          createWordCloud(processedTweets$text, matrix)
        }
      }
    })
    
    createPieChart <- function(movie_result) {
      labels <- c("Negative", "Neutral", "Positive")
      # Plot the chart.
      print(table(movie_result))
      output$pie <- renderPlot(pie(table(movie_result),labels))
      output$bar <- renderPlot(barplot(table(movie_result), 
                                       xlab="Sentiment"))
    }
    
    createWordCloud <- function(raw, m) {
      v <- sort(rowSums(m),decreasing=TRUE)
      summary(v)
      output$cloud <- renderPlot({
        wordcloud(raw,
                  max.words = 200,
                  scale=c(5,0.5), 
                  random.order=FALSE, 
                  rot.per=0.35, 
                  use.r.layout=FALSE, 
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
      write.csv(compare, file = paste0("C:/Users/Paula Tan/Documents/SP/", filename, collapse = ""))
      print("file written")
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
      #removedContractions <- FindReplace(data = tweets.df, Var = "text", 
      #            replaceData = contractions,
      #            from = "word", to = "expansion", 
      #            exact = FALSE) #DataCombine
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
    
    removePunctuationsAndNumbers <- function(tweets.df) {
      tweets <- tweets.df
      tweets$text <- removePunctuation(tweets$text)
      tweets$text <- removeNumbers(tweets$text)
      tweets$text <- sapply(tweets$text, function(row) gsub("( )+[a-zA-Z]{1}( )+", " ", row))
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
      #tags$Tags_mod = grepl("JJ|VB|RB", tags$Tags)
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
      #res = text_chunk[grepl("VB-JJ|JJ|VB", names(text_chunk))]
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
  }
)