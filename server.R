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
#library(RTextTools)
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
      movie <- input$query
      bad_words <- hunspell_find(movie)
      sugg <- hunspell_suggest(bad_words[[1]])
      print(bad_words[[1]])
      sugg <- unlist(sugg)
      print(sugg)
      if (!is.null(sugg)) { 
        movie <- gsub(bad_words[[1]], sugg[[1]], movie) 
      }
      
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
      write.csv(tweets.df, file="unprocessedTweets.csv")
      tweets.df$screenName <- tolower(tweets.df$screenName)
      movie <- tolower(input$query)
      filterOut <- tweets.df[grep(movie, tweets.df$screenName),]
      if(nrow(filterOut) != 0) {
        tweets.df <- tweets.df[- grep(movie, tweets.df$screenName),] #remove movie name found in screen name
      }
      output$tweets <- renderDataTable(tweets.df["text"])
      tweets.df["text"] <- sapply(tweets.df["text"],
                          function(row) iconv(row, "latin1", "ASCII", sub="")) #to remove emojis
      tweet_vector <- unlist(tweets.df["text"], use.names=FALSE)
      corpus <- Corpus(VectorSource(tweet_vector))
      print(corpus$content)
      if(length(corpus$content) == 0) {
        print("There are no tweets to undergo pre-processing.")
      } 
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
      output$processed_tweets <- renderTable(corpus$content)
      #write.csv(corpus$content, file="tweets.csv")
      #from https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd
      #convert tweets to a vector
      #if (file.exists("tweets.bin")) file.remove("tweets.bin")
      #model = train_word2vec("tweets.txt","tweets.bin",
      #                        vectors=100,threads=4,window=12,iter=5,negative_samples=0)
      #print(model %>% closest_to("good"))
    
      #data("movie_review")
      df <- read.csv("tweets.csv")
      #it <- itoken(movie_review[['review']], 
      #             preprocess_function = tolower, 
      #             tokenizer = word_tokenizer)
      #vocab <- create_vocabulary(it)
      #vectorizer <- vocab_vectorizer(vocab)
      #it = itoken(movie_review[['review']], 
      #          tokenizer = word_tokenizer)
      #dtm_train = create_dtm(it, vectorizer)
      #movie_review <- movie_review[1:91,]
      df$id <- as.character(df$id)
      df$review <- as.character(df$review)
      df$sentiment <- as.factor(df$sentiment)
      train_data <- df[(1:33), ]
      test_data <- df[-(1:33), ]
      
      svm_model <- svm(
                    train_data$sentiment ~ train_data$review, 
                    data = train_data,
                    kernel = "linear",
                    #cross=10,
                    cost=10, 
                    scale=FALSE)
      
      pred_train <- predict(svm_model, test_data)
      print(mean(pred_train==test_data$sentiment))
      #print(pred_train)
      
      #model <- train(review ~ sentiment, data = train_data, method = "svmLinear2")
      #from https://github.com/thomasp85/lime
      #explainer <- lime(train_data, model)
      #explanation <- explain(test_data, explainer, n_labels = 3, n_features = 1)
      #head(explanation)
      #plot_features(explanation)
      
      tab <- table(pred=pred_train, true=test_data$sentiment)
      results <- confusionMatrix(tab)
      results <- as.matrix(results)
      print(results)
      x <- c(
        sum(results["negative", "negative"], results["negative", "neutral"], results["negative", "positive"]),
        sum(results["neutral", "negative"], results["neutral", "neutral"], results["neutral", "positive"]),
        sum(results["positive", "negative"], results["positive", "neutral"], results["positive", "positive"])
      )
      labels <- c("Negative", "Neutral", "Positive")
      # Plot the chart.
      output$pie <- renderPlot(pie(x,labels))
    })
  }
)