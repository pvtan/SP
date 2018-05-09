library(rtweet)
library(syuzhet)
source("SP/config.R")

twitter_token <- create_token(app = "MyTwitterAppSP", consumer_key = api_key, consumer_secret = api_secret)
tweets <- search_tweets2(
  c("#moviereview", 
    "#moviereviews", 
    "#movie #review", 
    "#filmreviews", 
    "#filmreview", 
    "#film #review", 
    "#movieopinion", 
    "#movieopinions", 
    "#moviecritic", 
    "#filmcritic", 
    "#movie #critic", 
    "#film #critic"),
  n = 10000,
  include_rts = FALSE, 
  lang = "en"
)
#tweets <- do_call_rbind(tweets)
tweets.df <- data.frame(lapply(tweets, as.character), stringsAsFactors=FALSE)
write.csv(tweets.df, "C:/Users/Paula Tan/Documents/SP/sentimentrTrainingData.csv")