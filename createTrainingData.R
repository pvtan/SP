#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#library(devtools)
#devtools::install_github("IndicoDataSolutions/IndicoIo-R")

library(rtweet)
library(indicoio)
library(syuzhet)
source("SP/config.R")

twitter_token <- create_token(app = "MyTwitterAppSP", consumer_key = api_key, consumer_secret = api_secret)
tweets <- Map(
  "search_tweets",
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
  n = 1000,
  include_rts = FALSE, 
  lang = "en"
)
tweets <- do_call_rbind(tweets)
tweets.df <- data.frame(lapply(tweets, as.character), stringsAsFactors=FALSE)
tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, to="utf-8"))
tweets.df["emotion"] <- NA
tw <- as.vector(unlist(tweets.df["text"]))
emotion <- get_sentiment(tw, method = "syuzhet", path_to_tagger = "/SP/dict/stanford-corenlp-full-2014-01-04",
              cl = NULL, language = "english", lexicon = NULL)
emotion <- sign(emotion)
SA <- data.frame(text=tw,b=unlist(emotion))
write.csv(SA, "trainingData.csv")