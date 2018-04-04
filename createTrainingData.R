#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#library(devtools)
#devtools::install_github("IndicoDataSolutions/IndicoIo-R")

library(rtweet)
library(indicoio)
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
    "#film #critic",
    "#bollywood",
    "#hollywood"),
  n = 1000,
  include_rts = FALSE, 
  lang = "en"
)
tweets <- do_call_rbind(tweets)
tweets.df <- data.frame(lapply(tweets, as.character), stringsAsFactors=FALSE)
tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, to="utf-8"))
tweets.df["emotion"] <- NA
tw <- as.vector(unlist(tweets.df["text"]))
emotion <- sentiment(tw, api_key = '788c54b98bd2281122a1bf42ee013cc9')
SA <- data.frame(text=tw,b=unlist(emotion))
write.csv(SA, "trainingData.csv")