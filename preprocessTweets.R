#install.packages("sentimentr")

tweets.df <- read.csv("C:/Users/Paula Tan/Documents/SP/preprocessedDataSet2.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
tweets.df <- tweets.df[!(tweets.df["text"] == ""), ]
texts <- sentimentr::get_sentences(tweets.df$text)
s <- sentimentr::sentiment_by(texts, by = NULL) 
for(i in 1: nrow(s)) {
  #print(s$ave_sentiment[i])
  if(s$ave_sentiment[i] == 0) s$sent[i] = 0
  else if(s$ave_sentiment[i] > 0) s$sent[i] = 1
  else if(s$ave_sentiment[i] < 0) s$sent[i] = -1
} 

tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, to="utf-8"))
tweets.df["emotion"] <- NA
tw <- as.vector(unlist(tweets.df["text"]))
SA <- data.frame(text=tw,b=unlist(s$sent))
write.csv(SA, "C:/Users/Paula Tan/Documents/SP/trainingData2.csv")

