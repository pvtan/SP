tweets.df <- read.csv("C:/Users/Paula Tan/Documents/SP/preprocessedDataSet1.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
tweets.df["text"] <- sapply(tweets.df["text"], function(row) iconv(row, to="utf-8"))
tweets.df["emotion"] <- NA
tweets.df <- tweets.df[!(tweets.df["text"] == ""), ]
tw <- as.vector(unlist(tweets.df["text"]))
emotion <- get_sentiment(tw, method = "syuzhet", path_to_tagger = "/SP/dict/stanford-corenlp-full-2014-01-04",
              cl = NULL, language = "english", lexicon = NULL)
emotion <- sign(emotion)
SA <- data.frame(text=tw,b=unlist(emotion))
write.csv(SA, "trainingData1.csv")