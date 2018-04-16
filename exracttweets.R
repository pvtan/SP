#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("RMongo")
#install.packages("rjson")
#install.packages("rJava",,"http://rforge.net/",type="source")

library(base64enc)
library(twitteR)
#library(RMongo)
#library(rjson)

api_key <- "6vLKhDh7UJdO1vMwYOnDbrr91"
api_secret <- "6UY2wIWDTSrOgpQVziA9sUFlPBIDhodcUmmz2KheFLkXc0xJF8"
access_token <- "1328460499-EmARRTuftN6dqehvZ7CgeNKqIpB0JIWoT3EvjKi"
access_token_secret <- "1Sz7gKFQS8Gz0Y1zln7XO9gXIWccsw9hgrBeuoFhUdyrS"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets <- searchTwitter('Heneral Luna', n=10, lang="en")
tweets.df <- twListToDF(tweets)
print(tweets.df["text"])
#write.csv(tweets.df, "tweets.csv")

#mongo <- mongoDbConnect("movisenti", "localhost", 27017)
#dbRemoveQuery(mongo, "mycollection", {});

#for(i in 1:nrow(tweets.df)) {
#  tw <- c(text=unlist(tweets.df[i, "text"]));
#  r <- dbGetQuery(mongo, 'mycollection', toJSON(tw));
#  if(is.null(unlist(r))) dbInsertDocument(mongo, "mycollection", toJSON(tw));
#}

#print(dbGetQuery(mongo, 'mycollection', '{}')); #get all
#dbDisconnect(mongo)