tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/trainingData1.csv", 
                         header=TRUE, 
                         sep=",", 
                         stringsAsFactors = FALSE)
print(table(tweetDataset[,"b"]))
labels <- c("Negative", "Neutral", "Positive")
pie(table(tweetDataset[,"b"]), labels)