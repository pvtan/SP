library(caret)
library(e1071)
library(tm)
library(dplyr)

useSVMWithoutMatrix <- function(df,index) {
  train_data <- df[(1:index), ] #this must be 80 20
  test_data <- df[-(1:index), ]
  
  corpus <- Corpus(VectorSource(train_data$text)) # Create a corpus from our character vector
  tdm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTf(x))) # Create the document term matrix
  print(tdm)
  train_set <- as.matrix(tdm)
  save(tdm, file="tdm.RData")
  
  # add the classifier column and make it a data frame
  train_set <- cbind(train_set, train_data$b)
  colnames(train_set)[ncol(train_set)] <- "y"
  train_set <- as.data.frame(train_set)
  train_set$y <- as.factor(train_set$y)
  
  #example1_model <- train(y ~., data = train_set, method = 'svmLinear3')
  example1_model <- svm(
                  y ~ .,
                  data = train_set,
                  type = "C-classification",
                  kernel = "radial",
                  #cross=10,
                  cost = 1, 
                  gamma = 0.1,
                  scale = FALSE)
  
  test_corpus <- Corpus(VectorSource(test_data$text))
  test_tdm <- DocumentTermMatrix(test_corpus, control=list(dictionary = Terms(tdm)))
  test_tdm <- as.matrix(test_tdm)
  
  #tuned_parameters <- tune.svm(y~., data = train_set)
  #summary(tuned_parameters)
  
  #Build the prediction  
  model_toy_result <- predict(example1_model, newdata = train_set)
  print(mean(model_toy_result==train_data$b))
  train_data$b <- as.factor(train_data$b)
  #print(confusionMatrix(model_toy_result, train_data$b))
  model_toy_result <- predict(example1_model, newdata = test_tdm)
  print(mean(model_toy_result==test_data$b))
  saveRDS(example1_model, file="svm_model.rds")
}

tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/trainingData.csv", 
                         header=TRUE, 
                         sep=",", 
                         stringsAsFactors = FALSE)
training <- floor(nrow(tweetDataset) * 0.8)
useSVMWithoutMatrix(tweetDataset, training)
print(training)