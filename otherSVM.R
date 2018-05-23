library(caret)
library(e1071)
library(tm)
library(dplyr)
library(RTextTools)

useSVMWithoutMatrix <- function(df,index) {
  train_data <- df[(1:index), ] #this must be 80 20
  test_data <- df[-(1:index), ]
  
  dtMatrix <- RTextTools::create_matrix(df["text"], weighting=tm::weightTf)
  save(dtMatrix, file="C:/Users/Paula Tan/Documents/SP/dtMatrix1.RData")
  cont <- RTextTools::create_container(dtMatrix, df$b, trainSize = 1:index, testSize = (index+1):nrow(df), virgin = FALSE)
  m <- as.matrix(dtMatrix)
  print(dim(m))
  #print(cont)
  
  example1_model <- train_model(cont, "SVM", kernel = "linear")
  save(example1_model, file="C:/Users/Paula Tan/Documents/SP/svmTrain1.RData")
  print(example1_model)
  #Build the prediction  
  model_toy_result <- classify_model(cont, example1_model)
  analytics = create_analytics(cont, model_toy_result)
  summary(analytics)
  print(table(as.numeric(as.factor(df[(index+1):(nrow(df)), "b"])), model_toy_result[,"SVM_LABEL"]))
  labels <- c("Negative", "Neutral", "Positive")
  print(table(model_toy_result[,"SVM_LABEL"]))
  print(mean(model_toy_result[,"SVM_LABEL"]==df[(index+1):(nrow(df)), "b"]))
  pie(table(model_toy_result[,"SVM_LABEL"]), labels)
}

tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/trainingData1.csv", 
                         header=TRUE, 
                         sep=",", 
                         stringsAsFactors = FALSE)
training <- floor(nrow(tweetDataset) * 0.8)
useSVMWithoutMatrix(tweetDataset, training)
print(training)