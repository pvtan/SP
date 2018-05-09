library(caret)
library(e1071)
library(tm)
library(dplyr)
library(RTextTools)

useSVMWithoutMatrix <- function(df,index) {
  train_data <- df[(1:index), ] #this must be 80 20
  test_data <- df[-(1:index), ]
  
  dtMatrix <- RTextTools::create_matrix(df["text"], weighting=tm::weightTfIdf)
  print(dtMatrix)
  save(dtMatrix, file="C:/Users/Paula Tan/Documents/SP/dtMatrix.RData")
  cont <- RTextTools::create_container(dtMatrix, df$b, trainSize = 1:index, testSize = (index+1):nrow(df), virgin = FALSE)
  #print(cont)
  
  example1_model <- train_model(cont, "SVM", kernel = "linear")
  save(example1_model, file="C:/Users/Paula Tan/Documents/SP/svmTrain.RData")
  #Build the prediction  
  model_toy_result <- classify_model(cont, example1_model)
  analytics = create_analytics(cont, model_toy_result)
  #summary(analytics)
  #print(table(as.numeric(as.factor(df[(index+1):(nrow(df)), "b"])), model_toy_result[,"SVM_LABEL"]))
  saveRDS(example1_model, file="svm_model.rds")
}

tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/trainingData1.csv", 
                         header=TRUE, 
                         sep=",", 
                         stringsAsFactors = FALSE)
training <- floor(nrow(tweetDataset) * 0.8)
useSVMWithoutMatrix(tweetDataset, training)
print(training)