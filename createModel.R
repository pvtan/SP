library(caret)
library(e1071)
library(tm)
library(dplyr)
library(RTextTools)

useSVMWithoutMatrix <- function(df,index) {
  train_data <- df[(1:index), ] #this must be 80 20
  test_data <- df[-(1:index), ]
  
  save(train_data, file="train_data.RData")
  corpus <- Corpus(VectorSource(train_data$text)) # Create a corpus from our character vector
  tdm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTf(x))) # Create the document term matrix
  dtMatrix <- RTextTools::create_matrix(df["text"], weighting=tm::weightTfIdf)
  cont <- RTextTools::create_container(dtMatrix, df$b, trainSize = 1:index, testSize = (index+1):nrow(df), virgin = FALSE)
  #print(cont)
  
  #print(dtMatrix)
  #tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
  print(tdm)
  train_set <- as.matrix(tdm)
  save(tdm, file="tdm.RData")
  
  # add the classifier column and make it a data frame
  train_set <- cbind(train_set, train_data$b)
  colnames(train_set)[ncol(train_set)] <- "y"
  train_set <- as.data.frame(train_set)
  train_set$y <- as.factor(train_set$y)
  
  #example1_model <- train(y ~., data = train_set, method = 'svmLinear3')
  example1_model <- train_model(cont, "SVM")
  #print(example1_model)
  example1_model <- svm(
                    y ~ .,
                    data = train_set,
                    type = "C-classification",
                    kernel = "linear",
                    #cross=10,
                    cost = 10, 
                    gamma = 0.01)
  
  test_corpus <- Corpus(VectorSource(test_data$text))
  test_tdm <- DocumentTermMatrix(test_corpus, control=list(dictionary = Terms(tdm)))
  #test_tdm <- TermDocumentMatrix(test_corpus, control = list(weighting = weightTfIdf))
  test_tdm <- as.matrix(test_tdm)
  
  #Build the prediction  
  model_toy_result <- predict(example1_model, newdata = train_set)
  #model_toy_result <- classify_model(cont, example1_model)
  print(mean(model_toy_result==train_data$b))
  test_data$b <- as.factor(test_data$b)
  model_toy_result <- predict(example1_model, newdata = test_tdm)
  cm = as.matrix(confusionMatrix(model_toy_result, test_data$b))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy = sum(diag) / n 
  print(accuracy)
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  pc = data.frame(precision, recall, f1) 
  print(pc)
  #print(table(model_toy_result))
  #print(table(test_data$b))
  print(mean(model_toy_result==test_data$b))
  saveRDS(example1_model, file="svm_model.rds")
}

tweetDataset <- read.csv("C:/Users/Paula Tan/Documents/SP/trainingData1.csv", 
                         header=TRUE, 
                         sep=",", 
                         stringsAsFactors = FALSE)
training <- floor(nrow(tweetDataset) * 0.8)
useSVMWithoutMatrix(tweetDataset, training)
print(training)