library(NLP)
library(openNLP)

tagPOS <-  function(x) {
  s <- as.String(x)
  sent_token_annotator = Maxent_Sent_Token_Annotator()
  word_token_annotator = Maxent_Word_Token_Annotator()
  a2 = NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator = Maxent_POS_Tag_Annotator()
  a3 = NLP::annotate(s, pos_tag_annotator, a2)
  a3w = subset(a3, type == "word")
  POStags = sapply(a3w$features, `[[`, "POS")
  tokenizedAndTagged <- data.frame(Tokens = s[a3w], Tags = POStags)
  tokenizedAndTagged$Tags_mod = grepl("NN|JJ|VB|RB", tokenizedAndTagged$Tags)
  chunk = vector()  
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  for (i in 2:nrow(tokenizedAndTagged)) {
    if(!tokenizedAndTagged$Tags_mod[i]) {
      chunk[i] = 0
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      chunk[i] = chunk[i-1]
    } else {
      chunk[i] = max(chunk) + 1
    }
  }
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  res = text_chunk[grepl("NN-JJ|NN-VB|VB-JJ|NN.-NN|JJ|NN|VB", names(text_chunk))]
  res <- unlist(res, use.names=FALSE)
  res <- paste(res, collapse=" ")
  gc()
  return(res)
}

dataSet <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/syuzhetTrainingData.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
string <- (dataSet[1, "text"])
tags <- tagPOS(string)
print(tags)
