library(NLP)
library(openNLP)

tagPOS <-  function(x) {
  s <- as.String(x)
  sent_token_annotator = Maxent_Sent_Token_Annotator()
  word_token_annotator = Maxent_Word_Token_Annotator()
  a2 = annotate(s, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator = Maxent_POS_Tag_Annotator()
  a3 = annotate(s, pos_tag_annotator, a2)
  a3w = subset(a3, type == "word")
  POStags = sapply(a3w$features, `[[`, "POS")
  tokenizedAndTagged <- data.frame(Tokens = s[a3w], Tags = POStags)
  gc()
  return(tokenizedAndTagged)
}

dataSet <- read.csv("C:/Users/Paula Tan/Documents/SP/dict/syuzhetTrainingData.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
string <- (dataSet[1, "text"])
tags <- tagPOS(string)
print(tags)
