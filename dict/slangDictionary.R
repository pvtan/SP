slang <- read.csv("C:/Users/Paula Tan/Documents/SP/noslang.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

slang["expansion"] <- NA
for(i in 1:nrow(slang)) {
  line <- as.String(unlist(slang[i, "x"]))
  line <- strsplit(line, ":")
  line <- unlist(line)
  line[1] <- trimws(line[1])
  slang[i, "x"] <- line[1]
  slang[i, "expansion"] <- line[2]
  #print(slang[i, ])
}

slang <- unique(slang[,2:3])
write.csv(slang, file="SP/noslang.csv")
