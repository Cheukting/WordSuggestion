library(stringr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tm)
library(RWeka)
options(mc.cores=1)

## define cleaning sample function

# helper functions
removeHashTags <- function(x) gsub("#\\S+", " ", x)
removeTwitterHandles <- function(x) gsub("@\\S+", " ", x)
removeURL <- function(x) gsub("http:[[:alnum:]]*", " ", x)
removeApostrophe <- function(x) gsub("'", "", x)
removeNonLetters <- function(x) gsub("[^a-zA-Z\\s]", " ", x)
removeSingleChar <- function(x) gsub("\\s\\S\\s", " ", x)

# cleaning function
CleanIt <- function(input){
  input <- removeHashTags(input)
  input <- removeTwitterHandles(input)
  input <- removeURL(input)
  input <- removeApostrophe(input)
  input <- removeNonLetters(input)
  input <- tolower(input)
  input <- removeSingleChar(input)
  return(input)
}

## define sampling function

# tokenize funciton
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


# sample funciton
SampleIt <- function(lines){
  lines <- CleanIt(lines)
  MyCorpus <- VCorpus(VectorSource(lines))
  rm(lines)
  
  SingleTokenTDM <- TermDocumentMatrix(MyCorpus)
  unifreq <- sort(rowSums(as.matrix(SingleTokenTDM)),decreasing = TRUE)
  unifreqdf <- data.frame(word=names(unifreq), freq=unifreq)
  rm(SingleTokenTDM)
  rm(unifreq)
  
  BigramTokenTDM <- TermDocumentMatrix(MyCorpus, control = list(tokenize = BigramTokenizer))
  bifreq <- sort(rowSums(as.matrix(BigramTokenTDM)),decreasing = TRUE)
  bifreqdf <- data.frame(word=names(bifreq), freq=bifreq)
  rm(BigramTokenTDM)
  rm(bifreq)
  
  TrigramTokenTDM <- TermDocumentMatrix(MyCorpus, control = list(tokenize = TrigramTokenizer))
  trifreq <- sort(rowSums(as.matrix(TrigramTokenTDM)),decreasing = TRUE)
  trifreqdf <- data.frame(word=names(trifreq), freq=trifreq)
  rm(TrigramTokenTDM)
  rm(trifreq)
  
  rm(MyCorpus)
  
  output <- list(unifreqdf,bifreqdf,trifreqdf)
  #names(output) <- c("unifreq","bifreq","trifreq","fourfreq","fivefreq")
  
  return(output)
}

## custom join to add freq df together
myJoin <- function(x,y){
  if (dim(x)[1] == 0){
    out <- y
  }else if (dim(y)[1] == 0){
    out <- x
  }else{
    out <- full_join(x,y,by="word")
    out[is.na(out)] <- 0
    out <- transmute(out,word = word,freq = freq.x+freq.y)
  }
  return(out)
}

samplePotion = 0.1
seed=333

#dataCounts <- data.frame(files=c("twitter","blogs","news"),lines=c(0,0,0),words=c(0,0,0))
initdf <- data.frame(word=character(), freq=numeric())
allSample <- list(initdf,initdf,initdf)

conT <- file("en_US/en_US.twitter.txt", "rb") 
while (length(lines <- readLines(conT, 100, encoding = "UTF-8", skipNul = TRUE)) > 0){
  if (runif(1) < samplePotion){
    sample <- SampleIt(lines)
    for(i in (1:3)){
      allSample[[i]] <- myJoin(allSample[[i]],sample[[i]])
    }
  }
}
close(conT)

## triming freq < 2
for(i in (1:3)){
  allSample[[i]] <- allSample[[i]][allSample[[i]]$freq>1,]
}

print("finish sampling twitter")

conB <- file("en_US/en_US.blogs.txt", "rb") 
while (length(lines <- readLines(conB, 100, encoding = "UTF-8", skipNul = TRUE)) > 0){
  if (runif(1) < samplePotion){
    sample <- SampleIt(lines)
    for(i in (1:3)){
      allSample[[i]] <- myJoin(allSample[[i]],sample[[i]])
    }
  }
}
close(conB)

## triming freq < 2
for(i in (1:3)){
  allSample[[i]] <- allSample[[i]][allSample[[i]]$freq>1,]
}

print("finish sampling blogs")

conN <- file("en_US/en_US.news.txt", "rb") 
while (length(lines <- readLines(conN, 100, encoding = "UTF-8", skipNul = TRUE)) > 0){
  if (runif(1) < samplePotion){
    sample <- SampleIt(lines)
    for(i in (1:3)){
      allSample[[i]] <- myJoin(allSample[[i]],sample[[i]])
    }
  }
}
close(conN)

## triming freq < 2
for(i in (1:3)){
  allSample[[i]] <- allSample[[i]][allSample[[i]]$freq>1,]
}

rm(sample)

## write freqs into csv files
for(i in (1:3)){
  write.csv(allSample[[i]][order(allSample[[i]]$freq,decreasing = TRUE),]
            ,file = paste("freq",i,".csv",sep=""),row.names = FALSE)
}

