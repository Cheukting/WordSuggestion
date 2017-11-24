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

allFreq = list(
  read.csv(file = "freq1.csv", col.names = c("word","freq")),
  read.csv(file = "freq2.csv", col.names = c("word","freq")),
  read.csv(file = "freq3.csv", col.names = c("word","freq")))

lastWord <- function(input) tail(str_split(input," ")[[1]],1)

replLastWord <- function(input,addword){
  wordstr <- str_split(input," ")[[1]]
  wordstr[length(wordstr)] <- addword
  
  return(paste(paste(wordstr,collapse = " ")," ",sep = "",collapse = ""))
}

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

findSuggest <- function(input,n){
  inputWords <- str_split(input,"\\s+")[[1]]
  inputWords <- tail(inputWords,n)
  input <- paste("^",paste(inputWords,collapse=" "),sep="",collapse="")
  if (length(inputWords) < n) return (character())
  else return (head(as.character(allFreq[[n]]$word[grep(input,allFreq[[n]]$word)]),3))
}
  
# assume each word need to end in a space (non-letter)

Suggest <- function(input){
  
  #if (nchar(input) < 1){
  #  output <- c("1 the", "2 on", "3 a")
  #  out <- sapply(output,lastWord)
  #  names(out) <- c("one","two","three")
  #  return(out)
  #}
  
  input <- CleanIt(input)
  
  #wordNum always = length(inputWords)-1
  #wordNum <- length(gregexpr("\\s+", input)[[1]])
  output <- vector(mode = "character")
  #outputN <- 3
  
  wordsFound <- findSuggest(input, 3)
  output <- wordsFound
  if (length(output) >= 3){
    output <- head(output,3)
    out <- sapply(output,lastWord)
    names(out) <- c("one","two","three")
    return(out)
  }else{
    wordsFound <- findSuggest(input, 2)
    output <- c(output, wordsFound)
    if (length(output) >= 3){
      output <- head(output,3)
      out <- sapply(output,lastWord)
      names(out) <- c("one","two","three")
      return(out)
    }else{
      wordsFound <- findSuggest(input, 1)
      output <- c(output, wordsFound)
      if (length(output) >= 3){
        output <- head(output,3)
        out <- sapply(output,lastWord)
        names(out) <- c("one","two","three")
        return(out)
      }else{
        output <- c(output,"1 the", "2 on", "3 a")
        output <- head(output,3)
        out <- sapply(output,lastWord)
        names(out) <- c("one","two","three")
        return(out)
      }
    }
  }
  
}

