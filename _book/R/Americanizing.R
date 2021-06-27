# Function to turn all British words into American
Americanizing <- function(words,Am2Br){
  Am <- gsub("[[:punct:]]", "", Am2Br$Am)
  Br <- gsub("[[:punct:]]", "", Am2Br$Br)
  word.word <- gsub("[[:punct:]]", "",tolower(unlist(strsplit(words, " "))))
  unlist(lapply(word.word,function(x){
    ind.match <- match(x,Br)
    # print(ind.match)
    if (is.na(ind.match)==FALSE){
      Am.word <- Am[ind.match]
    }else{
      Am.word <- x
    }
    # print(Am.word)
    return(Am.word)
  }))
}
