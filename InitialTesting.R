con <- file("final/en_US/en_US.twitter.txt", "r")
readLines(con, 2)
close(con)

readLines("Textname.txt",encoding="UTF-8")
scan("Textname.txt","character",sep="\n")



strsplit(string,"")
table(sentences)

sentences<-scan("final/en_US/en_US.twitter.txt","character",sep="\n")
sentences<-gsub("\\.","",sentences)
sentences<-gsub("\\,","",sentences)
words<-strsplit(sentences," ")
words.freq<-table(unlist(words))

wordFrq <- cbind.data.frame(names(words.freq),as.integer(words.freq))


library(tau) 
library(data.table)

createNgram <-function(stringVector, ngramSize){
  ngram <- data.table()
  ng <- textcnt(stringVector, method = "string", n=ngramSize, tolower = FALSE)
  
  if(ngramSize==1){
    ngram <- data.table(w1 = names(ng), freq = unclass(ng), length=nchar(names(ng)))  
  }
  else {
    ngram <- data.table(w1w2 = names(ng), freq = unclass(ng), length=nchar(names(ng)))
  }
  
  return(ngram)
}

res <- createNgram(sentences, 2)
