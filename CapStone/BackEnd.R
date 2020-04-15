load("ngrams/unigram.RData")
load("ngrams/bigram.RData" )
load("ngrams/trigram.RData")
unigram <- unigram_sorted
bigram <-  bigram_sorted
trigram <- trigram_sorted
#rm(unigram_sorted, bigram_sorted, trigram_sorted)
unigram$Token <- as.character(unigram$Token)
bigram$Token <- as.character( bigram$Token)
trigram$Token <- as.character(trigram$Token)

split_n_gram <- function(x, n){
  # Make an empty data frame
  df <- as.data.frame(matrix(NA, nrow=dim(x)[1], ncol=n+1))
  
  # Split each row of n gram by word and fill empty data frame
  for(i in 1:dim(x)[1]){
    for(j in 1:n){df[i,j] <- strsplit(x[i,1], split=" ")[[1]][j]}
    df[i,n+1] <- x[i, 2]
  }
  colnames(df) <- 1:(n+1)
  colnames(df)[n+1] <- "Frequency"
  
  # return filled data frame
  return(df)
}
# Split bi and trigrams
bigram <- split_n_gram( bigram, 2)
trigram <- split_n_gram(trigram, 3)
save( bigram, file = "ngrams/bigram_split.RData" )
save(trigram, file = "ngrams/trigram_split.RData")

