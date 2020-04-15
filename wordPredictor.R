library(tm)


# Load N-Grams (if necessary)
load("./ngrams/unigram.RData")
load("./ngrams/bigram_split.RData" )
load("./ngrams/trigram_split.RData")
unigram <- unigram_sorted
unigram$Token <- as.character(unigram$Token)

# Define some functions to make word predictions easier

# Finds trigrams that share 2 of the same words that
# the user input sentence ends with
find_trigrams <- function(trigrams, word_1, word_2){
  has_both_words <- trigrams[,1] == word_1 & trigrams[,2] == word_2
  return(trigrams[has_both_words,])
}

# Finds bigrams that shares the same word as the last
# word in the user input sentence
find_bigrams <- function(bigrams, word_1){
  has_word <- bigrams[,1] == word_1
  return(bigrams[has_word,])
}

# Get user input sentence and clean it
predict_next_word <- function(user_input_sentence){
  sentence_lower <- tolower(user_input_sentence)
  sentence_no_num <- removeNumbers(sentence_lower)
  sentence_no_punct <- removePunctuation(sentence_no_num)
  sentence <- strsplit(sentence_no_punct, split=" ")[[1]]
  
  length_sentence <- length(sentence)
  
  # Find trigrams that share the same words as the 
  # last 2 words in the sentence
  if(length_sentence >= 2){
    word_1 <- sentence[length_sentence-1]
    word_2 <- sentence[length_sentence]
    matches <- find_trigrams(trigram, word_1, word_2)
    
    # Return 3rd word in top resulting trigram 
    if(dim(matches)[1] > 0){
      return(matches[1, 3])
    }
    # If there are no matching trigrams, search for bigrams
    else{
      matches <- find_bigrams(bigram, word_2)
      if(dim(matches)[1] > 0){
        return(matches[1, 2])
      }
      # if there are no bigrams, return top unigram
      else{
        return(unigram[1,1])
      }
    }
  }
  else if(length_sentence == 1){
    word_1 <- sentence[length_sentence]
    # Search for bigrams that match the last word in the sentence
    matches <- find_bigrams(bigram, word_1)
    if(dim(matches)[1] > 0){
      return(matches[1, 2])
    }
    # if there are no bigrams, return top unigram
    else{
      return(unigram[1,1])
    }
  }
  # If there are no words in input sentence, return NA
  else{
    return(NA)
  }
}