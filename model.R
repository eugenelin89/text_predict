library(dplyr)

# input is ngram dataframe
learn_from_ngrams <- function(ngram, model = NULL){
  if(is.null(model)){
    model <- dict()
  }
  for(i in 1: nrow(ngram)){
    rw <- ngram[i,]
    if(is.null(rw$word3)){ # 2-gram
      
    }else{ # 3-gram
      key <- paste(rw$word1, rw$word2, sep = " ") 
      n <- rw$n
      val <- rep(rw$word3, n)
      model[[key]] <- c(model$get(key,character()), val)
    }
  }
  model
}

tfunc <- function(foo = NULL){
  if(is.null(foo)){
    foo = dict()
  }
  foo
}

predict_next_word <- function(){
  
}

bigrams <- readRDS("bigrams.rds")
trigrams <- readRDS("trigrams.rds")
ngrams_model <- learn_from_ngrams(bigrams, trigrams)
next_word <- predict_next_word(ngrams_model, "thanks for")