library(dplyr)
library(dict)

# input is ngram dataframe
learn_from_ngrams <- function(ngram, model = NULL){
  
  
  model_df <- data.frame(matrix(ncol = 2, nrow = nrow(ngram)))
  colnames(model_df) <- c("key", "value")
  n = 0
  if(!is.null(model)){
    n = nrow(model) # number of entries
    model_df <- rbind(model, model_df)
  }
  
  for(i in 1: nrow(ngram)){
    if(i%%100 == 0){
      print(i) # for visual inspection
    }
    rw <- ngram[i,]
    if(is.null(rw$word3)){ # 2-gram
      
    }else{ # 3-gram
      key <- paste(rw$word1, rw$word2, sep = " ") 
      val <- paste(rep(rw$word3, rw$n), collapse = " ")
      
      y <- which(model_df$key==key)
      # model[[key]] <- c(model$get(key,character()), val)
      if(length(y) == 0){
        # Add new entry
        n = n + 1
        model_df[n,] <- c(key, val)
      }else{
        # Update existing entry
        cur_val <- model_df[y,"value"]
        model_df[y,"value"] <- paste(c(cur_val, val), collapse = " ")
      }
    }
  }
  model_df[complete.cases(model_df),]
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