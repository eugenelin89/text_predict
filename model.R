library(dplyr)
library(dict)

# input is ngram vector
learn_from_ngrams <- function(ngram){ 
  
  model <- list()
  
  
  for(i in 1: length(ngram)){
  #for(i in 1: 1000){
    rw <- ngram[i]
    str_vec <- strsplit(rw ,split=',', fixed=TRUE)[[1]] # e.g. [1] "99"   "at"   "the"  "time" "522"
    key <- paste(str_vec[2], str_vec[3], sep = " ")  #paste(rw$word1, rw$word2, sep = " ")
    val <- paste(rep(str_vec[4], strtoi(vec[5])), collapse = " ")  #paste(rep(rw$word3, rw$n), collapse = " ")
      
    cur_val <- model[[key]]
    if(is.null(cur_val)){
      # new entry
      model[key] = val
    }else{
      model[key] = paste(c(cur_val, val), collapse = " ")
    }
    # y <- which(model_df$key==key)
    # 
    # if(length(y) == 0){
    #   # Add new entry
    #   n = n + 1
    #   model_df[n,] <- c(key, val)
    # }else{
    #   # Update existing entry
    #   cur_val <- model_df[y,"value"]
    #   model_df[y,"value"] <- paste(c(cur_val, val), collapse = " ")
    # }
    
    if(i%%100 == 0){
      print(i) # for visual inspection
      #print(rw)
      #print(key)
      #print(val)
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