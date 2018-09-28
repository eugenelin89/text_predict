# Getting and Cleaning Data
library(tidytext)
library(dplyr)

# Loading and Sampling Data
sample_text <- function(input_file_path, output_file_path, prob = 0.1){
  # 1. Read text
  lines <- readLines(input_file_path)
  # 2. sample text
  size <- length(lines)
  sample_index <- as.logical(rbinom(size, 1, prob ))
  output_text <- lines[sample_index]
  # 3. Write to text
  writeLines(output_text, output_file_path)
}

# Task 1: Getting and Cleaning Data
# 1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.
# Input: file path
# Output: Tibble with a single token column.
tokenize_file <- function(input_file_path, badword_file_path = NULL){
  # turn file into character vector
  lines <- readLines(input_file_path)
  text_df <- data_frame(text = lines)
  tokens = text_df %>% unnest_tokens(token, text)
  if(is.null(badword_file_path)){
    tokens <- filter_badwords(badword_file_path, tokens)
  }
  return(tokens)
}

# 2. Profanity filtering - removing profanity and other words you do not want to predict.
filter_badwords <- function(badwords_file_path, tokens){
  lines <- readLines(badwords_file_path)
  text_df <- data_frame(token = lines)
  tokens %>% anti_join(text_df)
}

