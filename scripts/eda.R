# Exploratory Data Analysis
# Considerations:
# a. What are the distributions of word frequencies?
#  - term_frequency_count.png 
#  - More of low frequency words and few high frequency words
# b. Frequencies of 2-grams and 3-grams in the dataset?
#  - Craps out running 3-grams. Working with 2-grams for now.
# c. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# d. How many of the words come from foreign languages?
#  - how to evaluate???
# e. Increase the coverage? Identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?


library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
source("scripts/task1_gcd.R")

# A. Reading data
news <-  readLines("data/en_US/en_US.news.txt")
twitter <- readLines("data/en_US/en_US.twitter.txt") # vector of lines
blogs <- readLines("data/en_US/en_US.blogs.txt")

# B. Tidy Text
# Putting document text into dataframe
news_df <- data_frame(line = 1:length(news), text=news)
twitter_df <- data_frame(line = 1:length(twitter), text=twitter)
blogs_df <- data_frame(line = 1:length(blogs), text=blogs)

# Transform into tidy data structure, one token per row. 
tidy_news <- news_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) 
tidy_twitter <- twitter_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) 
tidy_blogs <- blogs_df %>% unnest_tokens(word, text)  %>% anti_join(stop_words)  

# Take a quick look at our data.
# Prob a good idea to use regular expression to include only single words.
tidy_news %>% 
  count(word, sort=TRUE) %>%   
  filter(n > 20000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
ggsave(("quicky_word_count.png"))

# C. TF-IDF Analysis.
corpus <- bind_rows(mutate(tidy_news, source="news"), mutate(tidy_twitter, source="twitter"), mutate(tidy_blogs, source="blogs"))
doc_words <- corpus %>% count(source, word, sort=TRUE) %>% ungroup()
total_words <- doc_words %>% group_by(source) %>% summarize( total = sum(n))
doc_words <- left_join(doc_words, total_words)

# Plot shows same a comon pattern for blogs, news, and twitter.
# Many rare words with low frequency, and a few popular words with high frequency in the long tail.
ggplot(doc_words, aes(n/total, fill = source)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.000005) +
  facet_wrap(~source, ncol = 2, scales = "free_y")
ggsave(("term_frequency_count.png"))

# Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- doc_words %>% 
  group_by(source) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
freq_by_rank

# Visualize Zipf's Law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = source)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
ggsave(("term_frequency_vs_rank.png"))
# Taking the "smooth" section from the above plot and perform linear regression
rank_subset <- freq_by_rank %>% 
  filter(rank > 10, rank < 10000)


lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
# Coefficients:
#   (Intercept)  log10(rank)  
# -0.8537      -0.9882 

# Plotting term frequency vs rank with regression line superimposed. Result close to Zipf's Law.
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = source)) + 
  geom_abline(intercept =  -0.8537, slope = -0.9882, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
ggsave(("zipf.png"))
# TF-IDF
doc_words <- doc_words %>%
  bind_tf_idf(word, source, n)
doc_words

# Arrange the tokens by tf-idf. 
# Prob. good idea to filter profanity
doc_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

doc_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(source) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~source, ncol = 2, scales = "free") +
  coord_flip()

ggsave(("tfidf_by_source.png"))

# Looks like some cleaning up is necessary in each source

# For Blog:
# Where does "iii" come from?
# Looks like roman numeral iii. Since i and ii appears in more documents, idf silenced i and ii. But for iii, there's high tf on a few documents.
# Need to deal with Roman Numerals. Same thing goes with news.
# Aso need to deal with Amazon domains.
grep("\\biii\\b", blogs, value = T)
grep("\\biii\\b", news, value = T)

# Filter profanity and clean up and re-run analysis

# D. n-gram
tidy_news_bigrams <- news_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2) 
tidy_twitter_bigrams <- twitter_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2) 
tidy_blogs_bigrams <- blogs_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2)

# tidy_news_trigrams <- news_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3) 
# tidy_twitter_trigrams <- twitter_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3) 
# tidy_blogs_trigrams <- blogs_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3)

tidy_news_bigrams %>% count(bigram, sort = TRUE)
tidy_twitter_bigrams %>% count(bigram, sort = TRUE)
tidy_blogs_bigrams %>% count(bigram, sort = TRUE)

# tidy_news_trigrams %>% count(trigram, sort = TRUE)
# tidy_twitter_trigrams %>% count(trigram, sort = TRUE)
# tidy_blogs_trigrams %>% count(trigram, sort = TRUE)
# lots of uninteresting words such as "of the", "for the","in the"...etc. but for text prediction they may be correct.

# Filtering out uninteresting words and see what we get.
news_bigrams_separated <- tidy_news_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")
twitter_bigrams_separated <- tidy_twitter_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")
blogs_bigrams_separated <- tidy_blogs_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")

# news_trigrams_separated <- tidy_news_trigrams %>%
#   separate(trigram, c("word1","word2","word3"), sep = " ")
# twitter_trigrams_separated <- tidy_twitter_trigrams %>%
#   separate(trigram, c("word1","word2","word3"), sep = " ")
# blogs_trigrams_separated <- tidy_blogs_trigrams %>%
#   separate(trigram, c("word1","word2","word3"), sep = " ")

# Filter out stop words
news_bigrams_filtered <- news_bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
twitter_bigrams_filtered <- twitter_bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
blogs_bigrams_filtered <- blogs_bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)

# news_trigrams_filtered <- news_trigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word)
# twitter_trigrams_filtered <- twitter_trigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word)
# blogs_trigrams_filtered <- blogs_trigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word)

# new bigram counts
news_bigrams_counts <- news_bigrams_filtered %>% count(word1, word2, sort = TRUE)
twitter_bigrams_counts <- twitter_bigrams_filtered %>% count(word1, word2, sort = TRUE)
blogs_bigrams_counts <- blogs_bigrams_filtered %>% count(word1, word2, sort = TRUE)

# news_trigrams_counts <- news_trigrams_filtered %>% count(word1, word2, word3, sort = TRUE)
# twitter_trigrams_counts <- twitter_trigrams_filtered %>% count(word1, word2, word3, sort = TRUE)
# blogs_trigrams_counts <- blogs_trigrams_filtered %>% count(word1, word2, word3, sort = TRUE)

news_bigrams_counts
twitter_bigrams_counts
blogs_bigrams_counts
# news_trigrams_counts
# twitter_trigrams_counts
# blogs_trigrams_counts

total_bigram_counts <- bind_rows(news_bigrams_filtered, twitter_bigrams_filtered, blogs_bigrams_filtered) %>% 
  count(word1, word2, sort = TRUE)
# total_trigram_counts <- bind_rows(news_trigrams_filtered, twitter_trigrams_filtered, blogs_trigrams_filtered) %>% 
#   count(word1, word2, word3, sort = TRUE)

# Try putting the documents togher first and if we get same result
# combined_bigrams <- bind_rows(news_df, blogs_df, twitter_df) %>% unnest_tokens(bigram, text, token = "ngrams", n=2)
# combined_bigrams_separated <- combined_bigrams %>%
#   separate(bigram, c("word1","word2"), sep = " ")
# combined_bigrams_filtered <- combined_bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# 
# combined_trigrams <- bind_rows(news_df, blogs_df, twitter_df) %>% unnest_tokens(trigram, text, token = "ngrams", n=3)
# combined_trigrams_separated <- combined_trigrams %>%
#   separate(trigram, c("word1","word2","word3"), sep = " ")
# combined_trigrams_filtered <- combined_trigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word)


