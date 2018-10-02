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
source("scripts/gcd.R")

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

# Using full data looks too big for R. Trying to use a subset of the data for n-gram analaysis and modelling.
test_tidy_twitter <- twitter_df %>% unnest_tokens(word, text) # 30093369 obs
test_tidy_news <- news_df %>% unnest_tokens(word, text)       # 34762395 obs
test_tidy_blogs <- blogs_df %>% unnest_tokens(word, text)     # 37546246 obs
test_tidy_twitter_size <- object.size(test_tidy_twitter) # 3.81e08
test_tidy_news_size <- object.size(test_tidy_news)       # 4.32e08
test_tidy_blogs_size <- object.size(test_tidy_blogs)     # 4.67e08
# Looks like twitter is the smalletst dataset in tidy format. Lets experiment with that.
tidy_twitter_bigrams <- twitter_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2) # 27733684 obs
tidy_twitter_bigrams_size <- object.size(tidy_twitter_bigrams) # 6.39e08
# Perhaps try subset instead, this is taking too long....
sample_text("data/en_US/en_US.news.txt", "sample_news.txt")
sample_text("data/en_US/en_US.twitter.txt", "sample_twitter.txt")
sample_text("data/en_US/en_US.blogs.txt", "sample_blogs.txt")
# A. Reading data subset
sub_news <-  readLines("sample_news.txt")
sub_twitter <- readLines("sample_twitter.txt") # vector of lines
sub_blogs <- readLines("sample_blogs.txt")
sub_news_df <- data_frame(line = 1:length(sub_news), text=sub_news)
sub_twitter_df <- data_frame(line = 1:length(sub_twitter), text=sub_twitter)
sub_blogs_df <- data_frame(line = 1:length(sub_blogs), text=sub_blogs)
# Bi-gram
tidy_sub_news_bigrams <- sub_news_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2) 
tidy_sub_twitter_bigrams <- sub_twitter_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2) # 2778083
tidy_sub_blogs_bigrams <- sub_blogs_df %>% unnest_tokens(bigram, text, token = "ngrams", n=2)
# Tri-gram
tidy_sub_news_trigrams <- sub_news_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3) 
tidy_sub_twitter_trigrams <- sub_twitter_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3)
tidy_sub_blogs_trigrams <- sub_blogs_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3)
# a bit of comparison
sorted_tidy_twitter_bigrams <- tidy_twitter_bigrams %>% count(bigram, sort = TRUE)
# # A tibble: 5,312,867 x 2
# bigram         n
# <chr>      <int>
#   1 in the     78250
# 2 for the    73921
# 3 of the     56923
# 4 on the     48432
# 5 to be      47094
# 6 to the     43385
# 7 thanks for 42995
# 8 at the     37162
# 9 i love     35918
# 10 going to   34273
sorted_tidy_sub_twitter_bigrams <- tidy_sub_twitter_bigrams %>% count(bigram, sort = TRUE)
# # A tibble: 948,059 x 2
# bigram         n
# <chr>      <int>
#   1 in the      7787
# 2 for the     7413
# 3 of the      5623
# 4 on the      4950
# 5 to be       4637
# 6 thanks for  4349
# 7 to the      4230
# 8 at the      3831
# 9 i love      3681
# 10 going to    3406
# We have shown that using the subset of twitter text the top 10 bigrams are identical!
# Now, try tri-gram
tidy_twitter_trigrams <- twitter_df %>% unnest_tokens(trigram, text, token = "ngrams", n=3) 
sorted_tidy_twitter_trigrams <- tidy_twitter_trigrams %>% count(trigram, sort = TRUE)
sorted_tidy_sub_twitter_trigrams <- tidy_sub_twitter_trigrams %>% count(trigram, sort = TRUE)
sorted_tidy_twitter_trigrams
# # A tibble: 13,907,982 x 2
# trigram                n
# <chr>              <int>
#   1 NA                 66260
# 2 thanks for the     23619
# 3 looking forward to  8832
# 4 thank you for       8678
# 5 i love you          8419
# 6 for the follow      7929
# 7 going to be         7415
# 8 can't wait to       7344
#  9 i want to           7113
# 10 a lot of            6250
# # ... with 13,907,972 more rows
sorted_tidy_sub_twitter_trigrams
# # A tibble: 1,858,722 x 2
# trigram                n
# <chr>              <int>
#   1 NA                  6594
# 2 thanks for the      2421
# 3 i love you           934
# 4 looking forward to   873
# 5 thank you for        820
# 6 for the follow       809
# 7 going to be          741
# 8 can't wait to        740
# 9 i want to            720
# 10 a lot of             635
# # ... with 1,858,712 more rows
# For trigrams, the top 10 ranking is a bit shuffled, regardless the same set.
# Apparently, I can just use 1/10th of the data and it'd be pretty accurate.






################################# BELOW FOR FUTURE USE ############################
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


