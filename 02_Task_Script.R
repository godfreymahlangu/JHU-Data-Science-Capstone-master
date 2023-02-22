#' ---
#' title: "Task 2: Exploratory Data Analysis"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#' ---
#'
#' ## Introduction
#' This script uses the tidy data principles applied to text mining, as outlined in
#' [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).  
#' 

#+ startup, echo = FALSE 
rm(list = ls())
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})
start_time <- Sys.time()

#' ## Data Loading and Summarizing
#+ DataLoading

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' File Sizes (Mb)
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#' Number of Lines per file
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

#' Distibution of characters per line, by file
blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name") 
title("Comparing Distributions of Chracters per Line")

#' Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

#' Total words per file
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(twitter, sep = " ")

#' Create summary of repo stats
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)

saveRDS(repo_summary, "./clean_repos/repo_summary.rds")

#' Read the data files into dataframes
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

#' ## Data Sampling and Cleaning
#+ DataSampling
set.seed(1001)
sample_pct <- 0.1

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)

#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)


#' Create filters: stopwords, profanity, non-alphanumeric's, url's, repeated letters(+3x)
#+ DataCleaning
data("stop_words")
swear_words <- read_delim("./data/final/en_US/en_US.swearWords.csv", delim = "\n", col_names = FALSE)
swear_words <- unnest_tokens(swear_words, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

#' Clean up
rm(blogs, blogs_nchar, news, news_nchar, twitter, twitter_nchar, replace_reg, replace_url, replace_aaa)
  
#' Create tidy dataframe for repo sample
tidy_repo <- clean_sample %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

#' ## Most frequent words and word distributions

#' Word counts: Number of unique words in repo
(repo_count <- tidy_repo %>%
    summarise(keys = n_distinct(word)))

#' Number of words to attain 50% and 90% coverage of all words in repo
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)

cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(cover_90)

#' ## Word distributions  
#' Word distribution
cover_90 %>%
  top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' Word distribution by source
freq <- tidy_repo %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))

freq %>%
  filter(proportion > 0.002) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_grid(~source, scales = "free")

#' Word cloud
cover_90 %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

saveRDS(tidy_repo, "./clean_repos/tidy_repo.rds")
saveRDS(cover_90, "./clean_repos/cover_90.rds")
rm(tidy_repo, cover_50, cover_90)

#' ## Bigrams  
#' Create bigrams by source using `unnest_tokens`

bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Number of bigrams to attain 90% coverage of all bigrams in repo
bigram_cover_90 <- bigram_repo %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)

#' Bigram distribution
bigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

saveRDS(bigram_cover_90, "./clean_repos/bigram_cover_90.rds")

#' ## Trigrams    
#' Create Trigrams by source using `unnest_tokens`
#+ trigrams

trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Number of trigrams to attain 90% coverage of all trigrams in repo
trigram_cover_90 <- trigram_repo %>%
  count(trigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(trigram_cover_90)

#' trigram distribution
trigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

saveRDS(trigram_cover_90, "./clean_repos/trigram_cover_90.rds")

#' ## Quadgrams  
#' Create quadgrams by source using `unnest_tokens`
#+ quadgrams

quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' Number of quadgrams to attain 90% coverage of all quadgrams in repo
quadgram_cover_90 <- quadgram_repo %>%
  count(quadgram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(quadgram_cover_90)

#' quadgram distribution
quadgram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(quadgram = reorder(quadgram, proportion)) %>%
  ggplot(aes(quadgram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

quadgrams_separated <- quadgram_cover_90 %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quadgrams_separated

saveRDS(quadgram_cover_90, "./clean_repos/quadgram_cover_90.rds")

end <- Sys.time()

(run_time <- end - start_time)

#' -------------
#'  
#' ## Session info
#+ show-sessionInfo
sessionInfo()       
