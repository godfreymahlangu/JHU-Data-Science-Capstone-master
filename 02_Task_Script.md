Task 2: Exploratory Data Analysis
================
Godfrey Mahlangu
2023-02-17

-   [Introduction](#introduction)
-   [Data Loading and Summarizing](#data-loading-and-summarizing)
-   [Data Sampling and Cleaning](#data-sampling-and-cleaning)
-   [Most frequent words and word distributions](#most-frequent-words-and-word-distributions)
-   [Word distributions](#word-distributions)
-   [Bigrams](#bigrams)
-   [Trigrams](#trigrams)
-   [Quadgrams](#quadgrams)
-   [Session info](#session-info)

Introduction
------------

This script uses the tidy data principles applied to text mining, as outlined in [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).

Data Loading and Summarizing
----------------------------

English Repository Files

``` r
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  
```

File Sizes (Mb)

``` r
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)
```

Read the data files

``` r
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)
```

Number of Lines per file

``` r
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines
```

Distibution of characters per line, by file

``` r
blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name") 
title("Comparing Distributions of Chracters per Line")
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Total characters per file

``` r
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)
```

Total words per file

``` r
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(twitter, sep = " ")
```

Create summary of repo stats

``` r
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)
```

| f\_names |   f\_size|  f\_lines|    n\_char|  n\_words|  pct\_n\_char|  pct\_lines|  pct\_words|
|:---------|---------:|---------:|----------:|---------:|-------------:|-----------:|-----------:|
| blogs    |  200.4242|    899288|  208361438|  37334131|          0.36|        0.21|        0.37|
| news     |  196.2775|   1010242|  203791400|  34372528|          0.35|        0.24|        0.34|
| twitter  |  159.3641|   2360148|  162385035|  30373583|          0.28|        0.55|        0.30|

``` r
saveRDS(repo_summary, "./clean_repos/repo_summary.rds")
```

Read the data files into dataframes

``` r
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)
```

Data Sampling and Cleaning
--------------------------

``` r
set.seed(1001)
sample_pct <- 0.1

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)
```

Create tidy repository

``` r
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)
```

Create filters: stopwords, profanity, non-alphanumeric's, url's, repeated letters(+3x)

``` r
data("stop_words")
swear_words <- read_delim("./data/final/en_US/en_US.swearWords.csv", delim = "\n", col_names = FALSE)
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character()
    ## )

``` r
swear_words <- unnest_tokens(swear_words, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  
```

Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words, and ngrams.

``` r
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))
```

Clean up

``` r
rm(blogs, blogs_nchar, news, news_nchar, twitter, twitter_nchar, replace_reg, replace_url, replace_aaa)
```

Create tidy dataframe for repo sample

``` r
tidy_repo <- clean_sample %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

Most frequent words and word distributions
------------------------------------------

Word counts: Number of unique words in repo

``` r
(repo_count <- tidy_repo %>%
    summarise(keys = n_distinct(word)))
```

    ## # A tibble: 1 x 1
    ##     keys
    ##    <int>
    ## 1 164925

Number of words to attain 50% and 90% coverage of all words in repo

``` r
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)
```

    ## [1] 1317

``` r
cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(cover_90)
```

    ## [1] 18097

Word distributions
------------------

Word distribution

``` r
cover_90 %>%
  top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

Word distribution by source

``` r
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
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

Word cloud

``` r
cover_90 %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
saveRDS(tidy_repo, "./clean_repos/tidy_repo.rds")
saveRDS(cover_90, "./clean_repos/cover_90.rds")
rm(tidy_repo, cover_50, cover_90)
```

Bigrams
-------

Create bigrams by source using `unnest_tokens`

``` r
bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
```

Number of bigrams to attain 90% coverage of all bigrams in repo

``` r
bigram_cover_90 <- bigram_repo %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)
```

    ## [1] 1428897

Bigram distribution

``` r
bigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-21-1.png)

``` r
saveRDS(bigram_cover_90, "./clean_repos/bigram_cover_90.rds")
```

Trigrams
--------

Create Trigrams by source using `unnest_tokens`

``` r
trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)
```

Number of trigrams to attain 90% coverage of all trigrams in repo

``` r
trigram_cover_90 <- trigram_repo %>%
  count(trigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(trigram_cover_90)
```

    ## [1] 5178400

trigram distribution

``` r
trigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-23-1.png)

``` r
saveRDS(trigram_cover_90, "./clean_repos/trigram_cover_90.rds")
```

Quadgrams
---------

Create quadgrams by source using `unnest_tokens`

``` r
quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)
```

Number of quadgrams to attain 90% coverage of all quadgrams in repo

``` r
quadgram_cover_90 <- quadgram_repo %>%
  count(quadgram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(quadgram_cover_90)
```

    ## [1] 7339529

quadgram distribution

``` r
quadgram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(quadgram = reorder(quadgram, proportion)) %>%
  ggplot(aes(quadgram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-25-1.png)

``` r
quadgrams_separated <- quadgram_cover_90 %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quadgrams_separated
```

    ## # A tibble: 7,339,529 x 7
    ##    word1 word2 word3 word4     n   proportion     coverage
    ##  * <chr> <chr> <chr> <chr> <int>        <dbl>        <dbl>
    ##  1   the   end    of   the   806 8.926682e-05 8.926682e-05
    ##  2    at   the   end    of   656 7.265389e-05 1.619207e-04
    ##  3   the  rest    of   the   651 7.210012e-05 2.340208e-04
    ##  4   for   the first  time   613 6.789151e-05 3.019123e-04
    ##  5    at   the  same  time   506 5.604095e-05 3.579533e-04
    ##  6    is going    to    be   482 5.338289e-05 4.113362e-04
    ##  7    is   one    of   the   446 4.939578e-05 4.607320e-04
    ##  8   one    of   the  most   435 4.817750e-05 5.089095e-04
    ##  9  when    it comes    to   419 4.640545e-05 5.553149e-04
    ## 10 going    to    be     a   376 4.164308e-05 5.969580e-04
    ## # ... with 7,339,519 more rows

``` r
saveRDS(quadgram_cover_90, "./clean_repos/quadgram_cover_90.rds")

end <- Sys.time()

(run_time <- end - start_time)
```

    ## Time difference of 15.94087 mins

------------------------------------------------------------------------

Session info
------------

``` r
sessionInfo()       
```

    ## R version 3.4.2 (2017-09-28)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 15063)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2       ngram_3.0.3        wordcloud_2.5     
    ##  [4] RColorBrewer_1.1-2 knitr_1.17         stringr_1.2.0     
    ##  [7] dplyr_0.7.4        purrr_0.2.3        readr_1.1.1       
    ## [10] tidyr_0.7.1        tibble_1.3.4       ggplot2_2.2.1     
    ## [13] tidyverse_1.1.1    tidytext_0.1.4    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.2  slam_0.1-40       reshape2_1.4.2   
    ##  [4] haven_1.1.0       lattice_0.20-35   colorspace_1.3-2 
    ##  [7] htmltools_0.3.6   SnowballC_0.5.1   yaml_2.1.14      
    ## [10] rlang_0.1.2       foreign_0.8-69    glue_1.1.1       
    ## [13] modelr_0.1.1      readxl_1.0.0      bindr_0.1        
    ## [16] plyr_1.8.4        munsell_0.4.3     gtable_0.2.0     
    ## [19] cellranger_1.1.0  rvest_0.3.2       psych_1.7.8      
    ## [22] evaluate_0.10.1   labeling_0.3      forcats_0.2.0    
    ## [25] parallel_3.4.2    highr_0.6         broom_0.4.2      
    ## [28] tokenizers_0.1.4  Rcpp_0.12.13      backports_1.1.1  
    ## [31] scales_0.5.0      jsonlite_1.5      mnormt_1.5-5     
    ## [34] hms_0.3           digest_0.6.12     stringi_1.1.5    
    ## [37] grid_3.4.2        rprojroot_1.2     tools_3.4.2      
    ## [40] magrittr_1.5      lazyeval_0.2.0    janeaustenr_0.1.5
    ## [43] pkgconfig_2.0.1   Matrix_1.2-11     xml2_1.1.1       
    ## [46] lubridate_1.6.0   assertthat_0.2.0  rmarkdown_1.6    
    ## [49] httr_1.3.1        R6_2.2.2          nlme_3.1-131     
    ## [52] compiler_3.4.2
