# find a wordcloud and do a sentiment analysis on anyones twitter
# this is assuming you set up the rtweet access tokens needed to pull from twitters api
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(ggwordcloud)
library(rtweet)

data <- get_timeline("realDonaldTrump", n=3200)

## function to make the wordcloud
get_wc_twitter <- function(user, n, data = NULL, hash = FALSE){
  if (!is.null(data)){
    tweets = data
  } else if (hash==FALSE){
    tweets <- get_timelines(user = user, n=n, include_rts = FALSE)
  } else {
    q=user
    tweets <- search_tweets(q , n, lang="en")
  }

  
  tweets$cleaned <- gsub("http.*","",  tweets$text)
  tweets$cleaned <- gsub("https.*","", tweets$cleaned)
  tweets$cleaned <- gsub("@\\w+ *", "", tweets$cleaned)
  
  #making a corpus to remove and change the data
  docs <- Corpus(VectorSource(tweets$cleaned)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  #tm_map(stemDocument)
  
  #extract the words
  docs_test <- strsplit(tolower(docs[["content"]][["content"]]), "[^a-z]+")
  docs_unlist <- unlist(docs_test)
  
  ## making corrections
  raw_text <- paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")
  split_text <- strsplit(tolower(raw_text), "[^a-z]+")
  word_count <- table(split_text)
  sorted_words <- names(sort(word_count, decreasing = TRUE))
  
  ## tryng to fix some autocorrects
  correct <- function(word) {
    edit_dist <- adist(word, sorted_words)
    min_edit_dist <- min(edit_dist, 2)
    proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
    proposals_by_prob <- c(proposals_by_prob, word)
    proposals_by_prob[1]
  }
  
  #try to do some word corrections
  tst <- lapply(docs_unlist, correct)
  
  #making the text a dataframe
  text <- data.frame(word = unlist(tst))
  stop_words <- data.frame(word = stopwords())
  text <-  anti_join(text, stop_words)
  
  #fifnding the text count for each word and removing duplicates
  text_count <- data.frame(sort(table(word = text), decreasing=T))
  m1 <- merge(text_count, text, by.y = "word")
  m2 <- m1 %>% distinct()
  
  ## finding poisitve, negative, neutral words
  sentiments <- get_sentiments("bing")
  m2$word <- as.character(m2$word)
  m3 <- left_join(m2,sentiments)
  m4 <- subset(m3,!is.na(m3$sentiment))
  
  ## making the wordcloud
  m4 <- m4 %>%
    mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
  set.seed(1337)
  wc <- ggplot(
    m4,
    aes(
      label = word, size = Freq, colour = as.factor(m4$sentiment), angle = angle
    )
  ) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 16) +
    theme_minimal()
  
  return(wc)
}

get_wc_twitter("realDonaldTrump", 201, data = data)


