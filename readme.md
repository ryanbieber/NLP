

## Start

To start you need to initizlize these packages in R


```{r, echo=FALSE}

library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(ggwordcloud)
library(rtweet)

```

## using twitters api

If you haven't already, sign up for devaccess on twitter, you will need a real phone number and a breif explanation to what you want to use it for.
From here on out I am assuming you have completed that portion of the readme and have set up your token access, etc. to work with the rtweet package.



```{r , echo=FALSE}
  tweets <- get_timelines(user = user, n=n, include_rts = FALSE)
  
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
```
This First chunk pulls the tweets from twitter, the gsub lines remove the pesky http/s and the @ people from the text. The docs bin is a corpus of text from the tm() package with the words cleaned up a bit.
```{r}
  
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
  tst = list()
  for (q in 1:length(docs_unlist)){
    tst[[q]] <- correct(docs_unlist[q])
    
  }
```
This entire chunk is trying to fix some errors in spelling, I first extract the cleaned words and unlist them into a vector, then I use a spell checker that norvig created called correct(ported to R), I then iterate through the list to try and fix the words the best I can. FYI norvig wrote the spell checker for Google.
```{r}
  
  #making the text a dataframe
  text <- data.frame(word = unlist(tst))
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
```
This chunck is merging a sentiment analysis and freq or words together to make a more meaningful wordcloud, also ggwordcloud needs it in a data frame.
```{r}
  ## making the wordcloud
  set.seed(1337)
  wc <- ggplot(
    m4,
    aes(
      label = word, size = Freq, colour = as.factor(m4$sentiment)
    )
  ) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 16) +
    theme_minimal()
  
```
The last chunk is making the wordcloud with the colour being determined by the sentiment of the words.


```{r}
## function to make the wordcloud
get_wc_twitter <- function(user, n){
  tweets <- get_timelines(user = user, n=n, include_rts = FALSE)
  
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
  tst = list()
  for (q in 1:length(docs_unlist)){
    tst[[q]] <- correct(docs_unlist[q])
    
  }
  
  #making the text a dataframe
  text <- data.frame(word = unlist(tst))
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
  set.seed(1337)
  wc <- ggplot(
    m4,
    aes(
      label = word, size = Freq, colour = as.factor(m4$sentiment)
    )
  ) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 16) +
    theme_minimal()
  
  return(wc)
}
```

Here it is in a nice tidy function in which all you have to do is feed it a name and the amount of tweets back you want to go(3200 is the max) and it will spit out a wordcloud for you.
