library(widyr)
library(rtweet)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(stopwords)
library(tm)

## tweets are stored in a global variable from other problems in data


text_clouds <- function(data){
  tweets <- data
  ## removing bad headers
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
  
  #extract the words
  docs_test <- strsplit(tolower(docs[["content"]][["content"]]), "[^a-z]+")
  
  ## making a unique identifer for each tweet
  for (i in seq_along(docs_test)){
    docs_test[[i]]$unique_id <- i
  }
  
  ## applying array to list
  docs_df <- lapply(docs_test, as.array)
  
  unlist_list <- function(x){
    ## unique id for number of words in tweet
    full_rank <- lst()
    for (i in 1:length(docs_test)){
      unid <- t(t(rep(docs_test[[i]][[length(docs_test[[i]])]], NROW(docs_test[[i]])-1)))
      word <- unlist(docs_test[[i]])
      word <- word[c(1:NROW(word)-1)]
      word <- t(t(word))
      uni_words <- cbind(unid, word)
      full_rank[[i]] <- uni_words
    }
    
    return(full_rank)
  }
  
  ## making a df with words that contain unique id for each tweet
  incred <- unlist_list(docs_df)
  docs_dataframe <- lapply(incred, as.data.frame)
  long <- rbindlist(docs_dataframe)
  
  
  ## changing column to char and cleaning some tweets
  long$V2 <- as.character(long$V2)
  colnames(long) <- c("uni_id", "word")
  long1 <- subset(long, long$word != "")
  long2 <- subset(long1, long1$word != "t")
  long3 <- subset(long2, long2$word != "s")
  
  ## anti joining stopwords
  stop_words <- data.frame(word = stopwords())
  long4 <-  anti_join(long3, stop_words)
  
  return(long4)
}

long4 <- text_clouds(data)


## pairwise counting words and changing them back to factor to be used in plot
tweet_pairs <- long4 %>%
  pairwise_count(word,uni_id,sort=TRUE,upper=FALSE)

## word n plot
tweet_pairs %>% filter(n >5) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(color = n, width = n)) + geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

## word correlation
word_phi <- long4 %>% 
  group_by(word) %>% 
  filter(n()>5) %>% 
  pairwise_cor(word, uni_id, sort = TRUE)

## word correlation plot
word_phi %>% filter(correlation > .01) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(color = correlation , width = correlation)) + geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, colour = "red", check_overlap = FALSE)


