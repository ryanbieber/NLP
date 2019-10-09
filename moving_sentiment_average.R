library(sentimentr)
library(tm)
library(forecast)
library(dplyr)
library(data.table)
library(rtweet)
library(quantmod)
library(ggplot2)

moving_sentiment_average <- function(user, n, ticker=NULL, hash = FALSE, ma = 30){
  if (hash==FALSE){
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
    tm_map(removeWords, stopwords("english")) 
  
  
  #extract the words
  
  tweets$year <- substr(tweets$created_at,1,4)
  tweets$month <- substr(tweets$created_at,6,7)
  tweets$day <- substr(tweets$created_at,9,10)
  tweets$date <- as.Date(with(tweets, paste(year, month, day,sep="-")), "%Y-%m-%d")
  test <- lapply(docs, as.data.frame)
  test1 <- rbindlist(test)
  word <- as.character(test1$`X[[i]]`)
  sent <- get_sentences(word)
  value <- sentiment(sent)
  final <- cbind.data.frame(value = value$sentiment, date = lubridate::as_date(tweets$date))
  
  bin_final <- final %>%
    group_by(date) %>%
    summarize(Mean = mean(value, na.rm=TRUE))
  bin_final$type <- "Tweets"
  year <- tweets$year[nrow(tweets)]
  month <- tweets$month[nrow(tweets)]
  day <- tweets$day[nrow(tweets)]
  
  mamonth <- forecast::ma(bin_final$Mean,order = ma)
  bind_final <- data.frame(date = bin_final$date, Mean = as.numeric(mamonth), type = "Tweet Moving Average")
  bind_final$type <- as.character(bind_final$type)
  bin_final <- as.data.frame(bin_final)
  bin_final <- bin_final %>% mutate_each_(funs(scale(.) %>% as.vector), vars = c("Mean"))
  bind_final <- bind_final %>% mutate_each_(funs(scale(.) %>% as.vector), vars = c("Mean"))
  
  
  tweet_averge <- gtools::smartbind(bin_final, bind_final)
  
  if (is.null(ticker)){
    sent_plot <- ggplot(tweet_averge, aes(x=date, y=Mean, colour=type, group=type)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Twitter sentiment value")
  } else {
    StockData <- new.env()
    getSymbols(ticker, from = as.character(paste(year, month, day,sep="-")), adjust =  TRUE, env = StockData)
    company <- StockData[[ticker]][,6]
    company <- as.data.frame(company)
    company <- data.frame(date = rownames(company),Mean = company[,1], type = ticker)
    company <- company %>% mutate_each_(funs(scale(.) %>% as.vector), vars = c("Mean"))
    
    
    final_bind <- gtools::smartbind(tweet_averge, company)
    sent_plot <- ggplot(final_bind, aes(x=date, y=Mean, colour=type, group=type)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Twitter sentiment value vs Stock Market")
    
  }
    
  return(sent_plot)
}



