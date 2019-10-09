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
  mamonth <- ma(bin_final$Mean,order = ma)
  bind_final <- data.frame(date = bin_final$date, Mean = as.numeric(mamonth), type = "Tweet Moving Average")
  bind_final$type <- as.character(bind_final$type)
  bind_final <- bind_final %>% mutate_each_(funs(scale(.) %>% as.vector), vars = c("Mean"))
  
  
  if (is.null(ticker)){
    sent_plot <- ggplot(bind_final, aes(x=date, y=Mean, colour=type, group=type)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Twitter sentiment value")
  } else {
    StockData <- new.env()
    getSymbols(ticker, from = min(tweets$date), adjust =  TRUE, env = StockData)
    company <- StockData[[ticker]][,6]
    company <- as.data.frame(company)
    company <- data.frame(date = rownames(company),Mean = as.numeric(company[,1]), type = ticker)
    company <- company %>% mutate_each_(funs(scale(.) %>% as.vector), vars = c("Mean"))
    
    
    final_bind <- gtools::smartbind(bind_final, company)
    sent_plot <- ggplot(final_bind, aes(x=date, y=Mean, colour=type, group=type)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Twitter sentiment value vs Stock Market")
    
  }
    
  return(sent_plot)
}

user = "officialmcafee"
n = 3200
ticker = "BTC_USD"
ma = 2

test <- moving_sentiment_average(user, n, ticker, ma=ma)


