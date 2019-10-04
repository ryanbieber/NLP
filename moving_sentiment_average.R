library(sentimentr)
library(tm)
library(forecast)
library(dplyr)
library(data.table)

moving_sentiment_average <- function(user, n, ma = 30){
  tweets <- get_timelines(user = user, n=n, include_rts = FALSE)
  
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
  
  ts <- as.ts(bin_final$Mean)
  
  mamonth <- ma(ts,order = ma)
  
  sent_plot <- print(autoplot(mamonth))
  
  return(sent_plot)
}

sent_plot <- moving_sentiment_average(user, n)

