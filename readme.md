

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


## Next Function



```{r, echo=FALSE}

library(tidyverse)
library(tidytext)
library(tm)
library(rtweet)
library(sentimentr)
library(tm)
library(forecast)
library(dplyr)
library(data.table)
library(rtweet)

```
Initilize these packages much like the other function

```{r, echo=FALSE}

moving_sentiment_average <- function(user, n, data=NULL, ticker=NULL, hash = FALSE, ma = 30){
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
    final_bind$date <- as.Date(final_bind$date)
    sent_plot <- ggplot(final_bind, aes(x=date, y=Mean, colour=type, group=type)) +
      geom_line() +
      scale_x_date(breaks = pretty_breaks(10))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste(user, "Twitter Sentiment",ma, "Days Moving Average vs.", ticker, "Adjusted Value", sep = " "))
    
  }
    
  return(sent_plot)
}
```
This function will do much of the same thing as the last one, to a point. Once we get to the extracting words comment we are creating the sentiment value moving average. Essentially we are getting year,month,day to make a time-series and running get_sentences() and sentiment() on the text column to output the value of each tweets sentiment. We then group_by() day and take the mean of each day. Finally, we change it into a ts() class and find the moving average of the sentiment and plot that based on whatever ma value you chose. Pretty neat.

```{r, echo=FALSE}

library(quantmod)
library(mice)
library(forecast)
library(tm)
library(data.table)
library(sentimentr)
library(dplyr)
```



```{r, echo=FALSE}
## Getting stock data for SPY and turning it into a data frame with date
stockdata <- new.env()
getSymbols("SPY", from = "2019-07-22", env = stockdata)
stocks <- stockdata[["SPY"]][,6]
stocks_df <- as.data.frame(stocks)
stocks_df <- cbind(row.names(stocks_df), stocks_df[,1])
Date <- seq.Date(as.Date("2019-07-22"), by = "day", length.out = 79)
Date <- data.frame(Date = Date)
stocks_df <- as.data.frame(stocks_df)
names(stocks_df) <- c("Date", "Stocks")
stocks_df$Date <- as.Date(stocks_df$Date)
m1 <- merge(Date, stocks_df, all = TRUE)


## imputing missing stock data days so I can have full rank data
tempdata <- mice(m1,m=50,maxit=50,meth='pmm',seed=1337)
#tempdata$imp$Stocks

## of those imputations I am randomly picking 1 of the 50 columns and completing the data set
completed_data <- complete(tempdata, sample(1:50,1))

## Getting tweet data, already have it cached so I am just calling it
# tweets <- get_timelines("realDonaldTrump", n=3200, include_rts = FALSE)
tweets = data1

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
final <- cbind.data.frame(value = value$sentiment, Date = lubridate::as_date(tweets$date))

## summerizing the sentiment value of the tweets by day and gettingthe mean
bin_final <- final %>%
  group_by(Date) %>%
  summarize(Mean = mean(value, na.rm=TRUE))

## merging tweets with stock data
m2 <- left_join(bin_final, completed_data)

## omitting missing rows as I call close value so it is possible that the markets havent closed yet
m3 <- na.omit(m2)

## getting xregs and ts_data
xreg <- m3 %>%
  select("Mean")
ts_data <- m3 %>%
  select("Stocks")

## making my testing and training sets
ts_data_train <- subset(ts_data[1:76,])
ts_data_test <- subset(ts_data[77:79,])
  
xreg_train <- subset(xreg[1:76,])
xreg_test <- subset(xreg[77:79,])

ts_data_train <- as.numeric(as.character(unlist(ts_data_train)))
ts_data_test <- as.numeric(as.character(unlist(ts_data_test)))

xreg_train <- as.numeric(as.character(unlist(xreg_train)))
xreg_test <- as.numeric(as.character(unlist(xreg_test)))



## looking at some ACF and PACF
Acf(ts_data_train)
Acf(xreg_train)

Pacf(ts_data_train)
Pacf(xreg_train)

  #arima modelling
ARIMAX <- auto.arima(ts_data_train,
                     stepwise = FALSE, parallel = TRUE, xreg = xreg_train, biasadj = TRUE, ic ="aicc")
ARIMAXf <- fitted.values(ARIMAX)
ARIMAXF <- forecast(ARIMAXf,
                   xreg = xreg_test)
  #arima modelling
ARIMA <- auto.arima(ts_data_train,
                     stepwise = FALSE, parallel = TRUE, biasadj = TRUE, ic ="aicc")
ARIMAf <- fitted.values(ARIMA)
ARIMAF <- forecast(ARIMAf,
                    h=3)

## plottting
autoplot(ARIMAF)

```
This next code chunk uses the idea that text sentiment can predict the stock market with someone as powerful as POTUS. I know my financial econometrics professor would be chastising me right now for think we can predict a stochastic process. It is more of a proof of concept that if the president is being very negative the traders will see that and react accordingly. A lot of assumptions are being made for that to work, nonetheless it is intresting to see it in action.

