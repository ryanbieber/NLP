library(quantmod)
library(mice)
library(forecast)
library(tm)
library(data.table)
library(sentimentr)
library(dplyr)

tweet_predicting_stocks <- function(Stock, Start){
  ## Getting stock data for SPY and turning it into a data frame with date
  stockdata <- new.env()
  getSymbols(Stock, from = Start, env = stockdata)
  stocks <- stockdata[[Stock]][,6]
  stocks_df <- as.data.frame(stocks)
  stocks_df <- cbind(row.names(stocks_df), stocks_df[,1])
  Date <- seq.Date(as.Date(Start), by = "day", length.out = 79)
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
  
  return(m3)
}

m3 <- tweet_predicting_stocks("SPY", "2019-08-09")
## getting xregs and ts_data
xreg <- m3 %>%
  select("Mean")
ts_data <- m3 %>%
  select("Stocks")

#length of predicting period
h=3

## making my testing and training sets
ts_data_train <- subset(ts_data[1:(NROW(m3)-h),])
ts_data_test <- subset(ts_data[(NROW(m3)-h+1):NROW(m3),])
  
xreg_train <- subset(xreg[1:(NROW(m3)-h),])
xreg_test <- subset(xreg[(NROW(m3)-h+1):NROW(m3),])

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
