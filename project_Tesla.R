########################################### Installing Packages ########################################### 
#install.packages("pacman")
library(pacman)
pacman::p_load(wordcloud2, ggplot2, reshape2, dplyr, tidyverse, lubridate, tidytext,
               xts, tseries, vars, MTS, lattice, grid, TSstudio) 
########################################### Importing the Data ############################################
tweet_data <- read.csv("tesla.csv", header = TRUE)
price_data <- read.csv("TSLA.csv", header = TRUE)
news_data <- read.csv("tesla_news.csv", header = TRUE)
comparison_data <- read.csv("TSLA_comparison.csv", header = TRUE)
########################################### Pre-Processing Data ###########################################
# Date Format Conversion Function
timestamp_to_date <- function(timestamp) {
  return (as.Date(as.POSIXct(timestamp, origin="1970-01-01", tz="America/New_York")))
}
#### Tweet Data ###
# Sub-setting Data
tweet_data = subset(tweet_data, select = c('Timestamp', 'Embedded_text'))
# Cleaning Data
tweet_data$stripped_text = gsub("&amp", "", tweet_data$Embedded_text)
tweet_data$stripped_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("@\\w+", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("[[:punct:]]", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("[[:digit:]]", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("http\\w+", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("[ \t]{2,}", "", tweet_data$stripped_text)
tweet_data$stripped_text = gsub("^\\s+|\\s+$", "", tweet_data$stripped_text)
tweet_data <- subset (tweet_data, select = c('Timestamp', 'stripped_text'))
tweet_data$stripped_text = tolower(tweet_data$stripped_text)
# Converting Date format
tweet_data$Timestamp = timestamp_to_date(tweet_data$Timestamp)
#### News Data ###
news_data = subset(news_data, select = c('Timestamp', 'Embedded_text'))
# Cleaning Data
news_data$stripped_text = gsub("&amp", "", news_data$Embedded_text)
news_data$stripped_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", news_data$stripped_text)
news_data$stripped_text = gsub("@\\w+", "", news_data$stripped_text)
news_data$stripped_text = gsub("[[:punct:]]", "", news_data$stripped_text)
news_data$stripped_text = gsub("[[:digit:]]", "", news_data$stripped_text)
news_data$stripped_text = gsub("http\\w+", "", news_data$stripped_text)
news_data$stripped_text = gsub("[ \t]{2,}", "", news_data$stripped_text)
news_data$stripped_text = gsub("^\\s+|\\s+$", "", news_data$stripped_text)
news_data <- subset (news_data, select = c('Timestamp', 'stripped_text'))
news_data$stripped_text = tolower(news_data$stripped_text)
# Converting Date format
news_data$Timestamp = timestamp_to_date(news_data$Timestamp)
### Stock Data ###
price_data = mutate(price_data, Returns = Adj.Close / lag(Adj.Close)-1)
price_data$Date = ymd(price_data$Date)-1
price_data = price_data[-1,]

########################################### Visualization - Word Cloud ########################################### 
#### Tweet Data ###
tweet_Table <- tweet_data %>% 
  unnest_tokens(word, stripped_text)
data(stop_words)
tweet_Table <- tweet_Table %>%
  anti_join(stop_words)
tweet_Table <- tweet_Table %>%
  count(word, sort = TRUE) 
tweet_Table <-tweet_Table %>%
  filter(!word %in% c('replying', 'tesla', 'andothers', 'model', 'tsla', 'car', 'cars',
                      'company', 'dont', 'teslas', 'im', 'didnt', 'musk', 'elon', 'gif',
                      'time', 'day', 'battery', 'stock', 'drive', 'people', 'ive', 'doesnt',
                      'cybertruck', 'texas', 'aug', 'jul', 'range', 'hey', 'nikola',
                      'hes', 'thread', 'solar', 'vehicle', 'electric', 'ev', 'lol',
                      'driving', 'autopilot', 'world', 'guy', 'month', 'fsd', 'app',
                      'energy', 'video', 'money', 'jun', 'service', 'tesmaniancom',
                      'companies', 'youtubecom', 'supercharger', 'delivery',
                      'factory', 'youre', 'price', 'truck', 'berlin', 'california',
                      'china', 'austin', 'ill', 'wait', 'call', 'jul', 'vehicles', 'update', 'share',
                      'home', 'yeah', 'giga', 'ceo', 'told', 'guys', 'evs', 'road', 'tech',
                      'week', 'tslaq', 'theyre', 'lot', 'twitter', 'answer'))

wordcloud2(tweet_Table, size=0.7, color='random-light', backgroundColor="black")

########################################### Sentiment Analysis ########################################### 
### Sentiment Analysis on Tweet Data ###
sentimentr_tweet_data  = tweet_data %>% 
  unnest %>% 
  sentimentr::get_sentences() %>% 
  sentimentr::sentiment()
sentimentr_tweet_data$Timestamp = as.Date(sentimentr_tweet_data$Timestamp,format='%m/%d/%y')
# Order By Week
t1= xts(sentimentr_tweet_data$sentiment,order.by=sentimentr_tweet_data$Timestamp)
t2 = xts::apply.weekly(t1, mean)
sentimentr_tweet_data = data.frame(Date=index(t2), coredata(t2))
colnames(sentimentr_tweet_data)[2] <- "Sentiment"
sentimentr_tweet_data = slice(sentimentr_tweet_data, 1:(n()-1))

### Sentiment Analysis on News Data ###
sentimentr_news_data  = news_data %>% 
  unnest %>% 
  sentimentr::get_sentences() %>% 
  sentimentr::sentiment()
sentimentr_news_data$Timestamp = as.Date(sentimentr_news_data$Timestamp,format='%m/%d/%y')
# Order By Week
n1= xts(sentimentr_news_data$sentiment,order.by=sentimentr_news_data$Timestamp)
n2 = xts::apply.weekly(n1, mean)
sentimentr_news_data = data.frame(Date=index(n2), coredata(n2))
colnames(sentimentr_news_data)[2] <- "Sentiment"
sentimentr_news_data = slice(sentimentr_news_data, 1:(n()-1))


########################################### Plotting ###########################################
### Plotting price, twitter_sentiment and news_data
# Preparing data for plotting
plot_data <- subset (sentimentr_news_data, select = c(1))
plot_data <- cbind(plot_data, news_sentiment = sentimentr_news_data$Sentiment,
                   tweet_sentiment = sentimentr_tweet_data$Sentiment,
                   price = price_data$Returns)
rownames(plot_data) <- plot_data$Date
# Converting data into time-series
news_sentiment <- ts(plot_data$news_sentiment,start=decimal_date(ymd("2020-06-07")), frequency=52)
tweet_sentiment <- ts(plot_data$tweet_sentiment,start=decimal_date(ymd("2020-06-07")), frequency=52)
price <- ts(plot_data$price,start=decimal_date(ymd("2020-06-07")), frequency=52)
VAR_data <- cbind(news_sentiment, tweet_sentiment, price)
# Plotting time-series data
forecast::autoplot(VAR_data) +
  ggtitle("Time Series Plot of the Sentiment Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))

########################################### Vector Auto-regressions VAR ########################################### 
# Choosing best lag for our data
apply(VAR_data, 2, adf.test)
stnry = diffM(VAR_data) 
apply(stnry, 2, adf.test)
plot.ts(stnry)
autoplot(ts(stnry, start=decimal_date(ymd("2020-06-07")), frequency=52)) +
  ggtitle("Time Series Plot of the Stationary Sentiment Time-Series")
VARselect(stnry, type = "none", lag.max = 6) # Highest lag order
var.a <- vars::VAR(stnry, lag.max = 6, ic = "AIC", type = "none")
# Getting adjusted r-squared value
summary(var.a$varresult$price)$adj.r.squared
serial.test(var.a)
# Forecasting the time-series for 8 weeks ahead
fcast = predict(var.a, n.ahead = 8)
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)
news_sentiment = fcast$fcst[1];
tweet_sentiment = fcast$fcst[2];
price = fcast$fcst[3];
x = news_sentiment$news_sentiment[, 1]
y = tweet_sentiment$tweet_sentiment[, 1]
z = price$price[, 1]
tail(VAR_data)
x = cumsum(x) + 0.17856389
y = cumsum(y) + (-0.003998803)
z = cumsum(z) + 0.04988139
par(mar = c(2.5,2.5,2.5,2.5))
# Plotting price stitched with predicted price
pred_price = ts(c(VAR_data[,3], z), start=decimal_date(ymd("2020-06-07")), frequency=52)
pred_price_datframe <- as.data.frame(pred_price[1:38]) 
colnames(pred_price_datframe) <- c("z")
a = zoo(pred_price[1:38])
xyplot(a, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(31, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })
# Comparing actual share price with predicted share price
comparison_data = mutate(comparison_data, Returns = Adj.Close / lag(Adj.Close)-1)
comparison_data$Date = ymd(comparison_data$Date)-1
comparison_data = comparison_data[-1,]
row.names(comparison_data) <- NULL
actual_price <- ts(comparison_data$Returns,start=decimal_date(ymd("2020-06-07")), frequency=52)
par(mar = c(2.5,2.5,2.5,2.5))
plot.ts(pred_price)
par(mar = c(2.5,2.5,2.5,2.5))
plot.ts(actual_price)
comparsion_df <- cbind(pred_price, actual_price)
ts_plot(comparsion_df, title = "TSLA Comparison between actual and predicted values",
        Xtitle = "Date",
        Ytitle = "Price")