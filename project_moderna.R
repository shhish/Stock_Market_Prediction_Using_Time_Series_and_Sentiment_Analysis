########################################### Installing Packages ########################################### 
#install.packages("pacman")
library(pacman)
pacman::p_load(wordcloud2, ggplot2, reshape2, dplyr, tidyverse, lubridate, tidytext,
               xts, tseries, vars, MTS, lattice, grid, TSstudio) 
########################################### Importing the Data ############################################
tweet_data1 <- read.csv("moderna1.csv", header = TRUE)
tweet_data2 <- read.csv("moderna2.csv", header = TRUE)
price_data <- read.csv("MRNA.csv", header = TRUE)
comparison_data <- read.csv("MRNA_comparison.csv", header = TRUE)
tweet_data <- rbind(tweet_data1, tweet_data2)
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

### Stock Data ###
price_data = mutate(price_data, Returns = Adj.Close / lag(Adj.Close)-1)
price_data = price_data[-1,]
price_data$Date = ymd(price_data$Date)-1
rownames(price_data) <- NULL
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
  filter(!word %in% c('replying', 'moderna', 'vaccine', 'vaccines', 'people', 'pfizer', 'andothers',
                      'dose', 'modernas', 'shot', 'coronavirus', 'doses', 'vaccinated', 'covid',
                      'mrna', 'im', 'dont', 'youre', 'theyre', 'week', 'trial', 'canada', 'month',
                      'receive', 'workers', 'months', 'canada', 'americans', 'received', 'health',
                      'day', 'gif', 'cdc', 'jj', 'shot', 'fda', 'study', 'shots', 'vaccination',
                      'uk', 'results', 'data', 'astrazeneca', 'virus', 'ive', 'time', 'world',
                      'trials', 'news', 'pandemic', 'supply', 'line', 'efficacy', 'effects',
                      'world', 'booster', 'stop', 'weeks', 'told', 'st', 'az', 'research', 'dolly',
                      'ferrari', 'thread', 'dr', 'ceo', 'rna', 'care', 'pfizermoderna', 'rare',
                      'hes', 'feb', 'read', 'due', 'wont', 'pm', 'public', 'share', 'clinical', 
                      'vax', 'dec', 'variants', 'feb', 'science', 'jab', 'company', 'clinic',
                      'companies', 'didnt', 'government', 'trump', 'pharma', 'eu', 'call',
                      'government', 'arm', 'story', 'ago', 'children', 'isnt', 'announced',
                      'tomorrow', 'jan', 'jun', 'kids', 'covidvaccine', 'lives', 'apr',
                      'doesnt','lives', 'variant', 'coming', 'info', 'scientists', 'emergency',
                      'pfizerbiontech', 'india', 'jul', 'live', 'update', 'countries', 'israel',
                      'heart', 'bit', 'johnsonjohnson', 'parton', 'fauci', 'delta', 'body', 'south',
                      'hiv', 'team', 'join', 'total', 'global', 'heres', 'site', 'medical', 'guess',
                      'drug', 'id', 'hours', 'appointment', 'country', 'county', 'peoplesvaccine',
                      'money', 'buy', 'speed', 'job', 'july', 'march', 'havent', 'china', 'test',
                      'doctor', 'post', 'testing', 'sarscov', 'start', 'sign', 'key', 'life',
                      'ready', 'mar', 'bc', 'biden', 'yesterday', 'bring', 'eua', 'pfizers', 'blood',
                      'book', 'race', 'based', 'rollout', 'appointments', 'lot', 'biotech', 'system',
                      'receiving', 'days', 'family', 'studies', 'plan', 'reports', 'million', 
                      'found', 'administered', 'antibodies', 'hey', 'mass', 'tech', 'technology',
                      'access', 'half', 'lot', 'folks', 'friends', 'media', 'candidate', 'april',
                      'africa', 'food', 'june', 'staff', 'dad', 'wife', 'reprted', 'email',
                      'production', 'view', 'college', 'monday', 'friday', 'arent', 'music', 'choice',
                      'canadians', 'vhcon', 'correct', 'easier', 'link', 'late', 'gto', 'yall', 'staff',
                      'cells', 'add', 'step', 'john', 'play', 'nov', 'mom', 'guy', 'ramp', 'bcpoli',
                      'times', 'head', 'summer', 'true', 'arrive', 'track', 'europe', 'house', 'weve',
                      'baby', 'ppl', 'de', 'tested', 'heard', 'report', 'morning', 'night', 'game',
                      'millions', 'govt', 'chinese', 'control', 'distribution', 'biontech', 'residents',
                      'means', 'united', 'healthcare', 'including', 'deal', 'president', 'spread'))
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
sentimentr_tweet_data = sentimentr_tweet_data[-1, ]
sentimentr_tweet_data = slice(sentimentr_tweet_data, 1:(n()-1))

########################################### Plotting ###########################################
### Plotting price, twitter_sentiment and news_data
# Preparing data for plotting
plot_data <- subset (sentimentr_tweet_data, select = c(1))
plot_data <- cbind(plot_data,
                   tweet_sentiment = sentimentr_tweet_data$Sentiment,
                   price = price_data$Returns)
rownames(plot_data) <- plot_data$Date
# Converting data into time-series
tweet_sentiment <- ts(plot_data$tweet_sentiment,start=decimal_date(ymd("2020-11-08")), frequency=52)
price <- ts(plot_data$price,start=decimal_date(ymd("2020-11-08")), frequency=52)
VAR_data <- cbind(tweet_sentiment, price)
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
autoplot(ts(stnry, start=decimal_date(ymd("2020-11-08")), frequency=52)) +
  ggtitle("Time Series Plot of the Stationary Sentiment Time-Series")
VARselect(stnry, type = "both", lag.max = 13) # Highest lag order
var.a <- vars::VAR(stnry, lag.max = 13, ic = "FPE", type = "trend")
# Getting adjusted r-squared value
summary(var.a$varresult$price)$adj.r.squared
serial.test(var.a)
# Forecasting the time-series for 10 weeks ahead
fcast = predict(var.a, n.ahead = 10)
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)
tweet_sentiment = fcast$fcst[1]
price = fcast$fcst[2]
x = tweet_sentiment$tweet_sentiment[, 1]
y = price$price[, 1]
tail(VAR_data)
x = cumsum(x) + -(0.050546251)
y = cumsum(y) + 0.094120470
par(mar = c(2.5,2.5,2.5,2.5))
# Plotting price stitched with predicted price
pred_price = ts(c(VAR_data[,2], y), start=decimal_date(ymd("2020-11-08")), frequency=52)
pred_price_datframe <- as.data.frame(pred_price[1:54]) 
colnames(pred_price_datframe) <- c("y")
a = zoo(pred_price[1:54])
xyplot(a, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(45, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })
# Comparing actual share price with predicted share price
comparison_data = mutate(comparison_data, Returns = Adj.Close / lag(Adj.Close)-1)
comparison_data$Date = ymd(comparison_data$Date)-1
comparison_data = comparison_data[-1,]
comparison_data = slice(comparison_data, 1:(n()-2))
row.names(comparison_data) <- NULL
actual_price <- ts(comparison_data$Returns,start=decimal_date(ymd("2020-11-08")), frequency=52)
par(mar = c(2.5,2.5,2.5,2.5))
plot.ts(pred_price)
par(mar = c(2.5,2.5,2.5,2.5))
plot.ts(actual_price)
comparsion_df <- cbind(pred_price, actual_price)
ts_plot(comparsion_df, title = "MRNA Comparison between actual and predicted values",
        Xtitle = "Date",
        Ytitle = "Price")