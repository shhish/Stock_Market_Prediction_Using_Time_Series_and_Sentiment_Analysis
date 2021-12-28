# Stock-Market-Prediction-Using-Time-Series-and-Sentiment-Analysis
This project studies the possibilities of forecasting stock market prices of firms using the sentiments captured via web scrapping. We have experimented with stock market price of Tesla and Moderna using sentiment analysis and ARIMA model. An accuracy analysis was also carried out with a R- sqaured value of each of the model to evaluate how each of them faired in the forecasting. The aim is to help reduce participants in loss while investing using the twitter data. The stock data was pulled of the Yahoo Finance API. The sentiments were obtained off the sentences of tweets from twitter. Results obtained has proved that the ARIMA model has good R-Squared value for short term prediction.


<h3> Working of the Project </h3>
This Project as a file called as "Tweets_Scrapping_Script.py"; Use this file to scrape tweets off Twitter by passing the the keyword and the timeframe.</br>
The tweets scrapped off Twitter is then matched with the corresponding News feed and both of which is matched with the corresponding stock price in that timeframe.</br> 
Used ARIMA for time series analysis. </br>
Files as "prject_Tesla.R" and "project_moderna.R" contains the entire code with importing the dataset to creating a wordcloud and analysing and preidcting the stock for a future event. </br>
