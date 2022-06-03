'''PROBLEM STATEMENT: To obtain short-term forecasts (2-3 months ahead) for each of the six series and repeat this task every month.

DATASET DESCRIPTION: The Australian Wine Sales data consists of monthly sales of six types of Australian wines (red, rose, sweet white, dry white, sparkling, and fortified) for 1980-1994. The units are thousands of liters. I have considered the sales dataset for fortified wine from the AustralianWines.csv file assigned.
'''
library(forecast)
library(dplyr)

data= read.csv("C:\\Users\\anous\\OneDrive\\Desktop\\Sem 2\\MSA\\Fortified .csv")
View(data)

plot(decompose(data))

selected_data = data[1:180,]
selected_data

#i. Start by partitioning the data using the period until December 1993 as the training period.
training_data = slice(selected_data,1:168)
train = select(training_data,-1)
View(train)

ts_object = ts(train,frequency=12)
Acf(ts_object, main="ACF plot - Training Data")
plot(ts_object, main= "TS Plot of Fortified Wine Data")

#ii. Fit a regression model to sales with a linear trend and seasonality
fit_model= tslm(ts_object~trend + season)
fit_model
forecast(fit_model,h=2)
summary(fit_model)

#iii. Create the "actual vs. forecast" plot. What can you say about model fit?
par(mfrow=c(1,2))
plot(ts_object, main= "TS Plot of Fortified Wine Data")
plot(forecast(fit_model, h=2))

#iV. Use the regression model to forecast sales in January and February 1994.

#iv-(b) Create an ACF and PACF plots for the residuals from the above model until lag-12.
Residuals= fit_model$residuals
par(mfrow=c(1,2))
Acf(Residuals,lag.max = 12, main="ACF Plot for Residuals")
Pacf(Residuals,lag.max = 12, main="PACF Plot for Residuals")

#iv-(c)  Try ARIMA (p,d,q)  and plot the residuals  Is this model better than the above ?
Arima_fit = auto.arima(train)
Arima_fit
Residual = Arima_fit$residual
par(mfrow=c(1,2))
Acf(Residual, lag.max = 12, main="ACF of ARIMA Model")
Pacf(Residual, lag.max = 12, main="ACF of ARIMA Model")
accuracy(forecast(Arima_fit,h=2))


