library(quantmod)
library(lmtest)
library(dplyr)
library(PerformanceAnalytics)
library(prophet)
library(ggplot2)
library(numDeriv)
library(dlm)
library(ggpubr)
library(MLmetrics)
library(rugarch)
library(FinTS)
library(tseries)
library(forecast)
library(tsfknn)
library(tidyverse)
library(quantmod)
library(magrittr)
library(broom)

start = as.Date("2019-05-01") #starting date
end = as.Date("2021-10-28") #end date 


getSymbols(c("GOOG","SCCO","WHR"), from = start, to = end) #Desired stock tickers, source:Yahoo finance 

##Single stock analysis 
stocks = as.xts(data.frame(A = GOOG[, "GOOG.Adjusted"])) #Transform stock into data frame 
plot(stocks) 
chartSeries(GOOG) #Volume and price of stock 


##Manual ARIMA model 
myts <- ts(stocks, frequency = 12) #Transform data into a time series 
ddata <- decompose((myts), "multiplicative") #Decompose time series 
plot(ddata)
myets <- ets(myts, "ZZZ") #Apply ETS on the time series 
plot(myets)
myprediction <- forecast(myets, 12, level = c(75,95)) #First forecast on ETS
lastValue = tail(myprediction$x,1)
myprediction$mean=ts(c(lastValue,myprediction$mean), 
                     frequency = frequency(myprediction$mean), 
                     end=end(myprediction$mean)) ##Determines the mean of the forecast 


myprediction$upper=ts(rbind(c(lastValue,lastValue),myprediction$upper), 
                      frequency = frequency(myprediction$upper), 
                      end=end(myprediction$upper)) ##Determines the upper limit of the forecast 

myprediction$lower=ts(rbind(c(lastValue,lastValue),myprediction$lower), 
                      frequency = frequency(myprediction$lower), 
                      end=end(myprediction$lower)) ##Determines the lower limit of the forecast

plot(myprediction, lwd=2, col="blue", fcol="red", flwd=2, shaded=TRUE, col.sub = "gray50", cex.sub=0.75, main="GOOGL Stock Price Forecast",xlab = "Date", ylab = "Price",xaxt="n") ##Forecast plot

Box.test(resid(myprediction), type="Ljung-Box",lag=3)




##ARIMA by using auto.arima 
function_ets <- function(x, h) {
  forecast(ets(stocks), h = h)
}
function_arima <- function(x, h) {
  forecast(auto.arima(stocks), h=h)
}

arima_limit <- window(stocks, start="2019-5-01")
ts1 <- tsCV(arima_limit, function_ets, h=1) #CV Errors model1
ts2 <- tsCV(arima_limit, function_arima, h=1) #CV Errors model2
mean(ts1^2, na.rm=TRUE) #Auto-arima MSE for one model class
mean(ts2^2, na.rm=TRUE) #Auto-arima MSE for second model class
arima_limit %>% ets() %>% forecast() %>% autoplot()
arima_pred %>% ets() %>% forecast(h=12) %>% autoplot()

arima_pred <- window(stocks, start=start)

##GARCH 

stocks_adj <- cbind(
  Price = stocks$GOOG.Adjusted,
  Return=CalculateReturns(GOOG$GOOG.Close, method = 'log'))  #Log transformation for the return calculation 
colnames(stocks_adj) <- c('Price','Return')
head(stocks_adj)

garch_model <- ugarchspec(
  mean.model = list(armaOrder=c(0,0)),
  variance.model = list(model = 'eGARCH',
  garchOrder = c(1, 1)),distribution = 'std') #Exponential GARCH methodology 

fit_garch <- ugarchfit(spec = garch_model, data= na.omit(stocks_adj$Return))
plot(fit_garch,which='all')



##KNN  
df <- data.frame(ds = index(GOOG),
                 y = as.numeric(GOOG[,'GOOG.Close']))

predknn <- knn_forecasting(df$y, h = 30, lags = 1:30, k = 40, msas = "MIMO") ##KNN prediction 



plot(predknn)
 
##Neural network 
alpha <- 1.5^(-10)
hn <- length(stocks)/(alpha*(length(stocks)+30)) 
lambda <- BoxCox.lambda(stocks) #BoxCox lambda parameter 
neural_pred <- nnetar(stocks, size= hn, lambda = lambda)
#Prediction using neural network function 
neural_forecast <- forecast(neural_pred, h= 30, PI = TRUE) #Neural Network forecast
Box.test(resid(neural_forecast), type="Ljung-Box",lag=3)
plot(neural_forecast)



###Accuracy of the models 
accuracy(myprediction) ##Accuracy of manual ARIMA plot 
accuracy(function_arima) ## Accuracy auto arima 
accuracy(fit_garch) ##GARCH Accuracy 
accuracy(predknn)
accuracy(neural_forecast) #Neural Network accuracy 


#####Analysis using two stocks 

stocks2 = as.xts(data.frame(C = SCCO[,"SCCO.Adjusted"],D=WHR[,"WHR.Adjusted"])) #Comparing between two different tickers 


stocks_series2 = tidy(stocks2) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "Whirpool Stock & Copper Stock")

stocks_series2



ggplot(data=stocks2, aes(x=WHR.Adjusted, y=SCCO.Adjusted)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=40, label.y=235)+
  stat_cor(aes(label=..rr.label..), label.x=40, label.y=228)


model1<-lm(stocks2$WHR.Adjusted~stocks2$SCCO.Adjusted) ##Linear regression analysis 
anova1<-aov(model1)
summary(anova1)
plot(model1)
