library(quantmod)
library(forecast)
library(ggplot2)

start <- as.Date("2019-05-01")  # Starting date
end <- as.Date("2021-10-28")  # End date

getSymbols(c("GOOG", "SCCO", "WHR"), from = start, to = end)  # Desired stock tickers, source: Yahoo finance

## Single stock analysis 
stocks <- as.xts(data.frame(GOOG = GOOG[, "GOOG.Adjusted"]))  # Transform stock into data frame 

## Manual ARIMA model 
myts <- ts(stocks, frequency = 12)  # Transform data into a time series 
myets <- ets(myts, "ZZZ")  # Apply ETS on the time series 
myprediction <- forecast(myets, 12, level = c(75, 95))  # First forecast on ETS
lastValue <- tail(myprediction$x, 1)
myprediction$mean <- ts(c(lastValue, myprediction$mean), 
                        frequency = frequency(myprediction$mean), 
                        end = end(myprediction$mean))  # Determine the mean of the forecast 
myprediction$upper <- ts(rbind(c(lastValue, lastValue), myprediction$upper), 
                         frequency = frequency(myprediction$upper), 
                         end = end(myprediction$upper))  # Determine the upper limit of the forecast 
myprediction$lower <- ts(rbind(c(lastValue, lastValue), myprediction$lower), 
                         frequency = frequency(myprediction$lower), 
                         end = end(myprediction$lower))  # Determine the lower limit of the forecast

plot(myprediction, lwd = 2, col = "blue", fcol = "red", flwd = 2, shaded = TRUE, 
     col.sub = "gray50", cex.sub = 0.75, main = "GOOGL Stock Price Forecast", 
     xlab = "Date", ylab = "Price", xaxt = "n")  # Forecast plot

## ARIMA by using auto.arima 
function_ets <- function(x, h) {
  forecast(ets(stocks), h = h)
}
function_arima <- function(x, h) {
  forecast(auto.arima(stocks), h = h)
}

arima_limit <- window(stocks, start = "2019-05-01")
ts1 <- tsCV(arima_limit, function_ets, h = 1)  # CV Errors model1
ts2 <- tsCV(arima_limit, function_arima, h = 1)  # CV Errors model2
mse_model1 <- mean(ts1^2, na.rm = TRUE)  # Auto-arima MSE for one model class
mse_model2 <- mean(ts2^2, na.rm = TRUE)  # Auto-arima MSE for second model class

## GARCH 
stocks_adj <- cbind(
  Price = GOOG$GOOG.Adjusted,
  Return = CalculateReturns(GOOG$GOOG.Close, method = 'log')
)  # Log transformation for return calculation 
colnames(stocks_adj) <- c('Price', 'Return')

garch_model <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
  distribution = 'std'
)  # Exponential GARCH methodology 

fit_garch <- ugarchfit(spec = garch_model, data = na.omit(stocks_adj$Return))
plot(fit_garch, which = 'all')

## KNN  
df <- data.frame(ds = index(GOOG), y = as.numeric(GOOG[,'GOOG.Close']))
predknn <- knn_forecasting(df$y, h = 30, lags = 1:30, k = 40, msas = "MIMO")  # KNN prediction 

plot(predknn)
 
## Neural network 
alpha <- 1.5^(-10)
hn <- length(stocks) / (alpha * (length(stocks) + 30))
lambda <- BoxCox.lambda(stocks)  # BoxCox lambda parameter 
neural_pred <- nnetar(stocks, size = hn, lambda = lambda)
neural_forecast <- forecast(neural_pred, h = 30, PI = TRUE)  # Neural Network forecast

### Accuracy of the models 
accuracy(myprediction)  # Accuracy of manual ARIMA plot 
accuracy(function_arima)  # Accuracy auto arima 
accuracy(fit_garch)  # GARCH Accuracy 
accuracy(predknn)
accuracy(neural_forecast)  # Neural Network accuracy 

## Analysis using two stocks 
stocks2 <- as.xts(data.frame(SCCO = SCCO[, "SCCO.Adjusted"], WHR = WHR[, "WHR.Adjusted"]))  # Comparing between two different tickers 

stocks_series2 <- tidy(stocks2) %>% 
  ggplot(aes(x = index, y = value, color = series)) + 
  geom_line() +
  facet_grid(series ~ ., scales = "free") + 
  labs(title = "Whirpool Stock & Copper Stock")

stocks_series2

ggplot(data = stocks2, aes(x = WHR.Adjusted, y = SCCO.Adjusted)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.x = 40, label.y = 235) +
  stat_cor(aes(label = ..rr.label..), label.x = 40, label.y = 228)

model1 <- lm(stocks2$WHR.Adjusted ~ stocks2$SCCO.Adjusted)  # Linear regression analysis 
anova1 <- aov(model1)
summary(anova1)
plot(model1)
