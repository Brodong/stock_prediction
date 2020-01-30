# 预测股票价格的收盘价

#library(data.table)
# 导入训练的数据
train <- read.csv("stock_data_BankofChina.csv")
View(train)
summary(train)

# 将日期转换为日期格式
train_date <- as.Date(train$date)
# 将收盘价转换为数值格式
train_close <- as.numeric(train$close)
# 绘图
plot(train_date, train_close, xlab = "Dates", ylab = "Closing Price", type = "l", col = "red", main = "Closing price of Bank of China")

# 导入时间序列包
library(tseries, quietly = T)
# ADF统计量值
adf.test(train_close)
# 使得不稳定的序列变得稳定
nifty_ret <- 100 * diff(log(train_close))
View(nifty_ret)
# 新序列的ADF统计量
adf.test(nifty_ret)
# 自相关因子图
acf(nifty_ret)
pacf(nifty_ret)
summary(arma(nifty_ret, order = c(1, 1)))

# 训练数据
nifty_ret_train <- nifty_ret[1:(0.996 * length(nifty_ret))]  
# 测试数据
nifty_ret_test <- nifty_ret[(0.996 * length(nifty_ret) + 1):length(nifty_ret)] 
fit <- arima(nifty_ret_train, order = c(1,0,1))
library(forecast, quietly = T)
arma.preds <- predict(fit, n.ahead = (length(nifty_ret) - (0.996 * length(nifty_ret))))$pred
arma.forecast <- forecast(fit, h = 15)
plot(arma.forecast, main = "ARMA forecasts for Bank of China")  
accuracy(arma.preds, nifty_ret_test) # RMSE values
View(arma.forecast)
fit2 <- arima(nifty_ret, order=c(1,0,1))
arma.forecast1 <- forecast(fit2, h = 3)
View(arma.forecast1)
plot(arma.forecast1)
print(arma.forecast1)

