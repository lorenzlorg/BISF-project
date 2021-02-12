#-------------------------DATA SUMMARY-------------------------


#caricamento librerie 
library(quantmod)
library(zoo)
library(xts)
library(PerformanceAnalytics) 
library(dygraphs)
library(forecast)
library(tseries)

#utilizzando la funzione "getSymbol" scarico direttamente da Yahoo i dati che andrò ad analizzare
getSymbols("AAPL", src = "yahoo", from = as.Date("2010-01-01"), to = as.Date("2019-10-31"))
getSymbols("MSFT", src = "yahoo", from = as.Date("2010-01-01"), to = as.Date("2019-10-31"))
getSymbols("AMZN", src = "yahoo", from = as.Date("2010-01-01"), to = as.Date("2019-10-31"))
getSymbols("GOOG", src = "yahoo", from = as.Date("2010-01-01"), to = as.Date("2019-10-31"))

#rinomino per ciascun stock la colonna relativa all'adjusted closing price
colnames(AAPL)[6] <- "AAPL"
colnames(MSFT)[6] <- "MSFT"
colnames(AMZN)[6] <- "AMZN"
colnames(GOOG)[6] <- "GOOG"

#estraggo per ciascun stock la sola colonna relativa all'adjusted closing price
AAPL.xts <- na.omit(AAPL)[, 6]
MSFT.xts <- na.omit(MSFT)[, 6]
AMZN.xts <- na.omit(AMZN)[, 6]
GOOG.xts <- na.omit(GOOG)[, 6]

#unisco tutti gli adjusted closing price dei quattro stocks
stocks_Adj_Close_Daily <- na.omit(cbind(AAPL.xts, MSFT.xts, AMZN.xts, GOOG.xts))

##grafici su base giornaliera
plot(ts(stocks_Adj_Close_Daily, start = c(2010, 1), frequency=365, end=c(2019, 10)), main="Daily Adjusted Closing Price")

dygraph(stocks_Adj_Close_Daily, main="Daily Adjusted Closing Price")
plot(stocks_Adj_Close_Daily, col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"), main="Daily Adjusted Closing Price")
addLegend("topleft", legend.names = c("AAPL", "MSFT", "AMZN", "GOOG"), col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"), lwd=c(1, 1,1,1))

dygraph(AAPL.xts, main="APPLE")
plot(AAPL.xts, main="APPLE")

dygraph(MSFT.xts, main="MICROSOFT")
plot(MSFT.xts, main="MICROSOFT")

dygraph(AMZN.xts, main="AMAZON")
plot(AMZN.xts, main="AMAZON")

dygraph(GOOG.xts, main="GOOGLE")
plot(GOOG.xts, main="GOOGLE")

#utilizzando la funzione "scale" si ha una comparazione più equa
stocks_Adj_Close_Dayly_Scale <- scale(stocks_Adj_Close_Daily)
dygraph(stocks_Adj_Close_Dayly_Scale, main="Daily Adjusted Closing Price")
plot(stocks_Adj_Close_Dayly_Scale, col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"), main="Daily Adjusted Closing Price")


#-------------------------DESCRIPTIVE ANALYTICS-------------------------


#converto i dati da base giornaliera a base mensile
stocks_Adj_Close_Monthly <- to.monthly(stocks_Adj_Close_Daily, OHLC = FALSE)
colnames(stocks_Adj_Close_Monthly) <- c("AAPL", "MSFT", "AMZN", "GOOG")

#grafico analogo ai precedenti ma su base mensile
dygraph(stocks_Adj_Close_Monthly, main="Monthly Adjusted Closing Price")

#calcolo i ritorni semplici e composti su base mensile
simple_monthly_returns <- na.omit(CalculateReturns(stocks_Adj_Close_Monthly, method = "simple"))
compound_monthly_returns <- na.omit(CalculateReturns(stocks_Adj_Close_Monthly, method = "compound")) #in modo alternativo: diff(log(stocks_Adj_Close_Monthly))

dygraph(simple_monthly_returns)%>%dyRangeSelector(height = 20)
plot(simple_monthly_returns, main="Simple Monthly Return", col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"))
addLegend("top", legend.names = c("AAPL", "MSFT", "AMZN", "GOOG"), col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"), lwd=c(1, 1,1,1))

dygraph(compound_monthly_returns)%>%dyRangeSelector(height = 20)
plot(compound_monthly_returns, main="Compund Monthly Return", col=c("red", "deepskyblue1", "darkorchid1", "chartreuse")) 
addLegend("top", legend.names = c("AAPL", "MSFT", "AMZN", "GOOG"), col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"), lwd=c(1, 1,1,1))

#confronto i ritorni composti degli stock presi a due a due per avere maggiore chiarezza
#AAPL-MSFT
dygraph(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$MSFT),main="AAPL-MSFT")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$MSFT), main="AAPL-MSFT", col=c("red", "deepskyblue1") )
addLegend("topright", legend.names = c("AAPL", "MSFT"), col=c("red", "deepskyblue1"), lwd=c(3, 3, 3, 3))

#AAPL-GOOG
dygraph(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$GOOG),main="AAPL-GOOG")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$MSFT), main="AAPL-GOOG", col=c("red", "chartreuse") )
addLegend("topright", legend.names = c("AAPL", "GOOG"), col=c("red", "chartreuse"), lwd=c(3, 3, 3, 3))

#AAPL-AMZN
dygraph(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$AMZN),main="AAPL-AMZN")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$AAPL, compound_monthly_returns$AMZN), main="AAPL-AMZN", col=c("red", "darkorchid1") )
addLegend("topright", legend.names = c("AAPL", "AMZN"), col=c("red", "darkorchid1"), lwd=c(3, 3, 3, 3))

#AMZN-GOOG
dygraph(cbind(compound_monthly_returns$AMZN, compound_monthly_returns$GOOG),main="AMZN-GOOG")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$AMZN, compound_monthly_returns$GOOG), main="AMZN-GOOG", col=c("darkorchid1", "chartreuse") )
addLegend("topright", legend.names = c("AMZN", "GOOG"), col=c("darkorchid1", "chartreuse"), lwd=c(3, 3, 3, 3))

#AMZN-MSFT
dygraph(cbind(compound_monthly_returns$AMZN, compound_monthly_returns$MSFT),main="AMZN-MSFT")%>%
  dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
  dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$AMZN, compound_monthly_returns$MSFT), main="AMZN-MSFT", col=c("darkorchid1", "deepskyblue1") )
addLegend("topright", legend.names = c("AMZN", "MSFT"), col=c("darkorchid1", "deepskyblue1"), lwd=c(3, 3, 3, 3))

#MSFT-GOOG
dygraph(cbind(compound_monthly_returns$MSFT, compound_monthly_returns$GOOG),main="MSFT-GOOG")%>%
   dyOptions(colors = c("blue","red"),stackedGraph = FALSE)%>%
   dyRangeSelector(height = 20)
plot(cbind(compound_monthly_returns$MSFT, compound_monthly_returns$GOOG), main="MSFT-GOOG", col=c("deepskyblue1", "chartreuse") )
addLegend("topright", legend.names = c("MSFT", "GOOG"), col=c("deepskyblue1", "chartreuse"), lwd=c(3, 3, 3, 3))


#diagnostic plots

#histograms
par(mfrow=c(2,2))

AAPL.mat <- coredata(compound_monthly_returns$AAPL)
hist(AAPL.mat, main="Histogram of AAPL monthly return", probability = TRUE, col="red", breaks = 20)

AMZN.mat <- coredata(compound_monthly_returns$AMZN)
hist(AMZN.mat, main="Histogram of AMZN monthly return", probability = TRUE, col="darkorchid1", breaks = 20)

GOOG.mat <- coredata(compound_monthly_returns$GOOG)
hist(GOOG.mat, main="Histogram of GOOG monthly return", probability = TRUE, col="chartreuse", breaks = 20)

MSFT.mat <- coredata(compound_monthly_returns$MSFT)
hist(MSFT.mat, main="Histogram of MSFT monthly return", probability = TRUE, col="deepskyblue1", breaks = 20)

#smoothed density plots
par(mfrow=c(2,2))

AAPL.density <- density(AAPL.mat)
plot(AAPL.density, type="l", xlab="returns", col="red", lwd=2, ylab="density estimate", main="Smoothed AAPL histogram")

AMZN.density <- density(AMZN.mat)
plot(AMZN.density, type="l", xlab="returns", col="darkorchid1", lwd=2, ylab="density estimate", main="Smoothed AMZN histogram")

GOOG.density <- density(GOOG.mat)
plot(GOOG.density, type="l", xlab="returns", col="chartreuse", lwd=2, ylab="density estimate", main="Smoothed GOOG histogram")

MSFT.density <- density(MSFT.mat)
plot(MSFT.density, type="l", xlab="returns", col="deepskyblue1", lwd=2, ylab="density estimate", main="Smoothed MSFT histogram")

#box plots
par(mfrow=c(2,2))

boxplot(AAPL.mat, main="AAPL boxplot", ylab="monthly AAPL return", col="red")

boxplot(AMZN.mat, main="AMZN boxplot", ylab="monthly AMZN return", col="darkorchid1")

boxplot(GOOG.mat, main="GOOG boxplot", ylab="monthly GOOG return", col="chartreuse")

boxplot(MSFT.mat, main="MSFT boxplot", ylab="monthly MSFT return", col="deepskyblue1")

#QQ plots
par(mfrow=c(2,2))

qqnorm(AAPL.mat, main="AAPL", col="red")
qqline(AAPL.mat)

qqnorm(AMZN.mat, main="AMZN", col="darkorchid1")
qqline(AMZN.mat)

qqnorm(GOOG.mat, main="GOOG", col="chartreuse")
qqline(GOOG.mat)

qqnorm(MSFT.mat, main="MSFT", col="deepskyblue1")
qqline(MSFT.mat)

#Boxplot 
par(mfrow=c(1,1))
AAPL_MSFT_AMZN_GOOG.mat <- cbind(AAPL.mat, MSFT.mat, AMZN.mat, GOOG.mat)
boxplot(AAPL_MSFT_AMZN_GOOG.mat, col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"))


#grafici diagnostici per singola stock
par(mfrow=c(2,2))

#aaple
hist(AAPL.mat, main="Histogram of AAPL monthly return", probability = TRUE, col="red", breaks = 20)
plot(AAPL.density, type="l", xlab="returns", col="red", lwd=2, ylab="density estimate", main="Smoothed AAPL histogram")
boxplot(AAPL.mat, main="AAPL boxplot", ylab="monthly AAPL return", col="red")
qqnorm(AAPL.mat, main="AAPL", col="red")
qqline(AAPL.mat)

#amazon
hist(AMZN.mat, main="Histogram of AMZN monthly return", probability = TRUE, col="darkorchid1", breaks = 20)
plot(AMZN.density, type="l", xlab="returns", col="darkorchid1", lwd=2, ylab="density estimate", main="Smoothed AMZN histogram")
boxplot(AMZN.mat, main="AMZN boxplot", ylab="monthly AMZN return", col="darkorchid1")
qqnorm(AMZN.mat, main="AMZN", col="darkorchid1")
qqline(AMZN.mat)

#google
hist(GOOG.mat, main="Histogram of GOOG monthly return", probability = TRUE, col="chartreuse", breaks = 20)
plot(GOOG.density, type="l", xlab="returns", col="chartreuse", lwd=2, ylab="density estimate", main="Smoothed GOOG histogram")
boxplot(GOOG.mat, main="GOOG boxplot", ylab="monthly GOOG return", col="chartreuse")
qqnorm(GOOG.mat, main="GOOG", col="chartreuse")
qqline(GOOG.mat)

#microsoft
hist(MSFT.mat, main="Histogram of MSFT monthly return", probability = TRUE, col="deepskyblue1", breaks = 20)
plot(MSFT.density, type="l", xlab="returns", col="deepskyblue1", lwd=2, ylab="density estimate", main="Smoothed MSFT histogram")
boxplot(MSFT.mat, main="MSFT boxplot", ylab="monthly MSFT return", col="deepskyblue1")
qqnorm(MSFT.mat, main="MSFT", col="deepskyblue1")
qqline(MSFT.mat)

#calcolo delle principali statistiche descrittive

#mean
AAPL_mean <- mean(AAPL.mat)
MSFT_mean <- mean(MSFT.mat)
AMZN_mean <- mean(AMZN.mat)
GOOG_mean <- mean(GOOG.mat)

mean_stocks <- c(AAPL_mean, MSFT_mean, AMZN_mean, GOOG_mean)

#variance
AAPL_var <- var(AAPL.mat)
MSFT_var <- var(MSFT.mat)
AMZN_var <- var(AMZN.mat)
GOOG_var <- var(GOOG.mat)

var_stocks <- c(AAPL_var, MSFT_var, AMZN_var, GOOG_var)

#standard deviation
AAPL_sd <- sd(AAPL.mat)
MSFT_sd <- sd(MSFT.mat)
AMZN_sd <- sd(AMZN.mat)
GOOG_sd <- sd(GOOG.mat)

sd_stocks <- c(AAPL_sd, MSFT_sd, AMZN_sd, GOOG_sd)

#skewness
AAPL_sk <- skewness(AAPL.mat)
MSFT_sk <- skewness(MSFT.mat)
AMZN_sk <- skewness(AMZN.mat)
GOOG_sk <- skewness(GOOG.mat)

sk_stocks <- c(AAPL_sk, MSFT_sk, AMZN_sk, GOOG_sk)

#kurtosis
AAPL_kurt <- kurtosis(AAPL.mat)
MSFT_kurt <- kurtosis(MSFT.mat)
AMZN_kurt <- kurtosis(AMZN.mat)
GOOG_kurt <- kurtosis(GOOG.mat)

kurt_stocks <- c(AAPL_kurt, MSFT_kurt, AMZN_kurt, GOOG_kurt)

#tabella riassuntiva
descriptive_statistics_stocks <- cbind(mean_stocks, var_stocks, sd_stocks, sk_stocks, kurt_stocks)
row.names(descriptive_statistics_stocks) <- c("AAPL", "MSFT", "AMZN", "GOOG")
colnames(descriptive_statistics_stocks) <- c("MEAN", "VARIANCE", "STANDARD DEVIATION", "SKEWNESS", "KURTOSIS")
print(descriptive_statistics_stocks)

#quantile
AAPL_quant <- quantile(AAPL.mat)
MSFT_quant <- quantile(MSFT.mat)
AMZN_quant <- quantile(AMZN.mat)
GOOG_quant <- quantile(GOOG.mat)

quant_stocks <- cbind(AAPL_quant, MSFT_quant, AMZN_quant, GOOG_quant)
colnames(quant_stocks) <- c("AAPL", "MSFT", "AMZN", "GOOG")
print(quant_stocks)

#volatility
par(mfrow = c(1,1))
variance_stocks <- c(AAPL_var, MSFT_var, AMZN_var, GOOG_var)
names(variance_stocks) <- c("AAPL", "MSFT", "AMZN", "GOOG")
barplot(variance_stocks, main="Volatility", col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"))

#sample covariance matrix
SAMPLE_COV_MAT <- var(AAPL_MSFT_AMZN_GOOG.mat)
print(SAMPLE_COV_MAT)

#sample correlation matrix
SAMPLE_CORR_MAT <- cor(AAPL_MSFT_AMZN_GOOG.mat)
print(SAMPLE_CORR_MAT)

#pair-wise scatterplots
pairs(AAPL_MSFT_AMZN_GOOG.mat, col="blue", pch=18)


#-------------------------PREDICTIVE ANALYTICS-------------------------


#AAPL
dataset.z <- get.hist.quote( instrument="AAPL", start="2010-01-01", end="2019-10-31", quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="month")

index(dataset.z) <- as.yearmon(index(dataset.z))
colnames(dataset.z) <- "AAPL"
dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
index(dataset.z) <- as.yearmon(index(dataset.z))
head(dataset.z)

#new
fitVal <- stl(dataset.z[,1], s.window = "periodic")
plot(fitVal,main="Decomposition of AAPL", col="red")

returns <- diff( log(dataset.z[,1]) )

returnsTrain <- returns[1:(0.9 * length(returns))]
length(returnsTrain)
tail(returnsTrain)

returnsTest <- returns[(0.9 * length(returns) + 1):length(returns)]
length(returnsTest)

fit <- auto.arima(returnsTrain)
arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.9 * length(returns))))$pred

arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95,80))
arma.forecast

plot(arma.forecast, main = "ARMA forecasts for AAPL returns", col="red")
lines(returnsTest)

accuracy(arma.predictions, returnsTest)
accuracy(arma.predictions, returnsTest)[2]

lines(returnsTest)

#MSFT
dataset.z <- get.hist.quote( instrument="MSFT", start="2010-01-01", end="2019-10-31", quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="month")

index(dataset.z) <- as.yearmon(index(dataset.z))
colnames(dataset.z) <- "MSFT"
dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
index(dataset.z) <- as.yearmon(index(dataset.z))
head(dataset.z)

#new
fitVal <- stl(dataset.z[,1], s.window = "periodic")
plot(fitVal,main="Decomposition of MSFT", col="deepskyblue1")

returns <- diff( log(dataset.z[,1]) )

returnsTrain <- returns[1:(0.9 * length(returns))]
length(returnsTrain)
tail(returnsTrain)

returnsTest <- returns[(0.9 * length(returns) + 1):length(returns)]
length(returnsTest)

fit <- auto.arima(returnsTrain)
arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.9 * length(returns))))$pred

arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95,80))
arma.forecast

plot(arma.forecast, main = "ARMA forecasts for MSFT returns", col="deepskyblue1")
lines(returnsTest)

accuracy(arma.predictions, returnsTest)
accuracy(arma.predictions, returnsTest)[2]

lines(returnsTest)

#AMZN
dataset.z <- get.hist.quote( instrument="AMZN", start="2010-01-01", end="2019-10-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01", compression="month")

index(dataset.z) <- as.yearmon(index(dataset.z))
colnames(dataset.z) <- "AMZN"
dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
index(dataset.z) <- as.yearmon(index(dataset.z))
head(dataset.z)

#new
fitVal <- stl(dataset.z[,1], s.window = "periodic")
plot(fitVal,main="Decomposition of AMZN", col="darkorchid1")

returns <- diff( log(dataset.z[,1]) )

returnsTrain <- returns[1:(0.9 * length(returns))]
length(returnsTrain)
tail(returnsTrain)

returnsTest <- returns[(0.9 * length(returns) + 1):length(returns)]
length(returnsTest)

fit <- auto.arima(returnsTrain)
arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.9 * length(returns))))$pred

arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95,80))
arma.forecast

plot(arma.forecast, main = "ARMA forecasts for AMZN returns", col="darkorchid1")
lines(returnsTest)

accuracy(arma.predictions, returnsTest)
accuracy(arma.predictions, returnsTest)[2]

lines(returnsTest)

#GOOG
dataset.z <- get.hist.quote( instrument="GOOG", start="2010-01-01", end="2019-10-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01", compression="month")

index(dataset.z) <- as.yearmon(index(dataset.z))
colnames(dataset.z) <- "GOOG"
dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
index(dataset.z) <- as.yearmon(index(dataset.z))
head(dataset.z)

#new
fitVal <- stl(dataset.z[,1], s.window = "periodic")
plot(fitVal,main="Decomposition of GOOG", col="chartreuse")

returns <- diff( log(dataset.z[,1]) )

returnsTrain <- returns[1:(0.9 * length(returns))]
length(returnsTrain)
tail(returnsTrain)

returnsTest <- returns[(0.9 * length(returns) + 1):length(returns)]
length(returnsTest)

fit <- auto.arima(returnsTrain)
arma.predictions <- predict(fit, n.ahead = (length(returns) - (0.9 * length(returns))))$pred

arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95,80))
arma.forecast

plot(arma.forecast, main = "ARMA forecasts for GOOG returns", col="chartreuse")
lines(returnsTest)

accuracy(arma.predictions, returnsTest)
accuracy(arma.predictions, returnsTest)[2]

lines(returnsTest)


# # Ho provato a farlo anche diversamente ma riscontro delle anomalie
# fit <- stl(stocks_Adj_Close_Monthly$AAPL[,1], s.window="period" )
# fitRet <- stl( compound_monthly_returns$AAPL[,1], s.window="period" )
# 
# returnsTrain <- compound_monthly_returns$AAP[1 : 96]  
# returnsValidation <- compound_monthly_returns$AAP[97 : 108]
# returnsTest <- compound_monthly_returns$AAP[108 : 117]  
# fit <- arima(returnsTrain, order = c(2, 0, 4))
# 
# arma.preds <- predict(fit, n.ahead = 30)$pred
# arma.forecast <- forecast(fit, h = 40)
# plot(arma.forecast, main = "AAPL")
# 
# accuracy(arma.preds, returnsValidation)[2] 
# 
# lines(returnsValidation, col="red")


#-------------------------BETA COMPUTATION-------------------------


#indice di riferimento
NASDAQ.xts <- getSymbols("^IXIC", from="2010-01-01", to="2019-10-31", src='yahoo', auto.assign = FALSE )
NASDAQ.xts <- to.monthly(NASDAQ.xts)
NASDAQ <-na.omit(NASDAQ.xts)[, 6]
NASDAQ <- na.omit(diff(log(NASDAQ)))
colnames(NASDAQ) <- c("NASDAQ")

#funzione per calcolare beta
beta_function <- function(stock, market_index){
  beta <- cov(stock, market_index)/var(market_index)
  return(beta)
}

AAPL_betas.xts <- NULL
MSFT_betas.xts <- NULL
AMZN_betas.xts <- NULL
GOOG_betas.xts <- NULL

#viene scelta una finestra temporale di 20 mesi
delta_t <- 20 
length_period = dim(NASDAQ)[1] 

start <- delta_t+1 

for (i in start:length_period){
  beta_val_AAPL <- beta_function(compound_monthly_returns$AAPL[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_val_MSFT <- beta_function(compound_monthly_returns$MSFT[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_val_AMZN <- beta_function(compound_monthly_returns$AMZN[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  beta_val_GOOG <- beta_function(compound_monthly_returns$GOOG[(i-delta_t):(i-1)], NASDAQ[(i-delta_t):(i-1)])
  
  beta_xts_AAPL <- as.xts(beta_val_AAPL, order.by = index(compound_monthly_returns$AAPL[(i-1)]))
  beta_xts_MSFT <- as.xts(beta_val_MSFT, order.by = index(compound_monthly_returns$MSFT[(i-1)]))
  beta_xts_AMZN <- as.xts(beta_val_AMZN, order.by = index(compound_monthly_returns$AMZN[(i-1)]))
  beta_xts_GOOG <- as.xts(beta_val_GOOG, order.by = index(compound_monthly_returns$GOOG[(i-1)]))
  beta_xts_MSFT <- as.xts(beta_val_MSFT, order.by = index(compound_monthly_returns$MSFT[(i-1)]))
  
  #vengono create time series di beta per ciascuno stock
  if(is.null(AAPL_betas.xts)){
    AAPL_betas.xts <- beta_xts_AAPL
    MSFT_betas.xts <- beta_xts_MSFT
    AMZN_betas.xts <- beta_xts_AMZN
    GOOG_betas.xts <- beta_xts_GOOG
    
  }else{
    AAPL_betas.xts <- rbind(AAPL_betas.xts,beta_xts_AAPL)
    MSFT_betas.xts <- rbind(MSFT_betas.xts,beta_xts_MSFT)
    AMZN_betas.xts <- rbind(AMZN_betas.xts,beta_xts_AMZN)
    GOOG_betas.xts <- rbind(GOOG_betas.xts,beta_xts_GOOG)
    
  }
  
   print('------time windows-------')
   print(paste("Start time window:", index(compound_monthly_returns$AAPL)[i-delta_t]))
   print(paste("End time window:  ", index(compound_monthly_returns$AAPL)[i-1]))
   print('------date for beta------')
   print(paste("Time index beta: ", index(compound_monthly_returns$AAPL)[i]))
   print(paste("AAPL beta:", beta_val_AAPL))
   print(paste("MSFT beta:", beta_val_MSFT))
   print(paste("AMZN beta:", beta_val_AMZN))
   print(paste("GOOG beta:", beta_val_GOOG))
   
  
}

#confronto di tutti i valori di beta
par(mfrow=c(1,1))
betas <- merge(AAPL_betas.xts$NASDAQ, MSFT_betas.xts$NASDAQ, AMZN_betas.xts$NASDAQ, GOOG_betas.xts$NASDAQ)
colnames(betas) <- c("AAPL", "MSFT", "AMZN", "GOOG")
dygraph(betas)

#per avere la stessa lunghezza
AAPL_betas <- as.xts(c(rep(NA,delta_t), as.numeric(AAPL_betas.xts)), order.by = index(compound_monthly_returns$AAPL))
MSFT_betas <- as.xts(c(rep(NA,delta_t), as.numeric(MSFT_betas.xts)), order.by = index(compound_monthly_returns$MSFT))
AMZN_betas <- as.xts(c(rep(NA,delta_t), as.numeric(AMZN_betas.xts)), order.by = index(compound_monthly_returns$AMZN))
GOOG_betas <- as.xts(c(rep(NA,delta_t), as.numeric(GOOG_betas.xts)), order.by = index(compound_monthly_returns$GOOG))


#per un ulteriore confronto tra indice e beta dei singoli stocks
par(mfrow=c(2,1))
plot(NASDAQ,type="l", main="Nasdaq")
plot(AAPL_betas,type="l",main="Beta AAPL", col="red")

plot(NASDAQ,type="l", main="Nasdaq")
plot(MSFT_betas,type="l",main="Beta MSFT", col="deepskyblue1")

plot(NASDAQ,type="l", main="Nasdaq")
plot(AMZN_betas,type="l",main="Beta AMZN", col="darkorchid1")

plot(NASDAQ,type="l", main="Nasdaq")
plot(GOOG_betas,type="l",main="Beta GOOG", col="chartreuse")

#confronto dei beta relativi ai 4 stocks
par(mfrow=c(2,2))
plot(AAPL_betas.xts,type="l",main="Beta AAPL", col="red")
plot(MSFT_betas.xts,type="l",main="Beta MSFT", col="deepskyblue1")
plot(AMZN_betas.xts,type="l",main="Beta AMZN", col="darkorchid1")
plot(GOOG_betas.xts,type="l",main="Beta GOOG", col="chartreuse")



#-------------------------PORTFOLIO MENAGEMENT-------------------------


#portfolio ottimizzato

opt <- portfolio.optim(compound_monthly_returns)
head(opt)

#La funzione portfolio.optim mi crea una lista costituita dai seguenti elementi:

# pw	
# the portfolio weights.

# px	
# the returns of the overall portfolio.

# pm	
# the expected portfolio return.

# ps	
# the standard deviation of the portfolio returns.

#pesi del portfolio ottimizzato
portfolio_weights <- opt$pw
names(portfolio_weights) <- colnames(compound_monthly_returns)
head(portfolio_weights)
#aaple: 26.5%     microsoft: 35.9%      amazon: 20.9%     GOOGe: 16.7%

#rappresento i pesi (in percentuale) tramite un barplot
barplot(portfolio_weights*100, main= "Pesi del portfolio",xlab="Stocks", ylab="Weight", col=c("red", "deepskyblue1", "darkorchid1", "chartreuse"))

#si investono 43000 su un budget iniziale di 50000
budget <- 43000

#prezzi al momento dell'acquisto e al momento della vendita
price_buy <- stocks_Adj_Close_Daily["2019-01-02", drop = FALSE, which.i=FALSE]
price_buy.matrix = data.matrix(as.data.frame(price_buy))

price_sell<- stocks_Adj_Close_Daily["2019-10-30", drop = FALSE, which.i=FALSE]
price_sell.matrix = data.matrix(as.data.frame(price_sell))

#permette di calcolare la quantità di denaro da considerare per ciascuna stock
p <- function() {budget * portfolio_weights}

#permette di calcolare il numero di azioni di ciascuna stock che è possibile considerare
q <- function() {p() / price_buy.matrix}

#dal momento in cui i pesi dei vari stock devono essere valori discreti approssimo i valori precedenti
floor(q())
z <- floor(q())

price_init <- function() {price_buy.matrix * z}
price_init_v <- price_init()

price_fin <- function() {price_sell.matrix * z}
price_fin_v <- price_fin()

#totale inziale
tot_init = 0
for (i in 1:4) {
    tot_init = tot_init + price_init_v[1,i]
}
print(tot_init)

#totale finale
tot_final = 0
for (i in 1:4) {
    tot_final= tot_final + price_fin_v[1,i]
}
print(tot_final)

#guadagno lordo totale
gross_return <- (tot_final - tot_init)

#si considerano ora le transazioni

#transazioni acquisto del 5%
buy_trans = 0
for (i in 1:4) {
    buy_trans= buy_trans + (price_init_v[1,i] * 0.05)
}
print(buy_trans)

#transazioni vendita del 5%
sell_trans = 0
for (i in 1:4) {
  sell_trans= sell_trans + (price_fin_v[1,i] * 0.05)
}
print(sell_trans)

#totale transazioni
tot_trans <- buy_trans + sell_trans
print(tot_trans)

#guadagno netto totale
net_return <- (gross_return - tot_trans)
print (net_return)

#ritorno atteso del portfolio
print(opt$pm)

