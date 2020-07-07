

  #package
  library(stats)
  library(forecast)
  library(zoo)
  library(tseries)
  library(TTR)
  warnings()
  #load the file
  ty <- read.csv("C:/Users/arun/Documents/DeliveryRate.csv")
  set.seed(1000)
  
  attach(ty)
#time series object
  tr<- ts(DeliveryRate,frequency = 12,start=2013)
 class(tr)
 plot(tr)
 summary(tr)
#decompose
 decompose_tr <- decompose(tr)
 plot(decompose_tr)

#stationary
 diff2 <- diff(tr,differences = 2)
 plot(diff2)
 station <- adf.test(diff2)
 suppressWarnings(station)

#acf
  acf(diff2) # from this i found order 2 for Ma
  pacf(diff2) # from this i found order  for ar
  auto.arima(diff2)
  plot(diff2)
#moving avareage for smoothing the data


  mva <- function(tr,a){
    sma <- SMA(tr,a)
    error <- tr[a] - sma[a]
    plt_sma <- plot(sma,main="after smothing",type ="b",col = "red")
    plt_ob <- plot(tr,main = "before smoothing",type = "b",col ="blue")
    return(c(sma[a],error,plt_sma,plt_ob))
    
  }
  mva(tr,5)

  #fit9
  fit9 <- arima(diff2,order = c(4,2,0)) #aic 233
  predf9 <- predict(fit9,n.ahead = 10)
  fut9 <- forecast(fit9,h=4)
  plot(fut9)
  
  
  
  
  #check aic value o arima model

  arima(diff2,order = c(3,2,2)) #aic value 221.42
  auto.arima(diff2,d=2) #aic value 233.22
  auto.arima(diff2)  #aic 207.22
  #lower aic better the model
  
   fier <- arima(diff2,order = c(2,0,1))
  
  predf <- predict(fier,n.ahead = 10)
  fut <- forecast(fier,h=4)
  plot(fut)
  plot(predf$pred)
 
  #aic value
  
  acf(fier$residuals)
  fit8 <- arima(diff2,order = c(3,2,2),)
  predf1 <- predict(fit8,n.ahead = 10)
  fut23 <- forecast(fit8,h=4)
  plot(fut23)
  
  #fier normal
  res_fi <- fier$residuals
  jarque.bera.test(res_fi) # normalydistribute
  
  
  