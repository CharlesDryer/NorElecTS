#TSA of Norwegian electrical energy usage
library(dplyr)
library(graphics)
library(tseries)
library(timeSeries)
library(forecast)

#import data

data = read.csv("/Users/charles/Downloads/norway_emd.csv")
unique(data$geo)
names(data)

#filter for Norwegian data

data_norway_E = data %>% dplyr::filter(geo == "NO") %>% dplyr::filter(nrg_bal == "AIM") %>% dplyr::filter(siec == "E7000") %>% dplyr::filter(unit == "GWH")

#get minimum and maximum date

min_date = min(data_norway_E$TIME_PERIOD); min_date
max_date = max(data_norway_E$TIME_PERIOD); max_date

#plot of all data, from the start of 2008 up to November 2023

data_norway_E$month = as.Date(paste(data_norway_E$TIME_PERIOD,"-01", sep = ""))
par(mfrow = c(1,1));
plot(data_norway_E$month,data_norway_E$OBS_VALUE, xlab = "time", ylab = "Energy (GWH)",col = "magenta",type = "l",main = "Time series of Electrical Energy use in Norway")
data_norway_E

#Box plot to check for outliers

boxplot(data_norway_E$OBS_VALUE)

#plot acf and pacf for Norway

dnpty_an = data_norway_pty %>% select(month, OBS_VALUE)
par("mar")
par(mar = c(1,1,1,1))
par(mfrow = c(1,2))
acf(dnpty_an$OBS_VALUE, main = "acf lag 0")
pacf(dnpty_an$OBS_VALUE, main = "pacf lag 0")
acf(dnpty_an$OBS_VALUE, main = "acf lag 12",lag = 12)
pacf(dnpty_an$OBS_VALUE, main = "pacf lag 12",lag = 12)
#spikes at p = 4 & 6 in the pacf => test p at both values
#Data appears highly seasonal, so likely need a model reflecting this

#looking at making the data stationary with differencing

par(mfrow = c(1,3))
diff12 = diff(dnpty_an$OBS_VALUE,lag = 12)
plot(diff12,type = "l")
acf(diff12)
pacf(diff12)

#table of aic and bic values for peaks of interest

A1 = arima(dnpty_an$OBS_VALUE,order = c(2,0,12), optim.control = list(maxit = 1000),method="ML")
A2 = arima(dnpty_an$OBS_VALUE,order = c(11,0,12), optim.control = list(maxit = 1000),method="ML")
A3 = arima(dnpty_an$OBS_VALUE,order = c(12,0,12), optim.control = list(maxit = 1000),method="ML")


table = matrix(c(A1$aic,A2$aic,A3$aic,BIC(A1),BIC(A2),BIC(A3)),ncol = 2)
colnames(table) = c("AIC","BIC")
rownames(table) = c("ARMA(2,12)", "ARMA(11,12)", "ARMA(12,12)")
table
#select ARMA(11,12)


#making estimates at about 90% of the data:
length(dnpty_an$OBS_VALUE)
#191 data points = > take first 171
m = 20
n = 191-m
future = (n+1):(n+m)

d_train = dnpty_an$OBS_VALUE[1:n]
d_test = dnpty_an$OBS_VALUE[future]
par(mfrow = c(1,1))
Elec = arima(d_train,c(11,0,12))
Elec
par(mfrow=c(1,3))
Elecr = Elec$residuals
plot(Elecr, type="l")
acf(Elecr,  main="",lag = 12)
pacf(Elecr,  main="", lag = 12)
#perform a lijung-box test on the residuals 
res = Elec$residuals
res.acf = acf(res,plot=FALSE)
LB_Elec = Box.test(res, lag = length(res.acf$acf) - 1, type = "Ljung-Box", fitdf = 10)
LB_Elec
#p value = 0.6143


Elec_f = forecast::Arima(d_train, c(11,0,12))
coef(Elec_f)
model_f = forecast(Elec_f, h = m, level = c(90,95))
#
par(mfrow = c(1,1))
plot(model_f,ylab = "Energy (GWH)", xlab = "Time (Months)")
lines(dnpty_an$OBS_VALUE,col = "magenta")
lines(dnpty_an$OBS_VALUE[1:n], col = "black")

#testing our forecast against the data:
d = vector()
ElecF = vector()
for (i in 1:(m-(k-1))){
  values = dnpty_an$OBS_VALUE[1:(n-i+1)]
  ElecV = forecast::Arima(ts(values),order = c(11,0,12),include.constant=TRUE)
  mod.E = forecast::forecast(ElecV,h=k,level=c(90,95)) #mod.f is based on 90% of data
  ElecF[i] = mod.E$mean[k]
  d[i] = ElecF[i]- dnpty_an$OBS_VALUE[n-1+i+k] 
}

MAPE(d,ElecF)
MAPE<-function(d,x){
  return(mean(100*abs(d/x)))
}
#16.4%


#forecasting values for december 2023 - march 2024, using rolling data and the whole model

Elec_f2 = forecast::Arima(dnpty_an$OBS_VALUE, c(11,0,12))
Elec_dm = forecast(Elec_f2, h = 4, level = c(90,95))
plot(Elec_dm ,main='Forecast for Norway Electrical Consumption to March 2024',ylab="Energy (KWH)",xlab="Time (Months)")
Elec_dm$mean


