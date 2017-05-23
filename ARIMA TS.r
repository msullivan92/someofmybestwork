

setwd("C:/Users/sullt/Desktop/GSE 522")
load("classData.Rdata")
library(astsa)
library(tseries)
library(ggplot2)
library(CombMSC)

P <- read.csv("PaperTowelSales_Data.csv",header=T)

yt <- ts(P$Yt) # convert Yt into a time series object
layout(1:2)
plot(yt, main="Yt=Paper Towel Sales")
zt <- diff(yt,differences=1)
plot(yt, main="Yt=Paper Towel Sales")
plot(zt, main="First differences of Yt" )


# since zt plot shows stationarity, compute acf and pacf of zt
acf(zt)
pacf(zt)

# ACF of zt has a spike at lag 1, and then it cuts off.
# PACF of zt oscillates and dies down
# this suggests ARIMA(0,1,1)for yt


a1 <- arima(yt,order=c(0,1,1),include.mean=T)
acf(a1$residuals)

# if we fit ARIMA(0,1,2) model
a2 <- arima(yt,order=c(0,1,2),include.mean=T)
print(a2)

# note that AIC(a1) < AIC(a2), hence model a1 is preferable.
library(forecast)
a1forecasts <- forecast.Arima(a1, h=5)
layout(1)
plot.forecast(a1forecasts)

# run Ljung-Box test in R

Pvalue <- vector()
for (i in 1:24)
 {
  bt <- Box.test (a1$residuals, lag = i,  type="Ljung")
 print("Lag = ")
 print(i)
 Pvalue[i] <- (bt$p.value)
 }
j <- 1:24
plot(j,Pvalue,ymin=0,col="blue",pch=16,ylim=c(0,1))
abline(h=.05)

# alternative analysis of paper towel sales data
t <- c(1:120)
ts <- (t-mean(t))/sd(t)
ts2 <- ts**2

lm1 <- lm(yt~ts+ts2)
layout(1:2)
acf(lm1$residuals)
pacf(lm1$residuals)

x <- cbind(ts,ts2) # add standardized t and t-square as predictors
A1 <- arima(yt, order=c(2,0,0),xreg=x)

layout(1:2)
acf(A1$residuals)
pacf(A1$residuals)

Pvalue2 <- vector() 
for (i in 1:24)
 {
  Bt2 <- Box.test (A1$residuals, lag = i, type="Ljung")
Pvalue2[i] <- Bt2$p.value
}
plot(j,Pvalue2,ymin=0,col="blue",pch=16,ylim=c(0,1))
abline(h=.05)

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################


##################### MLR Example #############################################
P <- read.csv("Profit.csv",header=TRUE)
lm10 <- lm(PROFIT~ AREA+POPN+OUTLETS+Franchise+AvgIncome, data =P)
summary(lm10)

library(car)
vif(lm10)

lm11 <- lm(PROFIT~ AREA+OUTLETS+Franchise+AvgIncome, data =P)
summary(lm11)
vif(lm11)

lm12 <- lm(PROFIT~ AREA+Franchise, data =P)
summary(lm12)
vif(lm12)

qqnorm(lm12$residuals)
qqline(lm12$residuals)


################## Time Series Regression #####################################
G <- read.csv("gaming_revenue.csv",header=TRUE)
G$t <- 1:90

t1 <- ggplot(G, aes(x=t, y=Strip)) +
    geom_line()+labs(title="Strip Revenue") 

t2 <- ggplot(G, aes(x=t, y=Downtown)) +
    geom_line()+labs(title="Downtown Revenue")

t3 <- ggplot(G, aes(x=t, y=BC)) +
    geom_line()+labs(title="Boulder City Revenue")

grid.arrange(t1,t2,t3,ncol=2, nrow =2)
G$tsqr <- G$t*G$t

# dummy variables for 12-1 = 11 months
 G$DFeb<-as.numeric(G$Month==2)
 G$DMar<-as.numeric(G$Month==3)
 G$DApr<-as.numeric(G$Month==4)
 G$DMay<-as.numeric(G$Month==5)
 G$DJun<-as.numeric(G$Month==6)
 G$DJul<-as.numeric(G$Month==7)
 G$DAug<-as.numeric(G$Month==8)
 G$DSep<-as.numeric(G$Month==9)
 G$DOct<-as.numeric(G$Month==10)
 G$DNov<-as.numeric(G$Month==11)
 G$DDec<-as.numeric(G$Month==12)

G$y1 <-  G$Strip/1000000
lm1 <- lm(y1~t+tsqr+DFeb+ DMar+ DApr+ DMay+ DJun+ DJul+ DAug+ DSep+ DOct+ DNov+ DDec, data=G)
summary(lm1)
lm2 <- lm(y1~t+tsqr+DFeb+DApr+DJun+ DJul+ DAug+DNov+ DDec, data=G)
summary(lm2)
lm3 <- lm(y1~t+tsqr+DApr+DJun+ DJul+ DAug+DNov, data=G)
summary(lm3)
vif(lm1)
lm4 <- lm(y1~t+tsqr+DApr+DJun, data=G)
summary(lm4)
library(car)
vif(lm4)

###### Since this data set is a time series, we must look at 
# autocorrelation among residuals.
# Autocorrelation = correlation of a variable with itself 
# ac(lag=h) = correlation(Y(t),Y(t-h))

# Partial Autocorrelation at lag h = correlation of Y(t) and Y(t-h) 
# after removing effect of Y(1), ..., Y(t-h+1)

layout(1:2)
acf(lm4$residuals)
pacf(lm4$residuals)

X <- cbind(G$t,G$tsqr,G$DApr,G$DJun)

ts1 <- arima(G$y1, order=c(3,0,0), xreg=X)
print(ts1)

layout(1:2)
acf(ts1$residuals)
pacf(ts1$residuals)

# To decide on ARIMA(p,0,q) terms, look at both acf and pacf plots.
# one of these will quickly reduce to 0, and the other will cut-off
# after first few 

# Test if residuals from the final TS-REG model are not auto-correlated
# using the LjungBox test
# The LB-test is applied to the residuals of a time series #
# after fitting an ARMA(p,q) model to the data. The test #
# examines m autocorrelations of the residuals. If the #
# autocorrelations are very small, we conclude that the model #
# does not exhibit significant lack of fit.
Pvalue <- vector()
for (i in 1:10)
{
bt <- Box.test (ts1$residuals, lag = i, type="Ljung")
print(bt[3])

Pvalue[i] <- bt[3]
}
#Pvalue is a list object in R, so unlist it
Pvalue <- unlist(Pvalue)
plot(1:10,Pvalue,ylim=c(0,1))
abline(h = 0.05,col = "red")

# since all P-values to lag 10 are > .05, conclude that 
# the TS-REG residuals are not autocorrelated; this completes
# the TS-REG modeling exercise.

######################################################################################
######################################################################################
######################################################################################



##################### MLR Example #############################################
P <- read.csv("Profit.csv",header=TRUE)
lm10 <- lm(PROFIT~ AREA+POPN+OUTLETS+Franchise+AvgIncome, data =P)
summary(lm10)

library(car)
vif(lm10)

lm11 <- lm(PROFIT~ AREA+OUTLETS+Franchise+AvgIncome, data =P)
summary(lm11)
vif(lm11)

lm12 <- lm(PROFIT~ AREA+Franchise, data =P)
summary(lm12)
vif(lm12)

qqnorm(lm12$residuals)
qqline(lm12$residuals)


################## Time Series Regression #####################################
G <- read.csv("gaming_revenue.csv",header=TRUE)
G$t <- 1:90

t1 <- ggplot(G, aes(x=t, y=Strip)) +
    geom_line()+labs(title="Strip Revenue") 

t2 <- ggplot(G, aes(x=t, y=Downtown)) +
    geom_line()+labs(title="Downtown Revenue")

t3 <- ggplot(G, aes(x=t, y=BC)) +
    geom_line()+labs(title="Boulder City Revenue")

grid.arrange(t1,t2,t3,ncol=2, nrow =2)
G$tsqr <- G$t*G$t

data=read.csv("Book2.csv",header=TRUE)

year.f=factor(data$Year)
dummies=model.matrix(~year.f)
dummies

# dummy variables for 12-1 = 11 months

 G$DFeb<-as.numeric(G$Month==2)
 G$DMar<-as.numeric(G$Month==3)
 G$DApr<-as.numeric(G$Month==4)
 G$DMay<-as.numeric(G$Month==5)
 G$DJun<-as.numeric(G$Month==6)
 G$DJul<-as.numeric(G$Month==7)
 G$DAug<-as.numeric(G$Month==8)
 G$DSep<-as.numeric(G$Month==9)
 G$DOct<-as.numeric(G$Month==10)
 G$DNov<-as.numeric(G$Month==11)
 G$DDec<-as.numeric(G$Month==12)

G$y1 <-  G$Strip/1000000
lm1 <- lm(y1~t+tsqr+DFeb+ DMar+ DApr+ DMay+ DJun+ DJul+ DAug+ DSep+ DOct+ DNov+ DDec, data=G)
summary(lm1)
lm2 <- lm(y1~t+tsqr+DFeb+DApr+DJun+ DJul+ DAug+DNov+ DDec, data=G)
summary(lm2)
lm3 <- lm(y1~t+tsqr+DApr+DJun+ DJul+ DAug+DNov, data=G)
summary(lm3)
vif(lm1)
lm4 <- lm(y1~t+tsqr+DApr+DJun, data=G)
summary(lm4)
library(car)
vif(lm4)

###### Since this data set is a time series, we must look at 
# autocorrelation among residuals.
# Autocorrelation = correlation of a variable with itself 
# ac(lag=h) = correlation(Y(t),Y(t-h))

# Partial Autocorrelation at lag h = correlation of Y(t) and Y(t-h) 
# after removing effect of Y(1), ..., Y(t-h+1)

layout(1:2)
acf(lm4$residuals)
pacf(lm4$residuals)

X <- cbind(G$t,G$tsqr,G$DApr,G$DJun)

ts1 <- arima(G$y1, order=c(3,0,0), xreg=X)
print(ts1)

layout(1:2)
acf(ts1$residuals)
pacf(ts1$residuals)

# To decide on ARIMA(p,0,q) terms, look at both acf and pacf plots.
# one of these will quickly reduce to 0, and the other will cut-off
# after first few 

# Test if residuals from the final TS-REG model are not auto-correlated
# using the LjungBox test
# The LB-test is applied to the residuals of a time series #
# after fitting an ARMA(p,q) model to the data. The test #
# examines m autocorrelations of the residuals. If the #
# autocorrelations are very small, we conclude that the model #
# does not exhibit significant lack of fit.
Pvalue <- vector()
for (i in 1:10)
{
bt <- Box.test (ts1$residuals, lag = i, type="Ljung")
print(bt[3])


Pvalue[i] <- bt[3]
}
#Pvalue is a list object in R, so unlist it
Pvalue <- unlist(Pvalue)
plot(1:10,Pvalue,ylim=c(0,1))
abline(h = 0.05,col = "red")

# since all P-values to lag 10 are > .05, conclude that 
# the TS-REG residuals are not autocorrelated; this completes
# the TS-REG modeling exercise.


#ARMA(2) Model
n=1000
w=rnorm(n)
x = filter(w, filter=c(1.5,-.75), method="recursive")
plot.ts(x, main="autoregression")

ar= c(1.5,-0.75)
ma=0
lags=20
plot(ARMAacf(ar,ma,lags),type="h")
abline(h=0)
plot(ARMAacf(ar,ma,lags,pacf=TRUE),type="h")
abline(h=0)
x=arima.sim(list(ar=ar,ma=ma),n=1000)
acf(x)
pacf(x)

#MA(q) Model
n=1000
w=rnorm(n)
#fix this shit
#Xt=wt-3/2Wt-1+3/4Wt-2
x = filter(w, sides=1, filter=ma) # moving average
plot.ts(x, main="autoregression")

ma= c(-1.5,0.75)
ar=0
lags=20
#Theoretical ACF
plot(ARMAacf(ar,ma,lags),type="h")
abline(h=0)
#Theoretical PACF
plot(ARMAacf(ar,ma,lags,pacf=TRUE),type="h")
abline(h=0)
x=arima.sim(list(ar=ar,ma=ma),n=1000)
#Sample ACF
acf(x)
pacf(x)


#Exercise 3.4 (ACF and PACF)
par(mfrow=c(1,1))
plot(rec)

acf(rec)
pacf(rec)

trend=time(rec)
x=arima(rec,order=c(2,0,0),xreg=trend)
fit=lm(x~trend,data=rec)
summary(fit)

month=factor(cycle(rec))
month

###3/9/2017
n=100
x=arima.sim(list(order=c(0,1,1),ma=-0.8),n=100)
lambda=0.95
xs=c(x[1],rep(0,99))

for(i in 2:n){
  xs[i]=lambda*xs[i-1]+(1-lambda)*x[i]
}

plot(x)
lines(xs,col=2,lwd=2)


##Exercise 3.5 (GNP)
plot.ts(gnp)

plot.ts(log(gnp))

log.gnp=log(gnp)
z=log(diff(gnp))
z=diff(log.gnp)
plot.ts(z)
gnp
acf(z)
pacf(z)

trend=time(gnp)
xreg=trend
x=arima.sim(log.gnp,order=c(1,0,0),xreg=trend)
x=arima(rec,order=c(2,0,0),xreg=trend)
fit=lm(x~trend,data=rec)
summary(fit)


fit=numeric(16)
fit01=arma(log.gnp,order=c(p,0,q))
fit=as.list(1:16)


for(i in 1:length(fit)){
  for (p in 1:4){
    for (q in 1:4){
      fit[[i]]=arima(z,order=c(p,0,q))
   }
  }  
}

fit01.Box=Box.test (fit01$residuals, lag = i, type="Box-Pierce")



library(astsa)

mort=cbind(cmort,tempr,part)
pairs(mort)

trend=time(cmort)
temp=tempr-mean(tempr)
temp2=temp^2
fit=lm(cmort~temp+temp2+part+trend)
a=resid(fit)
summary(fit)
pacf(a)
acf(a)

sarima(cmort,2,0,0,xreg=temp+temp2+part+trend)



x = sarima.Sim(n=1000, period=12, model = list(order = c(1,0,0), ar=0.8),list(order= c(1,0,0), ar = 0.8))
plot(x)
acf(x)
pacf(x)


(fit01 <- sarima(x, 0, 0, 0,   1, 0, 0,   12, details=FALSE ))
(fit02 <- sarima(x, 0, 0, 0,   0, 0, 1,   12, details=FALSE ))
(fit03 <- sarima(x, 0, 0, 0,   1, 0, 1,   12, details=FALSE ))

(fit04 <- sarima(x, 1, 0, 0,   0, 0, 0,   12, details=FALSE ))
(fit05 <- sarima(x, 1, 0, 0,   1, 0, 0,   12, details=FALSE ))
(fit06 <- sarima(x, 1, 0, 0,   0, 0, 1,   12, details=FALSE ))
(fit07 <- sarima(x, 1, 0, 0,   1, 0, 1,   12, details=FALSE ))

(fit08 <- sarima(x, 1, 0, 1,   0, 0, 0,   12, details=FALSE ))
(fit09 <- sarima(x, 1, 0, 1,   1, 0, 0,   12, details=FALSE ))
(fit10 <- sarima(x, 1, 0, 1,   0, 0, 1,   12, details=FALSE ))
(fit11 <- sarima(x, 1, 0, 1,   1, 0, 1,   12, details=FALSE ))

mdl = c( "fit01", "fit02", "fit03", "fit04", "fit05", "fit06", "fit07", "fit08", "fit09", "fit10", "fit11")
aic = c( fit01$AIC, fit02$AIC, fit03$AIC, fit04$AIC, fit05$AIC, fit06$AIC, fit07$AIC, fit08$AIC, fit09$AIC, fit10$AIC, fit11$AIC )
bic = c( fit01$BIC, fit02$BIC, fit03$BIC, fit04$BIC, fit05$BIC, fit06$BIC, fit07$BIC, fit08$BIC, fit09$BIC, fit10$BIC, fit11$BIC )
kable( cbind(mdl,aic,bic) )

plot(AirPassengers)
trend=time(AirPassengers)

#Define x's
x = AirPassengers
log.x = log(x)
dlog.x = diff(log.x) 
ddlog.x = diff(dlog.x, 12)

fit=lm(log.x~trend)
summary(fit)



fit.diff=lm(dlog.x~trend)
summary(fit.diff)

fit.diff12=lm(ddlog.x~trend)
summary(fit.diff12)

plot.ts(cbind(x,log.x,dlog.x,ddlog.x), main="")
# below of interest for showing seasonal RW (not shown here):
par(mfrow=c(2,1))
monthplot(dlog.x); monthplot(ddlog.x)

sarima.fit=as.list(1:30)
for(i in 1:length(sarima.fit)){
  for(p in 0:1){
    for(d in 0:1)
      for(q in 0:1){
        for(p1 in 0:1){
          for(d1 in 0:1){
            for(q1 in 0:1){
              sarima.fit[[i]]=sarima(ddlog.x, p,d,q, p1,d1,q1,12)
              }
           }
        }
     }
  }
}

sarima.fit[[1]]$AIC

sarima.fit
sarima(log.x, 1,1,1, 0,1,1,12)

box.p=numeric(20)
for (i in 1:20){
  bt <- Box.test (fit$residuals, lag = i,  type="Box-Pierce")
  print("Lag = ")
  print(i)
  box.p[i] <- (bt$p.value)
}

box.p




#Garch Modeling Th 3/16
#######################################################################################################
#######################################################################################################


gspc=read.csv("GSPC20.csv",header=TRUE)
order(gspc$Date,decreasing=FALSE)
gspc

#1
#######################################################################################################

x=gspc$Close
x=rev(x)
y=ts(x,start=1997,end=2016)
plot(y)

#2
#######################################################################################################

log.y=ts(log(y),start=1997,end=2016)

plot(log.y)
fit=lm(log.y~time(log.y))
summary(fit)

ts.plot(log.y,fitted(fit),col=c(1,2))

plot(resid(fit), type="l", main="residuals")

#3
#######################################################################################################
#######################################################################################################

diff.log.y=diff(log.y)
plot(diff.log.y)


#Seasonality

acf(diff.log.y)
pacf(diff.log.y)

#4
#######################################################################################################
#######################################################################################################

acf(y^2)

#Volatility clustering: larger changes followed by larger changes
fit=lm(diff.log.y~time(diff.log.y))
summary(fit)
qqnorm(resid(fit))
shapiro.test(resid(fit))


