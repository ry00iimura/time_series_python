#Time series analysis

install.packages("tseries")
install.packages("forecast")

library(tseries)
library(forecast)

### Q1 demonstrate  White noise = random nomal curve 
## hensuu = z
z<-rnorm(200, 0, 1)
plot(z, type="l")

## Q2: ACF  & ÆPACF Z= white noi?e
acf(z)
pacf(z)

# Q3: Box.test
Box.test(z)

## AR model and MA model

### example: y_{t} = 0.2*y_{t-1} + 0.3*y_{t-2} + 0.3*y_{t-3} + e_{t} AR
# 400 samples order=c(AR,I,MA), until 3I , this model will be set
#ar = c(1,2,3),regression coeffient
y1<-arima.?im(n=400, list(order=c(3,0,0),
                     ar=c(1.2, 0.3, 0.3)))
par(mfrow=c(1,3))
plot(y1, type="l")
acf(y1)
pacf(y1)

### example: y_{t} = 0.2* e_{t-1} + 0.4* e_{?-2} +  + 0.5* e_{t-2} + e_{t} MA
y2<-arima.sim(n=400, list(order=c(0,0?3),
       ?                  ma=c(0.9, 0.9, 0.9)))
plot(y2,type="l")
par(mfrow=c(1,3))
plot(y2, type="l")
acf(y2)
pacf(y2)

### ‰‰K1: 
#### arima.simŠÖ”‚ğg‚Á‚ÄAR‰ß’?AMA‰ß’ö‚Ìƒf[ƒ^‚ğì¬‚µA
#### acfŠÖ”ApacfŠÖ”‚ğg‚Á‚ÄŒXŒü‚ğŠm”F‚µ‚Ü‚µ‚å?¤B
y3<-ari?a.sim(n=400? list(order=c(1,0,1),
                          ar= c(0.2),
                          ma= c(0.2)))
plot(?3, type="l")
par(mfrow=c(1,3))

acf(y3)
pacf(y3)



### ‰‰K2: ARMA‰ß’ö‚É]‚¤ƒf[ƒ^‚ğì¬‚µAacfŠÖ”ApacfŠÖ”‚ğ•`‚¢‚Ä‚İ‚Ü‚µ‚å‚¤ 

## trend?and tanikon?### rondom walk
set.seed(1234)
#400 sample are accumul?ted 
y<-cumsum(rnorm(400, 0, 1))
plot(y, type="l")

### 2:Unit root test
PP.test(y)

#????????????????????????????????????
PP.test(diff(y))

#????????????????????????????????????????????????????????????????????????????????????????????????????????????(?????????????????????)
#??????????????????????????????????????????????????????
#???????????????????????????????????????????????????????????????????????????Google???????????????????????????????????????
set.se?d(1235)
y1<-cumsum(rnorm(400, 0, 1))
plot(y1, type="l")
summary(lm(y1 ~ y))

### Q3 Produce a process of ARIMA
y3<-arima.sim(n=400, list(order=c(1,1,1),
                      ar=c(0.4),
                      ma=c(0.2)))
plot(y3, type="l")
#???????????????
?P.test(y3)
#???????????????
y4<-diff(y3,lag = 1)
PP.test(y4)
plot(y4, type="l")
#????????????????????????????????????(???????????????????????????????????????????????????)
Box.test(y4)
acf(y4)
pacf(y4)

library(forecast)
mymodel<-auto.arima(y3, 
           ?ax.p = 3,
           max.d = 1,
           max.q = 2, ?           trace=T, 
           stepwise = T)
#level??????????????????h??????
myforecast<-forecast(mymodel, level = c(50, 95), h = 30)
plot(myforecast)

####Q4 SARIMA and assumption through ARIMA
## SA?IMA
### demonstrate SARIMA

#### read CSV*consumption gasoline monthly
read.csv("data/gas_train.csv",header=T) -> train
read.csv("data/gas_test.csv" ,header=T) -> test
head(train)
# ploy by Time series
plot.ts(train$gasoline)

#the number 12 is desgnied by?human
train.ts<-ts(train$gasoline, frequency = 12)

#decompostion elements
library(stats)
ts.stl<-stl(train.ts, s.window = "periodic")
plot(ts.stl)

# diffrencing
train.diff<-diff(train.ts, 1)
PP.test(train.diff)

#ACF and PACF
acf(train.diff)
pacf(train.t?)
#self-correlation - that is no longer do modeling because of rejecting the null hyphothasis
Box.test(train.diff)

#model assumption 
library(forecast)
mymodel<-auto.arima(train.ts, 
                    max.p = 6,
                    max.q=6,
            ?       max.d=1,
                    seasonal = T,
                   stepwise = T,
                    trace = T)
plot(forecast(mymodel, level=c(0.5,0.9),
         h=12))


# case study1 predict the rate of turnover 
read.csv("data/unemploy_train.csv",head?r=T) -> dt
#check the seasonality
ts.stl<-stl(train.ts, s.window = "periodic")
ts.stl
plot(ts.stl)
plot.ts(dt$rate)

t.ts<-ts(dt$rate, frequency = 12)

#check unit root process
library(tseries)
PP.test(t.ts)

diff_rate <- diff(t.ts,2)
diff_rate


Box.test(?iff_rate,type = 'L')

mymodel<-auto.arima(t.ts, 
                    max.p = 6,
                    max.q=6,
                    max.d=2,
                    seasonal = T,
                    stepwise = T,
                    trace = T)
plot(forecast(mymod?l, level=c(0.5,0.9),
              h=12))





#ARIMA--------------------------------------------------------------------------------------------------------------------------
#read csv--------------------------------------------
read.csv("data/train.csv",?eader=T) -> stores_features
store_one ->stores_features[stores_features$Store==1,]


#plot
plot.ts(store_one$Weekly_Sales)

library(forecast)

train.ts<-ts(store_one$Weekly_Sales, frequency = 12)

#check the seasonality
ts.stl<-stl(store_one$Weekly_Sales, ?.window = "periodic")
ts.stl
plot(ts.stl)


#PP.text
PP.test(store_one$Weekly_Sales)

#differencing
#train.diff<-diff(store_one$Weekly_Sales,1)
#plot.ts(train.diff[1:48])
#PP.test(train.diff)

#acf, pacf
acf(store_one$Weekly_Sales)
pacf(store_one$Weekly_Sa?es)
Box.test(store_one$Weekly_Sales, lag = 12)

library(tseries)

#train
train.ts<-ts(store_one$Weekly_Sales, 
             frequency = 12)

#find the best parameters
mymodel<-auto.arima(train.ts, 
                    max.p = 12,
                    max.q ? 1,
                    max.d = 12,
                    seasonal = T,
                    stepwise = T,
                    trace= T)
mymodel
#predict
ypred<-forecast (mymodel, 
                 level = c( 50 , 95 ) ,
                 h = 24 )


#`check it? residual`
sum((ypred$mean-test_data$rate)^2)/24

#visualizing
plot(ypred)


#ARIMA
y1<-arima.sim(n=400, list(order=c(3,0,0),
                          ar=c(0.2, 0.3, 0.3)))
par(mfrow=c(1,3))
plot(y1, type="l")
acf(y1)
pacf(y1)



#var---------------------?--------------------------------------------------------------------
store_one$diffs <- diff(store_one$Weekly_Sales)[0:10242]

VARselect(store_one,lag.max=4) -> varsel #automatically culculate jisu AIU(n) n is jisu

# SELECT JISUU of auc
varsel$selection[1? -> P

# create VAR model
VAR(store_one,p=P) -> Cnd.var

summary(Cnd.var)

plot(Cnd.var)


# predict 3years later n.aded is the number of seasonality , ci is trusted interval.
predict(Cnd.var, n.ahead=12, ci=0.95)


