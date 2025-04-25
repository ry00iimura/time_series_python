# 失業率の予測
train_data<-read.csv("data/unemploy_train_UTF8.csv")
test_data<-read.csv("data/unemploy_test_UTF8.csv")

plot.ts(train_data$rate)

#単位根があるか？
PP.test(train_data$rate)

#単位根があるっぽいの?ﾅ、diffをとる
train.diff<-diff(train_data$ra?e,1)
plot.ts(train.diff[1:48])
PP.test(train.diff)
## どうやらトレンド(d=1)はあるみたい

acf(train.diff)
pacf(train.diff)
Box.test(train.diff, lag = 12)

train.ts<-ts(train_data?rate, 
             frequency = 12)

mymodel<-auto.arima(train.ts, 
          ?         max.p = 12,
                    max.q = 1,
                    max.d = 12,
                    seasonal = T,
                    stepwise = T,
                    trace= T)
mymodel
ypred<-forecast (mymodel, 
                 level = c( 50 , 95 ) ,?                 h = 24 ) 
#`check its residual`
sum((ypred$mean-test_data$rate)^2)/24
plot(ypred)