#install.packages("tseries")
#install.packages("lmtest")
library(tseries)
library(lmtest)


bike_data<-read.csv("data/bikesharing_hour.csv")
head(bike_data)

bike_data$season<-as.factor(bike_data$season)
bike_data$yr<-as.factor(bike_data$yr)
bike_data$weathersit<-as.factor(bike_data$weathersit)
bike_data$mnth<-as.factor(bike_data$mnth)
bike_data$hr<-as.factor(bike_data$hr)
bike_data$holiday<-as.factor(bike_data$holiday)
bike_data$weekday<-as.factor(bike_data$weekday)
bike_data$workingday<-as.factor(bike_data$workingday)

# 正規分布
bike_lm<-lm(casual~season+yr+weathersit+mnth+hr+holiday+
              weekday+workingday+
              windspeed+temp, data=bike_data)
summary(bike_lm)

#残差は正規分布か


#分散が不均一か
plot(bike_lm$residuals)
lmtest::bptest(bike_lm)

# 系列相関がないか
plot(bike_lm$residuals[1:100], type="l")
lmtest::bgtest(bike_lm,order =10)
