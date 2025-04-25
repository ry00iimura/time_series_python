#線形回帰分析
library(MASS)

#回帰分析
setwd("?/practical-stats-modeling/")

################################################
## Chapter1. Simple Linear regression
################################################

#まず使用するデータは住宅の成約データです。

house_data<-read.csv("data/kc_house_data.csv")
head(house_data)

#変数の説明
## id	いわゆるID
## date	販売日
## price（被説明変数）	価格($)
## bedrooms	ベッドルームの数
## bathrooms	バスルームの数
## sqft_living	リビングルームの広さ
## sqft_lot	駐車場の広さ
## floors	階数
## waterfront	ウォーターフロントか(0,1)
## view	ビューの種類(0,1,2,3,4)
## condition	状態（1,2,3,4,5)
## grade	グレード(1?13)
## sqft_above	地上階の広さ
## sqft_basement	地下の広さ
## yr_built	建築年
## yr_renovated	リノベーションの年
## zipcode	郵便番号
## lat	緯度
## long	経度
## sqft_living15	近所15件の平均のリビングの広さ
## sqft_lot15	近所15件の駐車場の広さ

#演習1
#まずはシンプルにsqft_aboveを説明変数、priceを被説明変数として線形回帰モデルをやってみましょう。
#解釈をお願いします
house_lm1<-lm(price?sqft_above, data=house_data)
house_lm1
summary(house_lm1)

#残差の正規性のチェック
## ヒストグラム
hist(house_lm1$residuals)

## qqプロット
qqnorm(house_lm1$residuals)
qqline(house_lm1$residuals, col="red")


#演習2:ひとつ説明変数を選んで価格を説明する


################################################
## Chapter2. Multi-variavle linear regression
################################################

#カテゴリ変数はas.factor（因子型）に変換する
## すると、勝手にダミー変数にしてくれる
house_data$waterfront<-as.factor(house_data$waterfront)
house_data$view<-as.factor(house_data$view)
house_data$condition<-as.factor(house_data$condition)
house_data$grade<-as.factor(house_data$grade)



#演習1
#以下の説明変数を使って重回帰分析をしてください
house_lm2<-lm(price?.-id-date-zipcode-lat-long-yr_built-yr_renovated, data=house_data)
AIC(house_lm2)
summary(house_lm2)




################################################
## Chapter3. Better Models
################################################
house_lm2<-lm(price?.-id-date-zipcode-lat-long-yr_built-yr_renovated, data=house_data)
house_lm3<-step(house_lm2)
summary(house_lm3)
AIC(house_lm3)


#####################
## 解釈の観点
#####################

#チェック1: 正規性のチェック
hist(house_lm3$residuals)
qqnorm(house_lm3$residuals)
qqline(house_lm3$residuals, col="red")


#対処法：対数変換
#セミ対数モデル
house_lm_semilog<-lm(log(price)?bedrooms + bathrooms + sqft_living + 
                waterfront + view + condition + grade + sqft_above +sqft_living15 + sqft_lot15, data=house_data)
summary(house_lm_semilog)
hist(house_lm_semilog$residuals)
qqnorm(house_lm_semilog$residuals)
qqline(house_lm_semilog$residuals, col="red")

#本当にそうでしょうか？常識と照らして納得感がありますか？

#チェック2: 外れ値: Cook's distance
ck_dist<-cooks.distance(house_lm3)
4/length(ck_dist)
max(ck_dist, na.rm = TRUE)
ck_dist[ck_dist ==max(ck_dist, na.rm = TRUE)]
plot(house_lm3)


#対処法: 外れ値を除外する
house_data_wip<-house_data
house_data_wip<-house_data[-c(7253, 3915, 9255, 19453, 4412), ]
house_lm_wip<-lm(price?bedrooms + bathrooms + sqft_living + floors + 
                   waterfront + view + condition + grade + sqft_above + yr_built + 
                   yr_renovated + sqft_living15 + sqft_lot15, data=house_data_wip)
ck_dist<-cooks.distance(house_lm_wip)
ck_dist[ck_dist == max(ck_dist)]
summary(house_lm_wip)
plot(house_lm_wip)

#チェック3: 多重共線性
library(car)
vif(house_lm3)

house_lm4<-lm(price ? bedrooms + bathrooms + floors + 
                waterfront + view + condition + grade + sqft_above +　sqft_living15 + sqft_lot15, data=house_data)
vif(house_lm4)

house_lm5<-lm(price ? bedrooms + bathrooms + floors + 
                waterfront + view + condition +sqft_living15 + sqft_lot15, data=house_data)
vif(house_lm5)

house_lm6<-lm(price ? bedrooms +floors + waterfront + view + condition +sqft_living15 + sqft_lot15,
              data=house_data)
vif(house_lm6)
summary(house_lm6)

#####################
## 説明力の観点
#####################
summary(house_lm6)

house_full<-lm(price ? bedrooms +floors + waterfront + view + condition +sqft_living15 + sqft_lot15, data=house_data)

#sqft_lot15を除いてみましょう

house_reduced<-lm(price ? bedrooms +floors + waterfront + view + condition +sqft_living15, data=house_data)
summary(house_reduced)
anova(house_reduced, house_full)

AIC(house_full)
AIC(house_reduced)

#conditionを除いてみましょう

house_reduced<-lm(price ? bedrooms +floors + waterfront + view + sqft_living15 + sqft_lot15, data=house_data)
summary(house_reduced)
anova(house_reduced, house_full)

#やっぱりフルモデルが良い

################################################
## Chapter4. 予測
################################################

#住宅価格のデータで予測
house_lm1<-lm(price?sqft_above, data=house_data)
house_lm1

plot(house_data$sqft_above, house_data$price)
abline(house_lm1, col="red")

new_data<-data.frame(sqft_above=c(2000,4000,6000,8000))
new_data

## 回帰直線上の値を求める場合
predict(house_lm1, newdata = new_data)

## 信頼区間も含めて求める場合
predict(house_lm1, newdata = new_data, interval = "confidence")

## 信頼区間も含めて求める場合
predict(house_lm1, newdata = new_data, interval = "predict")

#可視化してみましょう
plot(house_data$sqft_above, house_data$price, xlim=c(-1000, 12000), ylim=c(-1000,4e+6))
abline(house_lm1, col="red")

sqft_above_seq<-c(-1000:12000)
new_data2<-data.frame(sqft_above=sqft_above_seq)
conf_interval<-predict(house_lm1, newdata = new_data2, interval = "confidence")
lines(sqft_above_seq,conf_interval[,2],col="blue",lty=2)
lines(sqft_above_seq,conf_interval[,3],col="blue",lty=2)

pred_interval<-predict(house_lm1, newdata = new_data2, interval = "prediction")
lines(sqft_above_seq,pred_interval[,2],col="green",lty=2)
lines(sqft_above_seq,pred_interval[,3],col="green",lty=2)

############################################################################
## （参考）機械学習的アプローチ 学習データとテストデータに分割して精度検証
############################################################################

#学習データとテストデータに分割します
train_idx<-sample(c(1:dim(house_data)[1]), size = dim(house_data)[1]*0.7)
train <- house_data[train_idx, ]
test <- house_data[-train_idx, ]

mymodel<-lm(price ? bedrooms + floors + waterfront + view + 
                 condition + yr_built + yr_renovated + sqft_living15 + sqft_lot15, data=train)

ypred<-predict(mymodel, newdata = test)
mse<-sum((test$price - ypred)^2)/length(ypred)
mse
rmse<-sqrt(mse)
rmse
