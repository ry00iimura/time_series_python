#データの読み込み
train<-read.csv("train.csv")
test<-read.csv("test.csv")
features<-read.csv("features.csv")
stores<-read.csv("stores.csv")
sampleSubmission<-read.csv("sampleSubmission.csv")

# store1だけを抽出する
library(sqldf)
store1_data<-sqldf("SELECT * FROM train WHERE Store=1")

# データを縦持ちから横持ちにする
library("reshape2")
store1_data2<-dcast(store1_data,Date?Dept,value.var="Weekly_Sales",mean)

# 列ごと(Deptごと)に平均を計算する
store1_data2_sum<-apply(store1_data2[,2:dim(store1_data2)[2]], 2, FUN=mean)

# 50000以上のDeptだけにする
store1_data2_sum<-as.data.frame(store1_data2_sum)
names(store1_data2_sum)<-"AVEG_Sales"
store1_data2_sum$Dept<-row.names(store1_data2_sum)
store1_data2_sum<-store1_data2_sum[is.na(store1_data2_sum$AVEG_Sales)==FALSE,]
store1_data2_sum<-store1_data2_sum[order(store1_data2_sum$AVEG_Sales, decreasing=T),]
target_dept<-store1_data2_sum[store1_data2_sum$AVEG_Sales>50000, "Dept"]

# VARモデルに投入するようのデータにする
var_data<-store1_data2[,target_dept]

# ACF, PACF
acf(var_data$`92`)
acf(var_data$`95`)

# VARモデルの適用
VARselect(var_data, 
          lag.max = 4, 
          type="both"
          )
store1_var<-VAR(var_data3, 
                p=4, 
                type="both"
                )

#インパルス応答関数
store1_var_irf<-irf(store1_var,n.ahead=30,ci=0.95) 
plot(store1_var_irf)


