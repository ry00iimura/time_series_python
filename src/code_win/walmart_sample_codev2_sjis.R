library(randomForest)
library(ggplot2)

#ƒf[ƒ^‚Ì“Ç‚İ‚İ
train<-read.csv("train.csv")
test<-read.csv("test.csv")
features<-read.csv("features.csv")
stores<-read.csv("stores.csv")
sampleSubmission<-read.csv("sampleSubmission.csv")

#•Ö—˜ŠÖ”1: ƒf[ƒ^•ÏŠ·ŠÖ”
data_conversion<-function(samp){
  for (n in names(samp)){
    col<-samp[,n]
    na<-is.na(col)
    if (any(na)){
      if (is.numeric(col)){
        print(paste("*",n))
        samp[,paste(n,"NA",sep="_")]<-as.numeric(na)+1
        if(median(col,na.rm=T)<1){
          samp[na,n]<-1
        }else{
          samp[na,n]<-median(col,na.rm=T)          
        }
      }else{
        print(paste(".",n))
        samp[na,n] <-'*#NA,#'
      }
    }
    if (nlevels(col)>32){
      print(paste("32",n))
      samp[,n]<-unclass(col)
    }
    dt<-strptime(col,"%m/%d/%Y")
    if(!any(is.na(dt)))
    {
      print(paste("!",n))
      samp[,n] = as.double(dt)
      samp[,paste(n,"wday",sep="_")]=dt$wday
    }
  }
  return(samp)  
}

#Create Diff variables: “Á’¥—Ê‚Ç‚¤‚µ‚ğˆø‚«Z‚·‚éŠÖ”
create_diffFeatures<-function(hoge){
  d<-cor(hoge[,colnames(hoge)!="Month"],method="spearman")
  d_label<-colnames(d)
  for(i in 1:length(d_label)-1){
    k<-i+1
    for(j in k:length(d_label)){
      if(i!=0){
        tmp<-as.data.frame(hoge[,d_label[i]]-hoge[,d_label[j]])
        colnames(tmp)<-paste(d_label[i],d_label[j],sep="_diff_")
        hoge<-cbind(hoge,tmp)        
      }    
    }
  }
  return(hoge)  
}


#Set test data
Weekly_Sales<-NA
test<-cbind(test[,1:3],Weekly_Sales,test[,4])
colnames(test)<-c("Store","Dept","Date","Weekly_Sales","IsHoliday")
whole<-rbind(train,test)
whole$Date<-as.Date(whole$Date)
features$Date<-as.Date(features$Date)


#Data transformation: c‚¿‚ğ‰¡‚¿‚É‚·‚é
allStore<-unique(whole$Store)
allDept<-unique(whole$Dept)
res<-NULL
for(i in 1:length(allStore)){
  temp<-subset(whole,Store==allStore[i])
  for(j in 1:length(allDept)){
    if(j==1){
      temp_dat<-subset(temp,Dept==allDept[j])[,c(1,3,4)]
      colnames(temp_dat)[3]<-"Dept_1"
    }else{
      temp_dat2<-subset(temp,Dept==allDept[j])[,3:4]
      if(dim(temp_dat2)[1]==0){
        temp_dat<-cbind(temp_dat,NA)
      }else{
        temp_dat<-merge(temp_dat,temp_dat2,all.x=TRUE,by="Date")
      }
      a<-paste("Dept",allDept[j],sep="_")
      colnames(temp_dat)[dim(temp_dat)[2]]<-a
    }
  }
  res<-rbind(res,temp_dat)
}

#merge features
res$Key<-paste(res$Store,res$Date,sep="_")
features$Key<-paste(features$Store,features$Date,sep="_")
res<-merge(res,features[,-c(1,2)],all.x=TRUE,by="Key")
res$Year<-as.numeric(format(res$Date,"%Y"))
res$Month<-as.factor(format(res$Date,"%m"))
res$Week<-as.numeric(format(res$Date,"%d"))
res$IsHoliday<-as.numeric(res$IsHoliday)

#Train and Predict
submission_temp<-NULL
replicatedTest<-NULL
#length(allStore)
for(i in 1:length(allStore)){
  tmp_res<-subset(res,Store==allStore[i])
  hoge<-data_conversion(tmp_res[,85:97])
  hoge<-create_diffFeatures(hoge)
  tmp_res<-cbind(tmp_res[,1:84],hoge)
  tmp_train<-subset(tmp_res,Date<="2012-10-26")
  tmp_test<-subset(tmp_res,Date>"2012-10-26")
  ind_train<-tmp_train[,c(85:dim(tmp_train)[2])]
  ind_test<-tmp_test[,c(1,85:dim(tmp_train)[2])]
  
  for(j in 4:84){
    if(!all(is.na(tmp_train[,j]))){
      target<-tmp_train[,j]
      target[is.na(target)]<-0
      rfData<-cbind(target,ind_train)
      rfData2<-rfData
      rfData3<-rfData
      rfData4<-rfData
      
      #Check whether target variable is unique or not
      if(length(unique(target))==1){
        Weekly_Sales<-unique(target)
      }else{
        #Extract Important features
        fe<-randomForest(target?.,data=rfData,ntree=500)
        fe_imp<-importance(fe)
        a<-fe_imp> max(fe_imp)/100
        imp_features<-names(a[a[,1]==TRUE,])
        rfData<-rfData[,imp_features]
        rfData<-cbind(target,rfData)
        
        #Base Trend Model
        base_model<-randomForest(target?.,data=rfData,ntree=500)
        base_pred<-predict(base_model,rfData)
        tmp_train$pred<-base_pred
        p<-ggplot(tmp_train)+geom_line(aes(x=Date,y=tmp_train[,j]))+geom_line(aes(x=Date,y=pred),col="red")
        # print(p)
        #Spike model
        target2<-target - base_pred
        rfData2$target<-target2

        #For Spike Model, feature selection
        fe<-randomForest(target?.,data=rfData2,ntree=500)
        fe_imp<-importance(fe)
        a<-fe_imp> max(fe_imp)/100
        imp_features<-names(a[a[,1]==TRUE,])
        rfData2<-rfData2[,imp_features]
        rfData2$target<-target2

        spike_model<-randomForest(target?., data= rfData2,ntree=500)
        spike_pred<-predict(spike_model,rfData2)

        #For anothe residual(spike), anothe random forest
        target3<-target - base_pred -spike_pred
        rfData3$target<-target3
        fe<-randomForest(target?.,data=rfData3,ntree=500,nodesize=60)
        fe_imp<-importance(fe)
        a<-fe_imp> max(fe_imp)/100
        imp_features<-names(a[a[,1]==TRUE,])
        rfData3<-rfData3[,imp_features]
        rfData3$target<-target3

        spike_model2<-randomForest(target?., data= rfData3,ntree=500)
        spike_pred2<-predict(spike_model2,rfData3)

        residuals<-mean(abs(target-(base_pred+spike_pred+spike_pred2)))
        if(residuals>300){
          target4<-target - base_pred -spike_pred -spike_pred2
          rfData4$target<-target4
          fe<-randomForest(target?.,data=rfData4,ntree=500)
          fe_imp<-importance(fe)
          a<-fe_imp> max(fe_imp)/100
          imp_features<-names(a[a[,1]==TRUE,])
          rfData4<-rfData4[,imp_features]
          rfData4$target<-target4

          spike_model3<-randomForest(target?., data= rfData4,ntree=500)
          spike_pred3<-predict(spike_model3,rfData4)

          residuals<-mean(abs(target-(base_pred+spike_pred+spike_pred2 +spike_pred3)))
          print(residuals)
          tmp_train$pred<-base_pred+spike_pred+spike_pred2 +spike_pred3
          
          p<-ggplot(tmp_train)+geom_line(aes(x=Date,y=tmp_train[,j]))+geom_line(aes(x=Date,y=pred),col="red")
          print(p)
          
          #Prediction for test data
          base_pred_test<-predict(base_model,ind_test)
          spike_pred_test<-predict(spike_model,ind_test)
          spike2_pred_test<-predict(spike_model2,ind_test)
          spike3_pred_test<-predict(spike_model3,ind_test)

          Weekly_Sales<-base_pred_test+spike_pred_test+spike2_pred_test+spike3_pred_test
          
        }else{
          residuals<-mean(abs(target-(base_pred+spike_pred+spike_pred2)))
          print(residuals)
          tmp_train$pred<-base_pred+spike_pred+spike_pred2
          p<-ggplot(tmp_train)+geom_line(aes(x=Date,y=tmp_train[,j]))+geom_line(aes(x=Date,y=pred),col="red")
          print(p)

          #Prediction for test data
          base_pred_test<-predict(base_model,ind_test)
          spike_pred_test<-predict(spike_model,ind_test)
          spike2_pred_test<-predict(spike_model2,ind_test)
          
          Weekly_Sales<-base_pred_test+spike_pred_test+spike2_pred_test
        }
        
      }
      deptID<-strsplit(colnames(tmp_train)[j],split="_")[[1]][2]
      cat(paste(allStore[i],deptID,sep="_"),"?n")
      Id<-as.data.frame(paste(allStore[i],deptID,ind_test$Date,sep="_"))
      tmp_res<-cbind(Id,Weekly_Sales)
      colnames(tmp_res)<-c("Id","Weekly_Sales")
      submission_temp<-rbind(submission_temp,tmp_res)
      
      tmp_test[,j]<-Weekly_Sales
      
    }
  }
  replicatedTest<-rbind(replicatedTest,tmp_test)
}


submission<-read.csv("sampleSubmission.csv")
Order<-seq(1,dim(submission)[1])
submission<-cbind(Order,submission)
submission2<-merge(submission[,1:2],submission_temp,all.x=TRUE,by="Id")
submission2<-submission2[order(submission2[,2]),]
write.csv(submission2[,-2],"submission_20190129.csv",row.names=FALSE)



