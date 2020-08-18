# Using Random Forest
install.packages("randomForest")
library(randomForest)
View(fraudcheck)
str(fraudcheck)
###  ifelse condtion applying  if taxable income <=30000 Risky ,else good 
Risky_good=ifelse(fraudcheck$Taxable.Income<=30000,"Risky","good")


### Creating the dummy variables for Undergrand,Marital status ,urban columns 
library(dummies)
fcheck.new=dummy.data.frame(fraudcheck)
dummy[Undergrad=="Yes"]=1
dummy[Undergrad=="No"]=0
dummy[Marital.Status=="Single"]=0
dummy[Marital.Status=="Divorced"]=1
dummy[Marital.Status=="Married"]=2
dummy[Urban=="Yes"]=1
dummy[Urban=="No"]=0
View(fcheck.new)
fc=fcheck.new[-6]## Deleting the taxable.income column  bocz we applied ifelse condition to that column 
### and we considering the taxable.income  is dependent variable  ,if <=30000 risky ,otherwise good
View(fc)
View(Risky_good)

## Clubing the both dependent and independent variable 
finaldata=data.frame(fc,Risky_good)
View(finaldata)

# table of diagnasis goog-->476 & Risky-->124
table(finaldata$Risky_good)

# table or proportation of enteries in the datasets. What % of entry is good and % of entry is Risky
round(prop.table(table(finaldata$Risky_good))*100,1)

#Create a function to normalize the data(Custom define normalization function)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#Apply the normalization function to wbcd dataset
finaldata_n <- as.data.frame(lapply(finaldata[1:9], norm))
View(finaldata_n)

finaldata_n["Risky_good"] <- finaldata$Risky_good

# Building a random forest model on training data 
fraudcheck_forest <- randomForest(Risky_good~.,data=finaldata_n,importance=TRUE, ntree=900)
??randomForest
plot(fraudcheck_forest)
legend("topright",colnames(fraudcheck_forest$err.rate),col=1:3,cex=0.8,fill=1:3)

acc_fraudcheck <- mean(finaldata_n$Risky_good==predict(fraudcheck_forest))
acc_fraudcheck 
varImpPlot(fraudcheck_forest) 
 ### Accuracy of our model is 79.33% 



















# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
finaldata_Risky<-finaldata[finaldata$Risky_good=="Risky",] # 124
finaldata_good <- finaldata[finaldata$Risky_good=="good",]# 476


finaldata_train <- rbind(finaldata_Risky[1:62,],finaldata_good[1:238,])
finaldata_test <- rbind(finaldata_Risky[63:124,],finaldata_good[239:476,])
# Building a random forest model on training data 
fit.forest <- randomForest(Risky_good~.,data=finaldata_train, na.action=na.roughfix,importance=TRUE, ntree=1000)
# ntree is nothing but the no.of decision trees and if we will not give default it will take 500
# Training accuracy 
mean(finaldata_train$Risky_good==predict(fit.forest,finaldata_train)) # 86.33% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,finaldata_train)
library(caret)
# Confusion Matrix
confusionMatrix(finaldata_train$Risky_good, pred_train) ## 86.67 Accuracy


# Predicting test data 
pred_test <- predict(fit.forest,newdata=finaldata_test)
mean(pred_test==finaldata_test$Risky_good) # Accuracy = 79.40% Accuracy 


# Confusion Matrix 
library(caret)
confusionMatrix(finaldata_test$Risky_good, pred_test) ## 79.33% accuracy 

## Conclusion :  In my prediction good is good predicted as 238 times but risky is good predicted as 40 times 
### So this is dangerious so only accuracy 79% so in order to improve accuracy we can use ensembled methods

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)





###### Note: for improving the accuarcy of my model i used the the ensembled method Bagging 
###Bagging####

## Clubing the both dependent and independent variable 
finaldata1=data.frame(finaldata_n,Risky_good)
View(finaldata1)
final=finaldata1[-10]
View(final)
library(C50)
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(final$Risky_good.1,p=.80,list=F)
  training1<-final[inTraininglocal,]
  testing<-final[-inTraininglocal,]
  
  fittree<-C5.0(training1$Risky_good.1~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-10])
  a<-table(testing$Risky_good.1,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
acc
View(acc)

### hence the bagging technique also we are nearly getting the same accuracy  which is 79.78 % 



### another ensembled method is Adaptive Bossting 
## Model building the 102 times 
acc<-c()
for(i in 1:102)
{
  
  inTraininglocal<-createDataPartition(final$Risky_good.1,p=.85,list=F)
  training1<-final[inTraininglocal,]
  testing<-final[-inTraininglocal,]
  
  fittree<-C5.0(training1$Risky_good.1~.,data=training1[,-10],trials=30) ### trails = 15 nothing but  Adaptable boosting Algorithm
  pred<-predict.C5.0(fittree,testing[,-10])
  a<-table(testing$Risky_good.1,pred)
  a
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
acc 
mean(acc) 
 ############ Hence Adaptive Boossting technique also gave 79.78 % Accuracy only 