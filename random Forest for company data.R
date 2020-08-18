View(c)
str(c) # In data set variables which data type
install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
library(randomForest)
### as per given problem firstly Dependent variable sales convert into categorical by using ifelse condition 
high1=ifelse(c$Sales<10,"no","yes")

### Creating the dummy variable for column Shelveloc ,Urban,US
library(dummies)
cmpdata.new1=dummy.data.frame(c)
dummy[ShelveLoc=="Bad"]==0
dummy[ShelveLoc=="Medium"]==1
dummy[ShelveLoc=="Good"]==2
dummy[Urban=="Yes"]==1
dummy[Urban=="No"]==0
dummy[US=="No"]==0
dummy[US=="Yes"]==1
View(cmpdata.new1)


## Clubing the both dependent and independent variable 
finaldata2=data.frame(cmpdata.new1,high1)
View(finaldata2)
finaldata3=finaldata2[-1]
View(finaldata3)

# recode high1 as a factor
finaldata3$high1=factor(finaldata3$high1)

# table or proportions with more informative labels
round(prop.table(table(finaldata3$high1)) * 100, digits = 1)
### In above code digit =1 means, in output in result after the point i want only 1 value so only digit =1

## in finaldata1 set independent variables have different units so we have to normalize 

# create normalization function (custom define normalization function)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the finaldata1
finaldata4_n <- as.data.frame(lapply(finaldata3[1:14], normalize))
View(finaldata4_n)


finaldata4_n["high1"] <- finaldata3$high1

# Building a random forest model on training data 

c_forest <- randomForest(finaldata3$high1~.,data=finaldata4_n,importance=TRUE, ntree=500)
??randomForest
plot(c_forest)
legend("topright",colnames(c_forest$err.rate),col=1:3,cex=0.8,fill=1:3)

acc_c <- mean(finaldata4_n$high1==predict(c_forest))
acc_c
varImpPlot(c_forest)
 ## Accuracy of the model is 87.75



## Clubing the both normalized data and independent variable 
f=data.frame(finaldata4_n,high1)
View(f)
f1=f[-15]
View(f1)

### Bagging ####
### Model buliding the 101 times ##

library(C50)
acc<-c()
for(i in 1:101)
{
  print(i)
  inTraininglocal<-createDataPartition(f1$high1.1,p=.80,list=F)
  training1<-f1[inTraininglocal,]
  testing<-f1[-inTraininglocal,]
  
  fittree<-C5.0(training1$high1.1~.,data=training1[,-15])
  pred<-predict.C5.0(fittree,testing[,-15])
  a<-table(testing$high1.1,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
mean(acc)
##### Accuracy of an accuracy is 83.38% which is bagging technique ,
### In bagging technique models accuracy calculated  in parallel manner 


 ####  Adaptive Boosting technique  ###

### Model building the 102 times 
acc<-c()
for(i in 1:102)
{

  inTraininglocal<-createDataPartition(f1$high1.1,p=.85,list=F)
  training1<-f1[inTraininglocal,]
  testing<-f1[-inTraininglocal,]
  
  fittree<-C5.0(training1$high1.1~.,data=training1[,-15],trials=15) ### trails = 15 nothing but  Adaptable boosting Algorithm
  pred<-predict.C5.0(fittree,testing[,-15])
  a<-table(testing$high1.1,pred)
  a
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
acc 
mean(acc)  ### Boosting technique the accuracy is 88.77%
 ### In Boosting technique Accuracy calculated based on weighted and sequential manner


