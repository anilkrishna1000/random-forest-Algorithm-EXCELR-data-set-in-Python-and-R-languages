install.packages("xgboost")
install.packages("methods")
require(xgboost)
require(methods)
library(xgboost)
library(methods)
library(caret)
library(Matrix)
library(dplyr)

View(datagbm)

Sales=ifelse(datagbm$Sales<10,"1","0")
str(Sales)
Sales <- as.numeric(Sales)
str(Sales)
high1=cbind(Sales,datagbm)
high1
high1=high1[-2]
high1
x <- factor(high1$Urban, levels = c("Yes", "No"), labels = c(1, 0))
x <- as.numeric(x)
class(x)
y <- factor(high1$US, levels = c("Yes", "No"), labels = c(1, 0))
y
y <- as.numeric(y)

z<- factor(high1$ShelveLoc, levels = c("Bad", "Good", "Medium"), labels = c(-1, 1,0))
z <- as.numeric(z)
high2=cbind(high1,x,y,z)
View(high2)
newdata <- high2[c(-7,-10,-11)]
colnames(newdata)
names(newdata)[9] <- "Urban"
names(newdata)[10]<- "US"
names(newdata)[11]<- "ShelveLoc"
colnames(newdata)
library(caret)
datagbm1 <- newdata
View(datagbm1)

mean<-c()
for(i in 1:12){
  
  inTraining <- createDataPartition(datagbm1$Sales, p = .8, list = FALSE)
  training <- datagbm1[inTraining,]
  testing  <- datagbm1[-inTraining,]
  
  labeltraining = as.numeric(training[[1]])
  datatraining = as.matrix(training[2:11])
  
  labeltesting  = as.numeric(testing [[1]])
  datatesting  = as.matrix(testing [2:11])
  
  xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)
  
  xgtesting <- xgb.DMatrix(data=datatesting, label = labeltesting)
  
  param = list("objective" = "binary:hinge","n_class"=10, "bst:eta" = 1,"bst:max_depth" = 7,"nthread" = 4,"gamma" =1,"min_child_weight" = 3)
  
  model = xgboost(params = param, data = xgtraining, nround = 100)#,subsample = 0.8,colsample_bytree = 0.8)
  
  
  ypred = predict(model, xgtesting)
  
  mean<-mean(labeltesting==ypred)
  mean
  
  mean <- c(mean,mean(labeltesting==ypred) )
  mean
  
}
summary(mean)

 ############### Conclusion :  XGBoost ensembled technique for company data set Accuracy is 91.25% 