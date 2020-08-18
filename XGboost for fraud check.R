install.packages("xgboost")
install.packages("methods")
require(xgboost)
require(methods)
library(xgboost)
library(methods)
library(caret)
library(Matrix)

###  ifelse condtion applying  if taxable income <=30000 Risky ,else good 
Risky_good=ifelse(fc$Taxable.Income<=30000,"1","0")
str(Risky_good)
Risky_good=as.numeric(Risky_good)
str(Risky_good)
f=cbind(Risky_good,fc)
View(f)
f=f[-4]
f
a <- factor(f$Undergrad, levels = c("YES", "NO"), labels = c(1, 0))
a <- as.numeric(a)
b <- factor(f$Marital.Status, levels = c("Single", "Divorced","Married"), labels = c(-1,1, 0))
b <- as.numeric(b)
c <- factor(f$Undergrad, levels = c("YES", "NO"), labels = c(1, 0))
c <- as.numeric(c)

f1=cbind(f,a,b,c)
View(f1)
newdata <- f1[c(-2,-3,-6)]
View(newdata)
colnames(newdata)
names(newdata)[4] <- "Udergrand"
names(newdata)[5]<- "Marital.Status"
names(newdata)[6]<- "Urban"
colnames(newdata)
View(newdata)
# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

newdata_norm<-as.data.frame(lapply(newdata,normalize))
View(newdata_norm)



mean<-c()
for(i in 1:24)
{
  
  inTraining <- createDataPartition(newdata_norm$Risky_good, p = .90, list = FALSE)
  training <- newdata_norm[ inTraining,]
  testing  <- newdata_norm[-inTraining,]

  
  
  labeltraining = as.numeric(training[[1]])
  datatraining = as.matrix(training[2:6])
  
  labeltesting  = as.numeric(testing [[1]])
  datatesting  = as.matrix(testing [2:6])
  
  xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)
  
  xgtesting <- xgb.DMatrix(data=datatesting, label = labeltesting)
  
  
  param = list("objective" = "binary:hinge","n_class"=5, "bst:eta" = 1,"bst:max_depth" = 7,"nthread" = 4,"gamma" =1,"min_child_weight" = 3)
  
  model = xgboost(params = param, data = xgtraining, nround = 10)#,subsample = 0.8,colsample_bytree = 0.8)
  
  ypred = predict(model, xgtesting)
  
  mean<-mean(labeltesting==ypred)
  mean
  
  mean <- c(mean,mean(labeltesting==ypred) )
  mean
  
}

summary(mean)
 

############# CONCLUSION : XGboost ensembled technique for fraud check data set Accuracy is 89 % 
