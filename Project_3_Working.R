library(tidyverse)
library(dplyr)
library(readxl)
library(caret)

help(predict)

creditData <- read_excel("creditCardData.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData<-select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_0,default)
creditData$default<-as.factor(creditData$default)
creditData$SEX<-as.factor(creditData$SEX)
creditData$EDUCATION<-as.factor(creditData$EDUCATION)
creditData$MARRIAGE<-as.factor(creditData$MARRIAGE)
creditData$PAY_0<-creditData$PAY_0
creditData

myexp <- paste0("default", "~", "PAY_0")
fitLogitPred<-train(as.formula(myexp), data=creditData, method="glm", family="binomial", 
                    preProcess=c("center","scale"), 
                    trControl=trainControl(method="cv",number=10))
fitLogitPred
fitLogitPred$coefnames
predPay<-predict(fitLogitPred,newdata= data.frame(PAY_0=c(2)),type="prob")
predPay[1,2]


fitTree<-train(as.formula(myexp), data=creditData, method="rpart",
               preProcess=c("center","scale"),
               trControl=trainControl(method="cv",number=5),
               tuneGrid=NULL)
fitTree
predPay<-predict(fitTree,newdata= data.frame(PAY_0=c(-1)),type="prob")
predPay[1,2]


  
  