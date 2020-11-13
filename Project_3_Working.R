library(tidyverse)
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

summary(select(creditData,LIMIT_BAL,EDUCATION,SEX,MARRIAGE,AGE))


  
  