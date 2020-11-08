library(tidyverse)
library(dplyr)
library(readxl)
library(caret)

creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData<-select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,default)
creditData$default<-as.factor(creditData$default)
creditData<-creditData[1:1000,]
creditData

fitTree1<-train(default ~ LIMIT_BAL, data=creditData, method="rpart",
               preProcess=c("center","scale"),
               trControl=trainControl(method="cv",number=10),
               tuneGrid=NULL)

fitTree2<-train(default ~ EDUCATION, data=creditData, method="rpart",
               preProcess=c("center","scale"),
               trControl=trainControl(method="cv",number=10),
               tuneGrid=NULL)

fitLogit1<-train(default ~ LIMIT_BAL, data=creditData, method="glm", family="binomial", 
                preProcess=c("center","scale"), 
                trControl=trainControl(method="cv",number=2))

fitLogit2<-train(default ~ EDUCATION, data=creditData, method="glm", family="binomial", 
                preProcess=c("center","scale"), 
                trControl=trainControl(method="cv",number=2))

fitTree1
fitTree2
fitLogit1
fitLogit2



  
  