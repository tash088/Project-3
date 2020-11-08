library(tidyverse)
library(dplyr)
library(readxl)
library(stats)

creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData
view(creditData)
creditData$default<-as.factor(creditData$default)
creditData2<-filter(creditData,AGE==25)
creditData2
knitr::kable(data.frame(summary(select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE))))

g <- ggplot(creditData)
hierClust <- hclust(dist(data.frame(creditData2$LIMIT_BAL, creditData2$EDUCATION)))
plot(hierClust, xlab = "")



  g + geom_boxplot(aes(x=as.factor(default),y=LIMIT_BAL))
  
  #g + geom_histogram(aes(x=as.numeric(LIMIT_BAL)))

  
  