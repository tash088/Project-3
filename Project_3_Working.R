library(tidyverse)
library(dplyr)

creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData
view(creditData)
creditData$default<-as.factor(creditData$default)
filter(creditData,MARRIAGE==3)
knitr::kable(data.frame(summary(select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE))))

g <- ggplot(creditData)




  g + geom_boxplot(aes(x=as.factor(default),y=LIMIT_BAL))
  
  #g + geom_histogram(aes(x=as.numeric(LIMIT_BAL)))

  
  