library(ggplot2)
library(caret)
library(readxl)
library(tidyverse)

creditData <- read_excel("creditCardData.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData<-select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_0,default)
creditData$default<-as.factor(creditData$default)
creditData$SEX<-as.factor(creditData$SEX)
creditData$EDUCATION<-as.factor(creditData$EDUCATION)
creditData$MARRIAGE<-as.factor(creditData$MARRIAGE)
creditData$PAY_0<-creditData$PAY_0
#to make processing quicker for now 
creditData<-creditData[1:1000,]

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        #apply marriage status filter if applicable
        if(input$marRB=="Married"){creditData<-filter(creditData,MARRIAGE==1)}
        if(input$marRB=="Single"){creditData<-filter(creditData,MARRIAGE==2)}
        #apply education filter if applicable
        if(input$edRB=="Grad School"){creditData<-filter(creditData,EDUCATION==1)}
        if(input$edRB=="University"){creditData<-filter(creditData,EDUCATION==2)}
        if(input$edRB=="Highschool"){creditData<-filter(creditData,EDUCATION==3)}
        if(input$edRB=="Other"){creditData<-filter(creditData,EDUCATION==4)}
        creditData
    })
    
    #create plot
    output$creditPlot <- renderPlot({
        #get filtered data
        creditData <- getData()
        
        #create plot
        g <- ggplot(creditData)
        
        if(input$gtype=="Boxplot"){
                g <- g + geom_boxplot(aes_string(x="default",y=input$boxVar))
            if(input$boxVar=="LIMIT_BAL"){g + ylim(0,750000)}
                else{g+ylim(0,80)}
                
            
            
        } else {
            g + geom_histogram(aes_string(x=input$histVar))
        }
    })
    
    #create dynamic title
    output$title<-renderUI({
        paste0(input$gtype," of Credit Data")
    })
    
    #Five number summary
    output$numSummary <- renderPrint({
        summary(select(creditData,LIMIT_BAL,EDUCATION,SEX,MARRIAGE,AGE,PAY_0))
 
    })
    
    output$info<-renderText({
        "This app was built using R Shiny. It allows the user to explore data on 
        credit card defaults in Taiwan. The data comes from the UCI learning repository, 
        <click here> for more detail"
    })
    
    output$cluster<-renderText({
        "This is a dendrogram showing the hierarchical clustering of
          the first 500 observations in our data set"
    })
    

    output$dend<-renderPlot({
        #to make processing quicker for now 
        creditData<-creditData[1:500,]
        
        hierClust <- hclust(dist(data.frame(creditData[input$var1], creditData[input$var2])),
                            method=input$clustMeth)
        plot(hierClust, xlab = paste0("Dendrogram Showing Clustering of ",input$var1," and ", input$var2))
    })
    
    output$model<-renderText({
        paste0("modeling goes here",input$preds)
    })
    
  
        output$mJ <- renderUI({
                if(input$mType=="Logistic Regression"){
                 withMathJax(
              helpText('When Y is categorical, as in this case, for logistic regression 
             we use the logit of Y as the response in our regression equation instead of just Y: 
                     $$log(\\frac{P}{1-P}) = \\beta_0+\\beta_1X_1+\\beta_2X_2+...++\\beta_kX_k$$')
            )
                }
        
        })
    
    
    output$mResults<-renderPrint({
    if(input$mType=="Logistic Regression"){
     if(input$preds=="All"){
               fitLogit<-train(default ~ ., data=creditData, method="glm", family="binomial", 
                           preProcess=c("center","scale"), 
                           trControl=trainControl(method="cv",number=input$cvFolds))
           fitLogit
       }
       else {
           myexp <- paste0("default", "~", input$preds)
           fitLogit<-train(as.formula(myexp), data=creditData, method="glm", family="binomial", 
                           preProcess=c("center","scale"), 
                           trControl=trainControl(method="cv",number=input$cvFolds))
           fitLogit
       }

    }
    else {
        if(input$preds=="All"){
            fitTree<-train(default ~ ., data=creditData, method="rpart",
                            preProcess=c("center","scale"),
                            trControl=trainControl(method="cv",number=input$cvFolds),
                            tuneGrid=NULL)
            fitTree
        } 
        else {
            myexp <- paste0("default", "~", input$preds)
            fitTree<-train(as.formula(myexp), data=creditData, method="rpart",
                           preProcess=c("center","scale"),
                           trControl=trainControl(method="cv",number=input$cvFolds),
                           tuneGrid=NULL)
            fitTree
        }
    }
    })
    
    output$pResults<-renderPrint({
      myexp <- paste0("default", "~", "PAY_0")
      if(input$mType=="Logistic Regression"){
          fitLogitPred<-train(as.formula(myexp), data=creditData, method="glm", family="binomial", 
                      preProcess=c("center","scale"), 
                        trControl=trainControl(method="cv",number=input$cvFolds))
          
          myPred<-as.numeric(input$payPred)
          predPay<-predict(fitLogitPred,newdata= data.frame(PAY_0=c(myPred)),type="prob")
          paste0("Probability of default is: ",predPay[1,2])
      }
      else{
        fitTreePred<-train(as.formula(myexp), data=creditData, method="rpart",
                           preProcess=c("center","scale"),
                           trControl=trainControl(method="cv",number=input$cvFolds),
                           tuneGrid=NULL)
        myPred<-as.numeric(input$payPred)
        predPay<-predict(fitTreePred,newdata= data.frame(PAY_0=c(myPred)),type="prob")
        paste0("Probability of default is: ",predPay[1,2])
        
      }
       
    })
    
    output$dataTable <- renderTable({
        #apply marriage status filter if applicable
        if(input$marRBDat=="Married"){creditData<-filter(creditData,MARRIAGE==1)}
        if(input$marRBDat=="Single"){creditData<-filter(creditData,MARRIAGE==2)}
        #apply education filter if applicable
        if(input$edRBDat=="Grad School"){creditData<-filter(creditData,EDUCATION==1)}
        if(input$edRBDat=="University"){creditData<-filter(creditData,EDUCATION==2)}
        if(input$edRBDat=="Highschool"){creditData<-filter(creditData,EDUCATION==3)}
        if(input$edRBDat=="Other"){creditData<-filter(creditData,EDUCATION==4)}
        if(input$save>0){write.csv(creditData,"./creditData_file.csv", row.names = FALSE)}
        view(creditData)
        
    })
    
    output$click_info <- renderText({
        paste0("x=", format(input$plot_click$x,digits=2), "\ny=", format(input$plot_click$y,digits=2))
    })
    
    
    
    
})