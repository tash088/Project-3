#Author: Taylor Ashby
#Date: November 2020
#Purpose: Project #3 in ST558. This program is the server portion of the R Shiny 
#application for Project #3.

#Call required libraries
library(ggplot2)
library(caret)
library(readxl)
library(tidyverse)

#Read in data and format as needed.
creditData <- read_excel("creditCardData.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData<-select(creditData,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_0,default)
creditData$default<-as.factor(creditData$default)
creditData$SEX<-as.factor(creditData$SEX)
creditData$EDUCATION<-as.factor(creditData$EDUCATION)
creditData$MARRIAGE<-as.factor(creditData$MARRIAGE)
creditData$PAY_0<-creditData$PAY_0

#Create Shiny server
shinyServer(function(input, output, session) {
    
    #Filter data based on user inputs
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
    
    #Create plot for Data Exploration tab
    output$creditPlot <- renderPlot({
      
        #get filtered data
        creditData <- getData()
        
        #Create plot
        g <- ggplot(creditData)
        
        if(input$gtype=="Boxplot"){
                g <- g + geom_boxplot(aes_string(x="default",y=input$boxVar))
            
            #Customize y-axis scale based on variable selection
            if(input$boxVar=="LIMIT_BAL"){g + ylim(0,750000)}
                else{g+ylim(0,80)}
                
            
            
        } else {
            g + geom_histogram(aes_string(x=input$histVar))
        }
        
        
    })
    
    #This code is a duplicate of the plot creation code above, but instead of displaying 
    #the plot in the app, it will save it to a ".png" file if user clicks the download button.
    plotInput <- reactive({

      g <- ggplot(creditData)
      
      if(input$gtype=="Boxplot"){
        g <- g + geom_boxplot(aes_string(x="default",y=input$boxVar))
        if(input$boxVar=="LIMIT_BAL"){g + ylim(0,750000)}
        else{g+ylim(0,80)}
        
      } else {
        g + geom_histogram(aes_string(x=input$histVar))
      }
      
    })
    
    #This saves the plot if the user clicks the download button (on Explore tab)
    output$downloadPlot <- downloadHandler(
      filename = function() { paste('creditPlot', '.png', sep='') },
      content = function(file) {
        ggsave(file, plot=plotInput(), device = "png")
      }
    )
    
    #Create dynamic title for Explore tab
    output$title<-renderUI({
      h3(paste0(input$gtype," of Credit Data"))
    })
    
    #Allow user to click anywhere on plot and have coordinates displayed
    output$click_info <- renderText({
      paste0("x=", format(input$plot_click$x,digits=2), "\ny=", format(input$plot_click$y,digits=2))
    })
    
    #Five number summary for display on Explore tab
    output$numSummary <- renderPrint({
        summary(select(creditData,LIMIT_BAL,EDUCATION,SEX,MARRIAGE,AGE,PAY_0))
 
    })
    
    #Add note to cluster page noting that the data has been subsetted.
    output$cluster<-renderText({
        "This is a dendrogram showing the hierarchical clustering of
          the first 500 observations in our data set"
    })
    
    #Create dendrogram to display clustering results
    output$dend<-renderPlot({
        #to make processing quicker for now 
        creditData<-creditData[1:500,]
        
        hierClust <- hclust(dist(data.frame(creditData[input$var1], creditData[input$var2])),
                            method=input$clustMeth)
        plot(hierClust, xlab = paste0("Dendrogram Showing Clustering of ",input$var1," and ", input$var2))
    })
    
    #Create text including MathJax equation on Modeling tab
    output$mJ <- renderUI({
            if(input$mType=="Logistic Regression"){
             withMathJax(
          helpText('When Y is categorical, as in this case, for logistic regression 
         we use the logit of Y as the response in our regression equation instead of just Y: 
                 $$log(\\frac{P}{1-P}) = \\beta_0+\\beta_1X_1+\\beta_2X_2+...++\\beta_kX_k$$')
        )
            }
    
    })
    
    #Fit Logistic Regression or Tree Model and output results
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
    
    #Allow user to predict the probability of default based on payment status in prior month
    #i.e., PAY_0 variable
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
    
    #Display raw data on Data tab, including options for user to subset
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

    }
)
    
    
    
    
