library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(miscTools)
library(knitr)
library(caret)

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
        creditData<-rename(creditData,default=`default payment next month`)
        creditData$default<-as.factor(creditData$default)
        #to make processing quicker for now 
        creditData<-creditData[1:1000,]
        #apply marriage status filter if applicable
        if(input$marRB=="Married"){creditData<-filter(creditData,MARRIAGE==1)}
        if(input$marRB=="Single"){creditData<-filter(creditData,MARRIAGE==2)}
        #apply education filter if applicable
        if(input$edRB=="Grad School"){creditData<-filter(creditData,EDUCATION==1)}
        if(input$edRB=="University"){creditData<-filter(creditData,EDUCATION==2)}
        if(input$edRB=="Highschool"){creditData<-filter(creditData,EDUCATION==3)}
        if(input$edRB=="Other"){creditData<-filter(creditData,EDUCATION==4)}
        #apply marriage status filter if applicable (for cluster tab)
        if(input$marRBClust=="Married"){creditData<-filter(creditData,MARRIAGE==1)}
        if(input$marRBClust=="Single"){creditData<-filter(creditData,MARRIAGE==2)}
        #apply education filter if applicable (for cluster tab)
        if(input$edRBClust=="Grad School"){creditData<-filter(creditData,EDUCATION==1)}
        if(input$edRBClust=="University"){creditData<-filter(creditData,EDUCATION==2)}
        if(input$edRBClust=="Highschool"){creditData<-filter(creditData,EDUCATION==3)}
        if(input$edRBClust=="Other"){creditData<-filter(creditData,EDUCATION==4)}
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
    output$table <- renderTable({
        summary(select(creditData,LIMIT_BAL,EDUCATION,SEX,MARRIAGE,AGE))
 
    })
    
    output$info<-renderText({
        "This app was built using R Shiny. It allows the user to explore data on 
        credit card defaults in Taiwan. The data comes from the UCI learning repository, 
        <click here> for more detail"
    })
    
    output$cluster<-renderText({
        "This will be clustering page"
    })
    

    output$dend<-renderPlot({
        
        hierClust <- hclust(dist(data.frame(creditData[input$var1], creditData[input$var2])),
                            method=input$clustMeth)
        plot(hierClust, xlab = "")
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
                           trControl=trainControl(method="cv",number=10))
           fitLogit
       } 
       else {
           myexp <- paste0("default", "~", input$preds)
           fitLogit<-train(as.formula(myexp), data=creditData, method="glm", family="binomial", 
                           preProcess=c("center","scale"), 
                           trControl=trainControl(method="cv",number=10))
           fitLogit
       }
    }
    else {
        if(input$preds=="All"){
            fitTree<-train(default ~ ., data=creditData, method="rpart",
                            preProcess=c("center","scale"),
                            trControl=trainControl(method="cv",number=10),
                            tuneGrid=NULL)
            fitTree
        } 
        else {
            myexp <- paste0("default", "~", input$preds)
            fitTree<-train(as.formula(myexp), data=creditData, method="rpart",
                           preProcess=c("center","scale"),
                           trControl=trainControl(method="cv",number=10),
                           tuneGrid=NULL)
            fitTree
        }
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