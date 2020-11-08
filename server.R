library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(miscTools)
library(knitr)

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
        creditData<-rename(creditData,default=`default payment next month`)
        creditData$default<-as.factor(creditData$default)
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
                g + geom_boxplot(aes_string(x="default",y=input$boxVar)) +
                ylim(0,1000000)
            
            
        } else {
            g + geom_histogram(aes_string(x=input$histVar)) + ylim(0,7500) 
        }
    })
    

    #show selections
    output$selections<-renderText({
        paste0("gtype is ", input$gtype, " histVar is ", input$histVar," boxVar is", input$boxVar)
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
        "This will be info page"
    })
    
    output$cluster<-renderText({
        "This will be clustering page"
    })
    
    output$model<-renderText({
        "This will be modeling page"
    })
    
    output$data<-renderText({
        "This will be data page"
    })
    
    
})