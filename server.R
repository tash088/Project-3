library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        creditData <- read_excel("default of credit card clients.xlsx",col_names=TRUE)
        creditData<-rename(creditData,default=`default payment next month`)
        creditData$default<-as.factor(creditData$default)
        creditData
    })
    
    #create plot
    output$creditPlot <- renderPlot({
        #get filtered data
        creditData <- getData()
        
        #create plot
        g <- ggplot(creditData)
        
        if(input$gtype=="Boxplot"){
                g + geom_boxplot(aes_string(x="default",y=input$boxVar))
            
            
        } else {
            g + geom_histogram(aes_string(x=input$histVar))
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
    
    #create output of observations    
    #output$table <- renderTable({
        #creditData()
    #})
    
})