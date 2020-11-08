library(ggplot2)
library(readxl)

shinyUI(fluidPage(
    
    # Application title
    titlePanel(
        uiOutput("title")
    ),
    
    # Sidebar with options for the data set
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("marRB","Subset by Married Status?",
                         selected = ("Do Not Subset"),
                         choices=list("Married","Single","Do Not Subset")),
            
            radioButtons("edRB","Subset by Education?",
                         selected = ("Do Not Subset"),
                         choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
            
            selectizeInput("gtype", "Select graph type:", selected = "Histogram", 
                           choices = c("Histogram","Boxplot")),
            
            conditionalPanel(condition="input.gtype=='Histogram'",
                             selectizeInput("histVar", "Select variable for histogram:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE","default")),
            ),
            
            conditionalPanel(condition="input.gtype=='Boxplot'",
                             selectizeInput("boxVar", "Select y variable for boxplot:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE")),
            )
            
        ),
       
        
        # Show outputs
        mainPanel(
            
                
                # Tabset w/ Information, Data Exploration, Clustering, Modeling, Data.
                tabsetPanel(type = "tabs",
                            tabPanel("Info", textOutput("info")),
                            tabPanel("Explore", 
                                     plotOutput("creditPlot"),
                                     textOutput("selections"),
                                     tableOutput("table")
                            ),
                            tabPanel("Clustering", textOutput("cluster")),
                            tabPanel("Modeling", textOutput("model")),
                            tabPanel("Data", textOutput("data"))
                )

        )
    )
))