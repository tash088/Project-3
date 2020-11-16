#Author: Taylor Ashby
#Date: November 2020
#Purpose: Project #3 in ST558. This program is the UI portion of the R Shiny 
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
creditDataPreds<-select(creditData,-default)

#Create Shiny UI
shinyUI(fluidPage(
    
    withMathJax(),
    
    # Application title
    titlePanel("Credit Card Default Data"

    ),
    

    # Sidebar with options including subsetting, plot type selection, etc.
    sidebarLayout(
        sidebarPanel(
            
            #sidebar settings for "Explore" tab
            conditionalPanel(condition="input.tabselected==2",
            
                radioButtons("marRB","Subset by Married Status?",
                            selected = "Do Not Subset",
                            choices=list("Married","Single","Do Not Subset")),
            
                radioButtons("edRB","Subset by Education?",
                            selected = "Do Not Subset",
                            choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
            
                selectizeInput("gtype", "Select graph type:", selected = "Histogram", 
                                choices = c("Histogram","Boxplot")),
            
                #Variable selection if plot type is histogram
                conditionalPanel(condition="input.gtype=='Histogram'",
                                 selectizeInput("histVar", "Select variable for histogram:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE")),
                ),
                
                #Variable selection if plot type is Boxplot
                conditionalPanel(condition="input.gtype=='Boxplot'",
                                selectizeInput("boxVar", "Select y variable for boxplot:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE")),
                ),
            
                #Allow user to download and save the plot image.
                downloadButton('downloadPlot', 'Download Plot')
            
            ),
            
            #Sidebar settings for "Clustering" tab
            conditionalPanel(condition="input.tabselected==3",
                             
                 #Select first variable for clustering
                 selectizeInput("var1", "Select Variable 1", selected = names(creditData)[[5]], 
                                choices = names(creditData)),
                 
                 #Select second variable for clustering
                 selectizeInput("var2", "Select Variable 2", selected = names(creditData)[[1]], 
                                choices = names(creditData)),
                 
                 #Allow user to customize agglomeration method for clustering
                 selectizeInput("clustMeth", "Select agglomeration method", 
                                selected = "complete", 
                                choices = c("complete","single","average","centroid")),
            ),
            
            #Sidebar settings for "Modeling" tab
            conditionalPanel(condition="input.tabselected==4",
                
                #Select model type             
                selectizeInput("mType","Select Model Type",
                                selected="Logistic Regression",
                                choices=c("Logistic Regression","Decision Tree")),
                
                #Select predictor variables    
                selectizeInput("preds","Select Predictor Variable(s)",
                                    selected="All",
                                    choices=c("All",names(creditDataPreds))),
                
                #User can customize # of folds for cross-validation
                numericInput("cvFolds","Choose # of Folds for Cross-Validation (Between 2 and 10)",
                             value=5,min=2,max=10,step=1),
                
                "The results indicate that PAY_0 (# of months behind in prior month, where 0 indicates current) 
                has the most predictive power. Use the dropdown below to predict the probability of default based
                on the PAY_0 value.",
                        
                #User can select a value for the PAY_0 for prediction
                selectizeInput("payPred","Select a value for PAY_0 to see the resulting probability of default:",
                               selected=c(0),
                               choices=c(-2,-1,0,1:9)),
                            
            ),
            
            #Sidebar settings for "Data" tab
            conditionalPanel(condition="input.tabselected==5",
                
                #Options to subset by marriage and/or education level                          
                 radioButtons("marRBDat","Subset by Married Status?",
                              selected = ("Do Not Subset"),
                              choices=list("Married","Single","Do Not Subset")),
                 
                 radioButtons("edRBDat","Subset by Education?",
                              selected = ("Do Not Subset"),
                              choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
                
                #Button to allow user to save the data as a CSV file
                actionButton("save","Save CSV")
            )
            
            
        ),
    
    mainPanel(
        
        # Create tabs for Information, Data Exploration, Clustering, Modeling, Data.
        tabsetPanel(type = "tabs",
            
            #Output for Information tab
            tabPanel("Info", value=1,
                     h3("General Information"),
                     "This app, designed with R Shiny, can be used to conduct exploratory data
                     analysis and modeling on Taiwanese credit card default data.  
                      More details on the data set are available on the ",
                     a(href="https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients",
                       target="_blank","UCI Machine Learning website"),
                     ".",
                     br(),
                     br(),
                     "For simplicity, the data has been subsetted to excluded 
                     the time-series variables. The modeling page will 
                     allow the user to run",
                     em("Logistic Regression"),
                     "and/or",
                     em("Decision Trees"),
                     "models to predict whether a borrower will default. On the data tab, 
                     you can view the raw data and save it out to a CSV
                     file if desired. Simply click on the tabs at the top of the page to 
                     navigate between them.",
                     br(),
                     br(),
                     "Note that many of the tabs have options to filter the data and/or customize
                     the output, so keep an eye out for those options in the sidebar panel."
                     ),
            
            #Output for Data Exploration tab
            tabPanel("Explore", value=2,
                     uiOutput("title"),
                     "Click a point on graph to get x/y values",
                     verbatimTextOutput("click_info"),
                     plotOutput("creditPlot",click = "plot_click"),
                     verbatimTextOutput("numSummary")
                     ),
            
            #Output for Clustering tab
            tabPanel("Clustering", value=3,
                     textOutput("cluster"),
                     plotOutput("dend")
            ),
            
            #Output for Modeling tab
            tabPanel("Modeling", value=4,
                     uiOutput("mJ"),
                     textOutput("model"),
                    verbatimTextOutput("mResults"),
                    verbatimTextOutput("pResults")
                     ),
            
            #Output for Data tab
            tabPanel("Data", value=5,
                     textOutput("data"),
                     tableOutput("dataTable")
                     ),
            
            id="tabselected"
        )
    )
    )

    )
)
