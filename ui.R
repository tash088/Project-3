library(ggplot2)
library(readxl)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Credit Card Default Data"

    ),
    

    # Sidebar with options for the data set
    sidebarLayout(
        sidebarPanel(
            #sidebar settings for "Explore" tab
            conditionalPanel(condition="input.tabselected==2",
            
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
            
            #sidebar settings for "Clustering" tab
            conditionalPanel(condition="input.tabselected==3",
                             
                             radioButtons("marRB","Subset by Married Status?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Married","Single","Do Not Subset")),
                             
                             radioButtons("edRB","Subset by Education?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
                             
                             selectizeInput("var1", "Select Variable 1", selected = names(creditData)[[2]], 
                                            choices = names(creditData)),
                             selectizeInput("var2", "Select Variable 2", selected = names(creditData)[[3]], 
                                            choices = names(creditData)),
            ),
            conditionalPanel(condition="input.tabselected==4",
                             selectizeInput("mType","Select Model Type",
                                            selected="Logistic Regression",
                                            choices=c("Logistic Regression","Decision Tree")),
                                
                                selectizeInput("preds","Select Predictor Variable(s)",
                                                selected="All",
                                                choices=c("All",names(creditData)))
                             ),
            #sidebar settings for "Data" tab
            conditionalPanel(condition="input.tabselected==5",
                             
                             radioButtons("marRBDat","Subset by Married Status?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Married","Single","Do Not Subset")),
                             
                             radioButtons("edRBDat","Subset by Education?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
                             actionButton("save","Save CSV")
                             
         
                             
            )
            
            
            ),
    
    mainPanel(
        
        
        # Tabset w/ Information, Data Exploration, Clustering, Modeling, Data.
        tabsetPanel(type = "tabs",
                    tabPanel("Info", value=1,
                             textOutput("info")
                             ),
                    tabPanel("Explore", value=2,
                             uiOutput("title"),
                             plotOutput("creditPlot"),
                             tableOutput("table")
                    ),
                    tabPanel("Clustering", value=3,
                             textOutput("cluster"),
                             plotOutput("dend")
                    ),
                    tabPanel("Modeling", value=4,
                             textOutput("model"),
                            verbatimTextOutput("mResults")
                             ),
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
