
#read in data
creditData <- read_excel("creditCardData.xlsx",col_names=TRUE)
creditData<-rename(creditData,default=`default payment next month`)
creditData$default<-as.factor(creditData$default)
#to make processing quicker for now 
creditData<-creditData[1:1000,]

shinyUI(fluidPage(
    
    withMathJax(),
    
    # Application title
    titlePanel("Credit Card Default Data"

    ),
    

    # Sidebar with options for the data set
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
            
            conditionalPanel(condition="input.gtype=='Histogram'",
                             selectizeInput("histVar", "Select variable for histogram:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE")),
            ),
            
            conditionalPanel(condition="input.gtype=='Boxplot'",
                             selectizeInput("boxVar", "Select y variable for boxplot:", 
                                            selected = "LIMIT_BAL", 
                                            choices = c("LIMIT_BAL","AGE")),
            )
            ),
            
            #sidebar settings for "Clustering" tab
            conditionalPanel(condition="input.tabselected==3",
                             
                             radioButtons("marRBClust","Subset by Married Status?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Married","Single","Do Not Subset")),
                             
                             radioButtons("edRBClust","Subset by Education?",
                                          selected = ("Do Not Subset"),
                                          choices=list("Grad School","University","Highschool","Other","Do Not Subset")),
                             
                             selectizeInput("var1", "Select Variable 1", selected = names(creditData)[[5]], 
                                            choices = names(creditData)),
                             selectizeInput("var2", "Select Variable 2", selected = names(creditData)[[1]], 
                                            choices = names(creditData)),
                             
                             selectizeInput("clustMeth", "Select agglomeration method", 
                                            selected = "complete", 
                                            choices = c("complete","single","average","centroid")),
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
                    tabPanel("Explore", value=2,
                             uiOutput("title"),
                             "Click a point on graph to get x/y values",
                             verbatimTextOutput("click_info"),
                             plotOutput("creditPlot",click = "plot_click"),
                             tableOutput("table")
                             ),
                    
                    tabPanel("Clustering", value=3,
                             textOutput("cluster"),
                             plotOutput("dend")
                    ),
                    
                    tabPanel("Modeling", value=4,
                             uiOutput("mJ"),
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
