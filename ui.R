library(shiny)
library(shinydashboard)
library(ggplot2)
library(e1071)
library(randomForest)
library(caret)
library(rsconnect)
deployApp()

dashboardPage( 
  dashboardHeader(title="Predictive Models"),  
  dashboardSidebar(
    #Upload File Data
    fileInput("selectFile", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    #Load of data file
    actionButton("load", "Load File")
  ),    
  dashboardBody(
   #Data Tabs
     tabsetPanel(
      
      tabPanel("Plots",
               sidebarPanel(
                 uiOutput("dynamicUI")
               ),
               sidebarPanel(width=7,plotOutput("demoPlot"))),
      tabPanel("Summary",
               sidebarPanel(verbatimTextOutput("summary")),
               verbatimTextOutput("cortable"),
               sidebarPanel(verbatimTextOutput("svm_model")),
               verbatimTextOutput("random_forest_model")
               ),
               
               mainPanel()
                 
               
      
      )
     #)
  )
)     
               
          
