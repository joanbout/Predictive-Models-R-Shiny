library(shiny)
library(shinydashboard)
library(ggplot2)
library(e1071)
library(randomForest)
library(caret)
library(rsconnect)
deployApp()

shinyServer(function(input,output){
  v <-reactiveValues(variable = NULL)
  
  #Check of the loaded data
  observeEvent(input$load, {
    
    if (is.null(input$selectFile)) return()
    v$variable<-read.table(input$selectFile$datapath,sep=",",header=T)
    
    })
  
  #Data columns tab set up
  output$ dynamicUI<-renderUI({
    if (is.null(v$variable)) return()
    selectInput("selectColumn", "Select Column to plot.",  choices = colnames(v$variable), 
                selected = 1)
  })
  
  
  #Display of the diagrams
  output$demoPlot<-renderPlot(
    if (!is.null(v$variable)) 
     boxplot(v$variable[,input$selectColumn])
      
  )
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    if (!is.null(v$variable))
    summary(v$variable)
  })
  
  
  # Generate a correlation table of the data ----
  output$cortable <- renderPrint({
    if (!is.null(v$variable))
      features<-((v$variable)[,1:9])
      cor(features)
      #cor(v$variable)
  })
  
  # Generate a random forest model of the data ----
  output$random_forest_model <- renderPrint({
    if (!is.null(v$variable))
      v$variable<-transform(v$variable,Age=as.integer(Age), BMI=as.integer(BMI), Glucose=as.integer(Glucose), Insulin=as.integer(Insulin), HOMA=as.integer(HOMA), Leptin=as.integer(Leptin), Adiponectin=as.integer(Adiponectin), Resistin=as.integer(Resistin), MCP.1=as.integer(MCP.1), Classification=as.factor(Classification))
    sample1<-createDataPartition((v$variable)$Classification,p=0.80,list = FALSE)
    trained<-v$variable[sample1,]
    tested<-v$variable[-sample1,]
    trainer<-trainControl(method = "repeatedcv",number = 20, summaryFunction = multiClassSummary,classProbs = TRUE)
    levels(trained$Classification) <- c("first_class", "second_class")
    train(Classification ~.,data = trained,method="rf",preProc=c("center","scale"),metric="Accuracy",tuneGrid=expand.grid(.mtry=c(1:3)),trControl=trainer)
  })
  
  # Generate a support vector machine model of the data ----
  output$svm_model <- renderPrint({
    if (!is.null(v$variable))
      v$variable<-transform(v$variable,Age=as.integer(Age), BMI=as.integer(BMI), Glucose=as.integer(Glucose), Insulin=as.integer(Insulin), HOMA=as.integer(HOMA), Leptin=as.integer(Leptin), Adiponectin=as.integer(Adiponectin), Resistin=as.integer(Resistin), MCP.1=as.integer(MCP.1), Classification=as.factor(Classification))
      svm(Classification ~.,data=v$variable)
  })
  
})
