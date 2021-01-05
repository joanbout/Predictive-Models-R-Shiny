#Predictive Models Project for dataR2

#First step is to clear our data table before we begin
rm(list = ls())
###require(caTools)

#Call of the library caret which contains functions to create partitions of data for our data training and testing
library(caret)
#Call of the library ggplot2 in order to plot our results
library(ggplot2)
#Call of the library MLmetrics which contains the proper tools for manipulation of trained data
library(MLmetrics)
#Call of library e1071 which is used in order to create the support vector machine training model
library(e1071)
#Call of the library randomForest which contains the functions for creating a random forest predictive model
library(randomForest)
#Call of the library that reads excel files
library(readxl)
#Import url address to read our data and download the excel file
url<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.xlsx"
#Use of tempfile function to store temporally our data
p1f <- tempfile()
#Download the file
download.file(url, p1f, mode="wb")
#Read excel file with read_excel command
dataR2<-read_excel(path = p1f)
#See the dimension of our data
dim(dataR2)
#See data 
View(dataR2)
#Name the data columns
names(dataR2) <- c("Age", "BMI", "Glucose", "Insulin", "HOMA", "Leptin", "Adiponectin", "Resistin", "MCP", "Classification")
#View a summary of our data
summa<-summary(dataR2)
summa
#View the types of each column
sapply(dataR2,class)
#Transform numeric to integer and character to factor
dataR2<-transform(dataR2,Age=as.integer(Age), BMI=as.integer(BMI), Glucose=as.integer(Glucose), Insulin=as.integer(Insulin), HOMA=as.integer(HOMA), Leptin=as.integer(Leptin), Adiponectin=as.integer(Adiponectin), Resistin=as.integer(Resistin), MCP=as.integer(MCP), Classification=as.factor(Classification))
#View the new types of each column
sapply(dataR2,class)
#View a summary of our data
summa1<-summary(dataR2)
summa1
#Remove non numerical/integer columns
features<-dataR2[,1:9]
#View new dataset
View(features)
#Create a correlation table in order to see which features are related
cor_table<-cor(features)
#View correlation table 
cor_table
#Find the columns which are highly correlated
high_cor<-findCorrelation(cor_table,cutoff = 0.8,verbose=FALSE,exact = FALSE)
#View highly correlated columns
high_cor

#Tree based method Random Forest
#Creating train and test sets
sample1<-createDataPartition(dataR2$Classification,p=0.80,list = FALSE)
trained<-dataR2[sample1,]
tested<-dataR2[-sample1,]
#Definition of the training method 
trainer<-trainControl(method = "repeatedcv",number = 20, summaryFunction = multiClassSummary,classProbs = TRUE)
#Use of levels function for categorizing Classification into two classes
levels(trained$Classification) <- c("first_class", "second_class")
#Definition of training model
train_model<-train(Classification ~.,data = trained,method="rf",preProc=c("center","scale"),metric="Accuracy",tuneGrid=expand.grid(.mtry=c(1:3)),trControl=trainer)
#View training model
train_model
#View training models randomly selected predictors
plot(train_model,log='y')
#Test of the testing set
predict.rf<-predict(train_model,newdata=tested)
#View of prediction
predict.rf
#Creating confusion matrix of our predicted and tested data
cm<-table(Predicted=predict.rf,Actual=tested$Classification)
#View confusion matrix
cm
#Calculate misclassification rate
mis_rate<-1-sum(diag(cm))/sum(cm)
mis_rate

#Support Vector Machines
#Creating support vector machine training model
svm_model<-svm(Classification ~.,data = dataR2)
svm_model
#View summary of the model
summa2<-summary(svm_model)
summa2
#Plot of our model
##plot(svm_model,data = dataR2,Age~Classification,slice = list(Insulin=3,Glucose=3))
#Test of the testing set
predict.svm<-predict(svm_model,tested)
predict.svm
#Creating confusion matrix of our predicted and tested data
cm1<-table(Predicted=predict.svm, Actual=tested$Classification)
#View confusion matrix
cm1
#Calculate misclassification rate
misrate<-1-sum(diag(cm1))/sum(cm1)
misrate

#Tuning
##set.seed(85)
##t_model<-tune(svm,Classification ~.,data = dataR2,ranges = list(epsilon=seq(0,0.1,0.01),cost=2^(2:9)))
##t_model
#Best Model
##new_model<-t_model$best.model
##new_model
##predict.best<-predict(new_model,tested)
##predict.best
##cm2<-table(Predicted=predict.best, Actual=tested$Classification)
##cm2
##misclas_rate<-1-sum(diag(cm2))/sum(cm2)
##misclas_rate

##comp<-c(cm,mis_rate,cm1,misrate)
##comp
##save(comp,file = 'Confusion_Matrix_Misclassification_Rate.rda')