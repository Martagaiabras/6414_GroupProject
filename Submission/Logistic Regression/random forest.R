#sourcing 
source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")


#removing the spaces from variable names
names(train)<-str_replace_all(names(train), c(" " = "." ))
names(test)<-str_replace_all(names(test), c(" " = "." ))


#converting variables to numeric
train$Total.Charges <- as.numeric(as.character(train$Total.Charges))
train$Tenure.Months<- as.numeric(as.character(train$Tenure.Months))
train$Monthly.Charges  <- as.numeric(as.character(train$Total.Charges))
#train$Churn.Value  <- as.numeric(as.character(train$Churn.Value))

test$Total.Charges <- as.numeric(as.character(test$Total.Charges))
test$Tenure.Months<- as.numeric(as.character(test$Tenure.Months))
#test$Churn.Value  <- as.numeric(as.character(test$Churn.Value))


#str(train)

#building the random forest
rf <- randomForest(Churn.Value~., data = train)
#rf


#predicting
predTrain_rf <- predict(rf, train, type = "prob")
#table(predTrain, train$Churn.Value)  


predTest_rf <- predict(rf, newdata = test, type = "prob")[,2]
#mean(predTest == test$Churn.Value)                    
table_rf <- table(test$Churn.Value, predTest_rf >=0.2)
table_rf_0.5 <- table(test$Churn.Value, predTest_rf >=0.5)
table_rf_0.6 <- table(test$Churn.Value, predTest_rf >=0.6)

#variable importance 
#importance(rf)        
#varImpPlot(rf) 

#ROC curves
ROCRpred_RF = prediction(predTest_rf, test$`Churn.Value`)
ROCRperf_RF = performance(ROCRpred_RF, "tpr", "fpr")


