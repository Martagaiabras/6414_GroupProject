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

test$Total.Charges <- as.numeric(as.character(test$Total.Charges))
test$Tenure.Months<- as.numeric(as.character(test$Tenure.Months))
test$Monthly.Charges  <- as.numeric(as.character(test$Total.Charges))


str(train)

#building the random forest
rf <- randomForest(Churn.Value~., data = train)
rf

#predicting
predTrain <- predict(rf, train, type = "class")
table(predTrain, train$Churn.Value)  


predTest <- predict(rf, test, type = "class")
mean(predTest == test$Churn.Value)                    
table(predTest,test$Churn.Value)


#variable importance 
importance(rf)        
varImpPlot(rf) 



