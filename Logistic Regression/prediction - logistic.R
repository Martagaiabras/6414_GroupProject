source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")
source("./Logistic Regression/logistic.R")


##1. Predictions on training dataset

# 1.1 Prediction for full model
predictTrain_log = predict(full.model, type="response")
#summary(predictTrain)
#tapply(predictTrain, train$`Churn Value`, mean)

##2. Confusion matrix for threshold of 0.5

#table(train$`Churn Value`, predictTrain > 0.5)

#Sensitivity: 820/(820+582)=0.58
#Specificity: 3464/(3464+408) = 89%



#The sensitivity, or true positive rate of the model, is shown on the y-axis.while the false positive rate, or 1 minus the specificity, is given on the x-axis. The line shows how these two outcome measures vary with different threshold values. The sensitivity, or true positive rate of the model, is shown on the y-axis.while the false positive rate, or 1 minus the specificity, is given on the x-axis. 
#So which threshold value one should pick? One should select the best threshold for the trade-off one wants to make.If you’re more concerned with having a high specificity or low false positive rate, pick the threshold that maximizes the true positive rate while keeping the false positive rate really low.A threshold around (0.1, 0.5) on this ROC curve looks like a good choice in this case.On the other hand, if one is more concerned with having a high sensitivity or high true positive rate, one should pick a threshold that minimizes the false positive rate In our case, we are concerned about identifying the customers that are likely to churn. If we identify a false positive is not as bad as not identifying a true positive. We want a higher sensitivity and a lower threshold. For a T of 0.2 we will have 84% of true positives and 38% false positives.  We want to have a low threshold because we will filter the customers who we are targeting to. 

##B. Predictions on testing dataset

#1. full model
predictTest_log = predict(full.model, type = "response", newdata = test)
#table(test$`Churn Value`, predictTest >= 0.2)

#With this model we predicted churn in (375)/(375+92) = 80% of the cases. 


##2. step model 
step_model <- glm(`Churn Value` ~ Partner + Dependents + `Multiple Lines` + `Internet Service` + `Online Security` + `Online Backup` + `Tech Support` + `Streaming TV` +  `Streaming Movies` + Contract + `Paperless Billing` + `Payment Method` , family = "binomial", data = train)
predictTest_log = predict(step_model, type = "response", newdata = test)
step_table <- table(test$`Churn Value`, predictTest_log >= 0.2)
#other t values
step_table_0.5 <- table(test$`Churn Value`, predictTest_log >= 0.5)
step_table_0.6 <- table(test$`Churn Value`, predictTest_log >= 0.6)


ROCRpred = prediction(predictTest_log, test$`Churn Value`)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

##3. Plot ROC curve

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#With this model we predicted churn in (420)/(420+47) = 90% of the cases. 

##lasso model 
lasso_model <- glm(`Churn Value` ~ `Tenure Months` + `Internet Service` + `Online Security` + `Online Backup` + `Online Backup` + `Device Protection` + `Tech Support`, family = "binomial", data = train)
predictTest_lasso = predict(lasso_model, type = "response", newdata = test)
#table(test$`Churn Value`, predictTest >= 0.2)

#With this model we predicted churn in (382)/(382+85) = 81% of the cases. 








