source("./Logistic Regression/prediction - logistic.R")
source("./Logistic Regression/bayes, svm, knn.R")
source("./Logistic Regression/decision tree")
source("./Logistic Regression/random forest.R")

#Accuracy, Sensitivity, Specificity for threshold of 0.2

table(test$`Churn Value`, predictStep >= 0.2)



#ROC curve for different models


#Sensibility to thresholds