source("./Logistic Regression/prediction - logistic.R")
source("./Logistic Regression/bayes, svm, knn.R")
source("./Logistic Regression/decision tree")
source("./Logistic Regression/random forest.R")

#Accuracy, Sensitivity, Specificity for threshold of 0.2

step_table
table_DT


#ROC curve for different models
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf_DT, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Sensibility to thresholds