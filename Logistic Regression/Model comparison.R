source("./Logistic Regression/prediction - logistic.R")
source("./Logistic Regression/bayes, svm, knn.R")
source("./Logistic Regression/decision tree.R")
source("./Logistic Regression/random forest.R")

#Functions for accuracy, senstivity and specificity
accuracy <- function(db) {
  accuracy <- (db[1,1] + db[2,2])/(db[1,1] + db[2,2] + db[1,2] + db[2,1])
  return(accuracy)
}

sensitivity <- function(db) {
  sensitivity <- db[2,2]/(db[2,2] + db[2,1])
  return(sensitivity)
}


specificity <- function(db) {
  specificity <- db[1,1]/(db[1,1] + db[1,2])
  return(specificity)
}


#1. accuracy
accuracy_step <- accuracy(step_table)
accuracy_DT <- accuracy(table_DT)
accuracy_RF <- accuracy(table_rf)
accuracy_Knn <- accuracy(table_knn)

#2. sensititvity
sensitivity_step <- sensitivity(step_table)
sensitivity_DT <- sensitivity(table_DT)
sensitivity_RF <- sensitivity(table_rf)
sensitivity_Knn <- sensitivity(table_knn)


#3. specificity
specificity_step <- specificity(step_table)
specificity_DT <- specificity(table_DT)
specificity_RF <- specificity(table_rf)
specificity_Knn <- specificity(table_knn)

#4. Table

table <- data.frame(cbind(c(accuracy_step,accuracy_DT, accuracy_RF, accuracy_Knn ), c(sensitivity_step, sensitivity_DT, sensitivity_RF, sensitivity_Knn), c(specificity_step, specificity_DT, specificity_RF, specificity_Knn)))
colnames(table) <- c("Accuracy", "Sensitivity", "Specificity")

row.names(table) <- c("logistic", "decision tree", "random forest", "KKNN")

#ROC curve for different models
plot(ROCRperf, col = 'darkred',lty = 2, lwd = 2, main = "ROC curve for different models", font.main=3, font.lab=3)
plot(ROCRperf_DT, col = 'darkgreen',lty = 10, lwd = 2, add = TRUE)
plot(ROCRperf_RF, col = 'orange', lty = 6, lwd = 2, add = TRUE)
#plot(ROCRperf_bayes, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#plot(ROCRperf_svm, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf_kknn, col = "blue", lty= 8, lwd = 2, add = TRUE)
legend(0.2,0.45, c('logistic','decistiontree','randomforest', "kknn"),lty=c(1,1),
       lwd=c(2,2),col=c('darkred','darkgreen','orange', "blue"))

#Sensibility to thresholds
#1. accuracy 0.5
accuracy_step_0.5 <- accuracy(step_table_0.5)
accuracy_DT_0.5 <- accuracy(table_DT_0.5)
accuracy_RF_0.5 <- accuracy(table_rf_0.5)
accuracy_Knn_0.5 <- accuracy(table_knn_0.5)

#2. accuracy 0.6
accuracy_step_0.6 <- accuracy(step_table_0.6)
accuracy_DT_0.6 <- accuracy(table_DT_0.6)
accuracy_RF_0.6 <- accuracy(table_rf_0.6)
accuracy_Knn_0.6 <- accuracy(table_knn_0.6)

#3. Table

table <- data.frame(cbind(c(accuracy_step,accuracy_DT, accuracy_RF, accuracy_Knn ), c(accuracy_step_0.5, accuracy_DT_0.5, accuracy_RF_0.5, accuracy_Knn_0.5), c(accuracy_step_0.6, accuracy_DT_0.6, accuracy_RF_0.6, accuracy_Knn_0.6)))
colnames(table) <- c("0.2T", "0.5T", "0.6T")

row.names(table) <- c("logistic", "decision tree", "random forest", "KKNN")
