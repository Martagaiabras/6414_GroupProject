#sourcing 
source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")


#removing the spaces from variable names
names(train)<-str_replace_all(names(train), c(" " = "." ))
names(test)<-str_replace_all(names(test), c(" " = "." ))


#building model
decision_tree <- tree(Churn.Value~., data = train)

#plotting model
plot(decision_tree)
text(decision_tree)

decision_tree_cv <- cv.tree(decision_tree)
plot(decision_tree_cv$size,decision_tree_cv$dev, type ='b')


#prunning decision tree
decision_tree_pruned <- prune.tree(decision_tree, best = 5)
plot(decision_tree_pruned)
text(decision_tree_pruned)

           
#thresholds and accuracy
thresholds = array() 
accuracies = array() 

for (threshold in seq(0,1,by=0.1)){
  
  thresholds = c(thresholds,threshold)
  
  predicted_churn_score <- predict(decision_tree_pruned)
  predicted_churn_score <- predicted_churn_score[,2]
  
  predicted_churn_score[predicted_churn_score < threshold] = 0
  predicted_churn_score[predicted_churn_score >= threshold] = 1
  
  
  count=0
  for (i in 1:length(predicted_churn_score)) {
    if(predicted_churn_score[i] == train$Churn.Value[i]) count = count+1
  }
  accuracy=count/length(predicted_churn_score)
  accuracies = c(accuracies,accuracy)
  
}

plot(thresholds,accuracies, type ='b')
print(accuracies)


#predicting
predictTrain = predict(decision_tree_pruned)[,2]
table(train$`Churn.Value`, predictTrain > 0.5)

#Sensitivity: 509/(893+509)=0.36
#Specificity: 3643/(3643+229) = 94%

#ROCR
ROCRpred = prediction(predictTrain, train$`Churn.Value`)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Decision tree for insigth 

