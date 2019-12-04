#sourcing 
source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")

# 1. Bayes Model
#creating target variable
target <- train$`Churn Value`

##run NaiveBayes function
nb1 <- NaiveBayes(target ~.,data=train, usekernel=T) 

#prediction
p1_1 <- predict(nb1, newdata = test, threshold = 0.2)


table_bayes <- table(true = test$`Churn Value`, predict = p1_1$class)


ROCRpred_bayes = prediction(as.numeric(p1_1$class), as.numeric(test$`Churn Value`))

ROCRperf_bayes = performance(ROCRpred_bayes, "tpr", "fpr")



#2. SVM Model 
#creating dummy variables
dummies <- dummyVars(~ ., data=train[,-c(5, 18:20)])

#trainign and testing datasets
c2 <- predict(dummies, train[,-c(5, 18:20)])
c3 <- predict(dummies, test[,-c(5, 18:20)])

d_training <- as.data.frame(cbind(train$`Churn Value`, c2))
d_test <- as.data.frame(cbind(test$`Churn Value`, c3))

svmfit <- svm(as.factor(V1) ~., data=d_training,  kernel = "linear", cost = 10, gamma = 1, probability = TRUE)


#tune.out <- tune(svm, as.factor(V1)~., data = d_training, ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))

#prediction
svm1 <- predict(svmfit, d_test[,-1], probability = TRUE)
svm1_1 <- as.data.frame(attr(svm1, "probabilities")[,2])


#confusion matrix
#confusionMatrix(svm1, as.factor(d_test$V1), )
table_svm <- table(true = test$`Churn Value`, svm1_1 >0.2)

#tune.out2 <- tune.svm(as.factor(V1) ~., data=d_training, kernel='linear', cost=2^(-1:5), gamma = gammalist)
#summary(tune.out2)
#summary(tune.out2$best.model)
#svm2 <- predict(tune.out2$best.model, d_test[,-1])
#confusionMatrix(svm2, as.factor(d_test$V1))
ROCRpred_svm = prediction(svm1_1, test$`Churn Value`)

ROCRperf_svm = performance(ROCRpred_svm, "tpr", "fpr")


#3. KNN model

#trying different kernells
model_2 <- train.kknn(as.factor(V1) ~., d_training, kmax = 30, kernel = 
                        c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2, probability = TRUE)

#plot(model_2)
#prediction

p1 <- predict(model_2, d_test[,-1],  type="prob")[,2]
table_knn <- table(test$`Churn Value`, p1 >= 0.2)
#other t values
table_knn_0.5 <- table(test$`Churn Value`, p1 >= 0.5)
table_knn_0.6 <- table(test$`Churn Value`, p1 >= 0.6)



#ROC curves
ROCRpred_kknn = prediction(p1, test$`Churn Value`)
ROCRperf_kknn = performance(ROCRpred_kknn, "tpr", "fpr")


