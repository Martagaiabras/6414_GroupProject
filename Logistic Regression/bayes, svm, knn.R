#sourcing 
source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")

# 1. Bayes Model
#creating target variable
target <- train$`Churn Value`

##run NaiveBayes function
nb1 <- NaiveBayes(target ~.,data=train, usekernel=T) 

#prediction
p1 <- predict(nb1, test)$posterior[,2]


table_bayes <- table(test$`Churn Value`, p1 >= 0.2)

#ROCRpred = prediction(p1, test$`Churn Value`)
#ROCRperf = performance(ROCRpred, "tpr", "fpr")
#plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#confusion matrix
#table(true = test$`Churn Value`, p1 >= 0.2)
#1 - mean(p1$class != test$`Churn Value`)


#2. SVM Model 
#creating dummy variables
dummies <- dummyVars(~ ., data=train[,-c(5, 18:20)])

#trainign and testing datasets
c2 <- predict(dummies, train[,-c(5, 18:20)])
c3 <- predict(dummies, test[,-c(5, 18:20)])
d_training <- as.data.frame(cbind(train$`Churn Value`, c2))
d_test <- as.data.frame(cbind(test$`Churn Value`, c3))
svmfit <- svm(as.factor(V1) ~., data=d_training,  kernel = "linear", cost = 10, gamma = 1)

#tune.out <- tune(svm, as.factor(V1)~., data = d_training, ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))

#prediction
svm1 <- predict(svmfit, d_test[,-1])

#confusion matrix
confusionMatrix(svm1, as.factor(d_test$V1))

#tune.out2 <- tune.svm(as.factor(V1) ~., data=d_training, kernel='linear', cost=2^(-1:5), gamma = gammalist)
#summary(tune.out2)
#summary(tune.out2$best.model)
#svm2 <- predict(tune.out2$best.model, d_test[,-1])
#confusionMatrix(svm2, as.factor(d_test$V1))


#3. KNN model

#trying different kernells
model_2 <- train.kknn(as.factor(V1) ~., d_training, kmax = 30, kernel = 
                        c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2)

plot(model_2)
#prediction
p1 <- predict(model_2, d_test[,-1])
#confusion matrix
confusionMatrix(p1, as.factor(d_test$V1))



