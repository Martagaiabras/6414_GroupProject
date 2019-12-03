#sourcing 
source("./Logistic Regression/DataTransformation.R")
source("./Logistic Regression/Splittingdata.R")


#1. Full model
# Building the model
full.model <- glm(`Churn Value` ~., family = "binomial", data = train)
summary(full.model)



# Coefficients
coefficients <-  coef(full.model)

kable(coefficients, digits =6,  caption = "Logistic regression model - Coefficients") %>%
  kable_styling(latex_options = c("striped", "hold_position"))



# Model significance overall
pc1 <- 8143.4-5628.0
pc2 <-7031-7008

#X2 test
1-pchisq(pc1,pc2)

#To see if model is significant overall we do $null deviance - residual deviance$ and we test for significance of this difference with a $X^2$ test for $df_{nulldeviance} - df_{residual_deviance}$. Since the result from the test is very close to 0, we reject the null hypothesis and we conclude the model is significant overall.



# 2. Step model
#both directions step model
step.model <- stepAIC(full.model, trace=0)
step.model$anova



#3. Lasso model

#converting data to dataframe and scaling

data.matrix <- as.matrix(train)
x <- model.matrix( ~ ., train)

predictors <- x[,1:length(train)-1]
response <-  x[,length(train)]


#Using cross validation for the Lasso regression
model_lasso <- cv.glmnet(predictors, response, alpha = 1,  family = "binomial")

#Finding optimal value of lambda that minimizes cross-validation errors
plot(model_lasso)

coef(model_lasso, model_lasso$lambda.1se)

#4. Elastic model

elastic_result <- cv.glmnet(predictors,
                            response,
                            alpha = 0.8,
                            nfolds=5,
                            type.measure="mse",
                            family="binomial",
                            standardize=FALSE)

coef(elastic_result, s = elastic_result$lambda.min)

summary(elastic_result)

