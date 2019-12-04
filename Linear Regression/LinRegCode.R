source("./LoadingData.R")
source("./Linear Regression/LinRegDataTransformation.R")

model.full <- lm(`CLTV`~.,data=dat.reduced)

cooks.distances <- cooks.distance(model.full)
n <- dim(dat.reduced)[1]
threshold <- 4/n

#plot(cooks.distances,type='h',ylab="Cook's Distances")
#abline(h=threshold,col='red')

above.indices <- cooks.distances[cooks.distances>threshold]

num.outliers <- length(above.indices)

dat.nooutliers <- dat.reduced[cooks.distances<=threshold,]

model.full <- lm(`CLTV`~.,data=dat.nooutliers)
model.intercept.only <- lm(`CLTV`~1,data=dat.nooutliers)

# Deal with Multiple Lines
dat.nooutliers$`Multiple Lines`[dat.nooutliers$`Multiple Lines` == 'No phone service'] <- 'No'
dat.nooutliers$`Multiple Lines` <- as.factor(dat.nooutliers$`Multiple Lines`)
dat.nooutliers$`Multiple Lines` <- droplevels(dat.nooutliers$`Multiple Lines`)

# Deal with Online Security
dat.nooutliers$`Online Security`[dat.nooutliers$`Online Security` == 'No internet service'] <- 'No'
dat.nooutliers$`Online Security` <- as.factor(dat.nooutliers$`Online Security`)
dat.nooutliers$`Online Security` <- droplevels(dat.nooutliers$`Online Security`)

# Deal with Online Backup
dat.nooutliers$`Online Backup`[dat.nooutliers$`Online Backup` == 'No internet service'] <- 'No'
dat.nooutliers$`Online Backup` <- as.factor(dat.nooutliers$`Online Backup`)
dat.nooutliers$`Online Backup` <- droplevels(dat.nooutliers$`Online Backup`)

# Deal with Device Protection
dat.nooutliers$`Device Protection`[dat.nooutliers$`Device Protection` == 'No internet service'] <- 'No'
dat.nooutliers$`Device Protection` <- as.factor(dat.nooutliers$`Device Protection`)
dat.nooutliers$`Device Protection` <- droplevels(dat.nooutliers$`Device Protection`)

# Deal with Tech Support
dat.nooutliers$`Tech Support`[dat.nooutliers$`Tech Support` == 'No internet service'] <- 'No'
dat.nooutliers$`Tech Support` <- as.factor(dat.nooutliers$`Tech Support`)
dat.nooutliers$`Tech Support` <- droplevels(dat.nooutliers$`Tech Support`)

# Deal with Streaming TV
dat.nooutliers$`Streaming TV`[dat.nooutliers$`Streaming TV` == 'No internet service'] <- 'No'
dat.nooutliers$`Streaming TV` <- as.factor(dat.nooutliers$`Streaming TV`)
dat.nooutliers$`Streaming TV` <- droplevels(dat.nooutliers$`Streaming TV`)

# Deal with Streaming Movies
dat.nooutliers$`Streaming Movies`[dat.nooutliers$`Streaming Movies` == 'No internet service'] <- 'No'
dat.nooutliers$`Streaming Movies` <- as.factor(dat.nooutliers$`Streaming Movies`)
dat.nooutliers$`Streaming Movies` <- droplevels(dat.nooutliers$`Streaming Movies`)

model.full <- lm(`CLTV`~.,data=dat.nooutliers)

vif.threshold <- 1 / (1 - summary(model.full)$r.squared)

cutoff.val <- max(10,vif.threshold)

dat.nooutliers.reduced.full <- dat.nooutliers[,-18]

## 75% of the sample size
train_size <- floor(0.75 * nrow(dat.nooutliers.reduced.full))

## set the seed to make your partition reproducible
set.seed(6414)
train_indices <- sample(seq_len(nrow(dat.nooutliers.reduced.full)), size = train_size)

testData <- dat.nooutliers.reduced.full[-train_indices, ]
dat.nooutliers.reduced <- dat.nooutliers.reduced.full[train_indices, ]

model.no.collinearity <- lm(`CLTV`~.,data=dat.nooutliers.reduced)

full <- lm(`CLTV`~.,data=dat.nooutliers.reduced)
minimum <- lm(`CLTV`~1,data=dat.nooutliers.reduced)
step(minimum, scope = list(lower=minimum, upper = full), direction = "forward",trace=FALSE)

model.reduced <- lm(formula = CLTV ~ `Tenure Months` + `Total Charges` + `Device Protection` + 
                      `Internet Service` + `Streaming TV` + `Online Backup`, data = dat.nooutliers.reduced)

preds <- model.matrix(as.formula(CLTV~.),data=dat.nooutliers.reduced)[,-1]

lasso.cv <- cv.glmnet(preds,dat.nooutliers.reduced$CLTV,alpha=1,nfolds=10)
lasso.lam <- lasso.cv$lambda.min 
model.lasso <- glmnet(preds,dat.nooutliers.reduced$CLTV, alpha = 1, nlambda = 100)

model.reduced.lasso <- lm(CLTV~`Tenure Months` + `Internet Service` + `Online Security` + `Device Protection` + `Streaming TV` + Contract
                          + `Payment Method` + `Total Charges`,data=dat.nooutliers.reduced)

elnet.cv <- cv.glmnet(preds,dat.nooutliers.reduced$CLTV,alpha=0.5,nfolds=10)
elnet.lam <- elnet.cv$lambda.min 
model.elnet <- glmnet(preds,dat.nooutliers.reduced$CLTV, alpha = 0.5, nlambda = 100)

model.not.reduced.elnet <- lm(CLTV~.,data=dat.nooutliers.reduced)
model.not.reduced.elnet$coefficients <- coef(model.elnet,s=elnet.lam)

library(CombMSC)
n <- dim(dat.nooutliers.reduced)[1]
model.comp<-rbind(
  full=c(summary(model.no.collinearity)$adj.r.sq,Cp(model.no.collinearity,S2=summary(model.no.collinearity)$sigma^2),
         AIC(model.no.collinearity,k=2),AIC(model.no.collinearity,k=log(n))), 
  step=c(summary(model.reduced)$adj.r.sq,Cp(model.reduced,S2=summary(model.reduced)$sigma^2), AIC(model.reduced,k=2),
         AIC(model.reduced,k=log(n))), 
  lasso=c(summary(model.reduced.lasso)$adj.r.sq,Cp(model.reduced.lasso,S2=summary(model.reduced.lasso)$sigma^2), AIC(model.reduced.lasso,k=2),
          AIC(model.reduced.lasso,k=log(n))),
  elnet=c(summary(model.not.reduced.elnet)$adj.r.sq,Cp(model.not.reduced.elnet,S2=summary(model.not.reduced.elnet)$sigma^2),
          AIC(model.not.reduced.elnet,k=2),AIC(model.not.reduced.elnet,k=log(n)))
)
colnames(model.comp) = c("adj.rsq","Cp","AIC","BIC")

sig.coefs <- c("Tenure Months","Total Charges","Device Protection","Internet Service (Fiber Optic)")
coef.vals <- c(17.40518808,0.04729367,-94.06929302,-63.92423944)
sig.levels <- c(0.001,0.01,0.01,0.1)

model.coefs <- cbind(sig.coefs,coef.vals,sig.levels)
colnames(model.coefs) <- c("Predictor","Coefficient Value","Significance Level")

model.logresponse <- lm(log(CLTV) ~ `Tenure Months` + `Total Charges` + `Device Protection` + 
                          `Internet Service` + `Streaming TV` + `Online Backup`,data=dat.nooutliers.reduced)

bc<- boxcox(model.logresponse)

# Get lambda from the boxcox function
bc.lam <- bc$x[which.max(bc$y)]

dat.bc <- cbind(dat.nooutliers.reduced)

# Then do a power transformation using that lambda
dat.bc$CLTV <- (dat.bc$CLTV^bc.lam - 1) / bc.lam

model.bc <- lm(log(CLTV) ~ `Tenure Months` + `Total Charges` + `Device Protection` + 
                 `Internet Service` + `Streaming TV` + `Online Backup`,data=dat.bc)

testData.bc <- cbind(testData)

testData.bc$CLTV <- (testData.bc$CLTV^bc.lam - 1) / bc.lam

preds <- predict(model.bc,testData.bc,interval='prediction')[,1]

mspe.step <- mean((preds-log(testData.bc$CLTV))^2)
mae.step <- mean(abs(preds-log(testData.bc$CLTV)))
mape.step <- mean(abs(preds-log(testData.bc$CLTV))/log(testData.bc$CLTV))
pm.step <- sum((preds-log(testData.bc$CLTV))^2)/sum((log(testData.bc$CLTV)-mean(log(testData.bc$CLTV)))^2)

library(gbm)

model.boosted <- gbm(as.formula(CLTV~.),data=dat.nooutliers.reduced,distribution = 'gaussian')

preds.boosted <- predict(model.boosted,testData,interval='prediction',n.trees=100)

mspe.boost <- mean((preds.boosted-testData$CLTV)^2)
mae.boost <- mean(abs(preds.boosted-testData$CLTV))
mape.boost <- mean(abs(preds.boosted-testData$CLTV)/testData$CLTV)
pm.boost <- sum((preds.boosted-testData$CLTV)^2)/sum((testData$CLTV-mean(testData$CLTV))^2)

metrics.step <- c(mspe.step,mae.step,mape.step,pm.step)
metrics.boost <- c(mspe.boost,mae.boost,mape.boost,pm.boost)

metrics.comp <- rbind(
  stepwise = round(metrics.step,3),
  boostedTree = round(metrics.boost,3)
)

colnames(metrics.comp) <- c("MSPE","MAE","MAPE","PM")
metrics.comp
