source("./Logistic Regression/DataTransformation.R")

dat.reduced_3$`Churn Value` = as.numeric(dat.reduced_3$`Churn Value`)


obdata.agg.n = aggregate(`Churn Value`~ . , data = dat.reduced_3, FUN=length)


obdata.agg.y = aggregate(`Churn Value`~ . , data = dat.reduced_3, FUN=sum)


data <- cbind(obdata.agg.n, obdata.agg.y$`Churn Value`)

data <- data %>% rename_at(20,~"Total")

## Fitting the model

model.agg = glm(cbind(`Churn Value`,Total-`Churn Value`)~ .,
                data = data,family=binomial)

## summary the model
summary(model.agg)
## Test for overall regression
gstat = model.agg$null.deviance - deviance(model.agg)
cbind(gstat, 1-pchisq(gstat,length(coef(model.agg))-1))

## Test for GOF: Using deviance residuals
deviances2 = residuals(model.agg,type="deviance")
dev.tvalue = sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,40))
#OR
c(deviance(model.agg), 1-pchisq(deviance(model.agg),40))
res = resid(model.agg,type="deviance")
