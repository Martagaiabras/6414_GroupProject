source("LoadingData.R")

## 1. Transforming variables to factors

dat$City <- as.factor(dat$City)
#length(levels(dat$City))
dat$`Zip Code` <- as.factor(dat$`Zip Code`)
#length(levels(dat$`Zip Code`))
dat$`Churn Reason` <- as.factor(dat$`Churn Reason`)
#length(levels(dat$`Churn Reason`))
dat$Gender <- as.factor(dat$Gender)
#levels(dat$Gender)
dat$`Senior Citizen` <- as.factor(dat$`Senior Citizen`)
#levels(dat$`Senior Citizen`)
dat$Dependents <- as.factor(dat$Dependents)
#levels(dat$Dependents)
dat$`Phone Service` <- as.factor(dat$`Phone Service`)
#levels(dat$`Phone Service`)
dat$`Multiple Lines` <- as.factor(dat$`Multiple Lines`)
#levels(dat$`Multiple Lines`)
dat$`Internet Service` <- as.factor(dat$`Internet Service`)
#levels(dat$`Internet Service`)
dat$`Online Security` <- as.factor(dat$`Online Security`)
#levels(dat$`Online Security`)
dat$`Online Backup` <- as.factor(dat$`Online Backup`)
#levels(dat$`Online Backup`)
dat$`Device Protection` <- as.factor(dat$`Device Protection`)
#levels(dat$`Device Protection`)
dat$`Tech Support` <- as.factor(dat$`Tech Support`)
#levels(dat$`Tech Support`)
dat$`Streaming Movies` <- as.factor(dat$`Streaming Movies`)
#levels(dat$`Streaming Movies`)
dat$`Streaming TV` <- as.factor(dat$`Streaming TV`)
#levels(dat$`Streaming TV`)
dat$Contract <- as.factor(dat$Contract)
#levels(dat$Contract)
dat$`Paperless Billing` <- as.factor(dat$`Paperless Billing`)
#levels(dat$`Paperless Billing`)
dat$`Payment Method` <- as.factor(dat$`Payment Method`)
#levels(dat$`Payment Method`)
dat$Partner <- as.factor(dat$Partner)

## 2. Dropping unecessary columns - Linear regression
drops <- c(
  "CustomerID",
  "Count",
  "Country",
  "State",
  "Churn Label",
  "Churn Value",
  "CLTV",
  "Churn Reason",
  "City",
  "Zip Code",
  "Lat Long",
  "Latitude",
  "Longitude"
)

dat.reduced <- dat[ , !(names(dat) %in% drops)]

## 2.1. Dropping NAs

nas <- dat.reduced[rowSums(is.na(dat.reduced)) > 0,]

dim(nas)

dat.reduced  <- na.omit(dat.reduced)

dim(dat.reduced)

print(20/7032)



## 3. Dropping unecessary columns - Logistic regression

drops <- c(
  "CustomerID",
  "Count",
  "Country",
  "State",
  "Churn Label",
  "CLTV",
  "Churn Reason",
  "City",
  "Zip Code",
  "Lat Long",
  "Latitude",
  "Longitude",
  "Churn Score"
)

dat.reduced_2 <- dat[ , !(names(dat) %in% drops)]

## 2.1. Dropping NAs

nas <- dat.reduced_2[rowSums(is.na(dat.reduced_2)) > 0,]

dim(nas)

dat.reduced_2  <- na.omit(dat.reduced_2)

dim(dat.reduced_2)

print(20/7032)