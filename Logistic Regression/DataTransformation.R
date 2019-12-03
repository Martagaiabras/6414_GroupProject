source("LoadingData.R")

## A. General Transformations
## 1. Transforming variables to factors
dat <- dat %>% mutate_at(vars(-c(`Churn Reason`, `Churn Score`, `Churn Label`, `Total Charges`, `Monthly Charges`, `Tenure Months`, `Zip Code`, CustomerID, `Lat Long`,  Latitude, Longitude)), as.factor)


## B. Transformation for logistic   
## 1. Dropping unecessary columns - Logistic regression

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

#2. Dropping NAs

nas <- dat.reduced_2[rowSums(is.na(dat.reduced_2)) > 0,]

dim(nas)

dat.reduced_2  <- na.omit(dat.reduced_2)

dim(dat.reduced_2)

print(20/7032)

#3. Creating buckets for Tenure months

dat.reduced_2  %>%  summarize(
  avg_tenure = mean(`Tenure Months`),
  std = sd(`Tenure Months`),
  max = max(`Tenure Months`),
  min = min(`Tenure Months`)
)


dat.reduced_3 <- dat.reduced_2 
dat.reduced_3$Tenure <- cut(dat.reduced_2$`Tenure Months`, 5, labels = c("bin1", "bin2", "bin3", "bin4", "bin5"))


#4. Removing tenure months continuous and adding factor
dat.reduced_3 <- dat.reduced_3[ , !(names(dat.reduced_3) %in% "Tenure Months")]


#5. Including total charges as bin
Charges <-  dat.reduced_3$`Total Charges`
dat.reduced_3 <- cbind(dat.reduced_3, Charges)
dat.reduced_3$Charges <- cut(dat.reduced_3$Charges, 5, labels = c("bin1", "bin2", "bin3", "bin4", "bin5"))

#6. Removing charges total + monthly charges 
dat.reduced_3 <- dat.reduced_3[ , !(names(dat.reduced_3) %in% c("Monthly Charges", "Total Charges"))]

