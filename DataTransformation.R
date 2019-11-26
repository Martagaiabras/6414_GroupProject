source("LoadingData.R")

## A. General Transformations

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
#levels(dat$`Payment Method`)
dat$`Churn Value` <- as.factor(dat$`Churn Value`)

## 2. Adding a column with reason to churn 

dat.reduced_C <- dat  %>%  filter(`Churn Reason` != "NA" & `Churn Reason` != "Don't know") %>% 
  mutate(Reason = case_when(
    grepl("Price|Extra data charges|Long distance charges",`Churn Reason`) ~ "Price", 
    grepl("Attitude|Service dissatisfaction|Poor expertise of phone support|Poor expertise of online support",`Churn Reason`) ~ "Customer service",
    grepl("Competitor",`Churn Reason`) ~ "Competitors offer",
    grepl("Network reliability|Product dissatisfaction|Lack of affordable download/upload speed|Lack of self-service on Website|Lack of affordable download/upload speed|Limited range of services",`Churn Reason`) ~ "Product features",                  
    TRUE ~ "Other"
  ))
  
  
## B. Transformations for linear
## 1. Dropping unecessary columns 
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

## 2. Dropping NAs

nas <- dat.reduced[rowSums(is.na(dat.reduced)) > 0,]

dim(nas)

dat.reduced  <- na.omit(dat.reduced)

dim(dat.reduced)

print(20/7032)



## C. Transformation for logistic   
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
  "Churn Score",
"Monthly Charges",
"Total Charges"
)

dat.reduced_2 <- dat[ , !(names(dat) %in% drops)]

##2. Dropping NAs

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


dat.reduced_2 <- dat.reduced_2  %>%  
  mutate(Tenure = case_when(
    `Tenure Months` <= 6 ~ "lesst6",
    `Tenure Months` > 6 & `Tenure Months` <=18  ~ "6to18",
    `Tenure Months` > 18 & `Tenure Months` <= 30  ~ "18to30",
    `Tenure Months` > 30  & `Tenure Months` <= 42 ~ "38to42",
    `Tenure Months` > 42  & `Tenure Months` <= 54 ~ "42to54",
    `Tenure Months` > 54 ~ "more54",
  ))

dat.reduced_2$Tenure

#4. Removing tenure months continuous and adding factor

dat.reduced_2 <- dat.reduced_2[-c(5)]
dat.reduced_2$Tenure <- as.factor(dat.reduced_2$Tenure)

