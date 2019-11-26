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
