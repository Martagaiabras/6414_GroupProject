#libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggpubr)
library(car)
library(glmnet)
library(gridExtra)
require(faraway)
library(aod)
library(ggpubr)
library(caTools)
library(MASS)
library(klaR)
library(lattice)
library(caret)
library(e1071)
library(kknn)
library(tree)
library(tidyverse)
library(ROCR)
library(randomForest)
library(CombMSC)
library(gbm)
require(caTools)



#importing data
dat <- read_csv('Telco_customer_churn.csv')
dat <- as_data_frame(dat)


