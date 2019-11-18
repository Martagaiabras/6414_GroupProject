#libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggpubr)
library(car)
library(MASS)
library(glmnet)
library(gridExtra)
require(faraway)
library(aod)
library(ggpubr)


#importing data
dat <- read_csv('Telco_customer_churn.csv')
dat <- as_data_frame(dat)


