#libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggpubr)
library(car)

#importing data
dat <- read_csv('Telco_customer_churn.csv')
dat <- as_data_frame(dat)
