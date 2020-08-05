
#======================================================
#			InsuranceDefault-EDA
#======================================================

#Set the working directory
setwd("workingdirectory")
getwd()


#Loading the libraries#
library(pacman)
p_load(readr,readxl,dplyr,ggplot2,tidyr,skimr,devtools,DataExplorer,visdat)


#Loading the Dataset into R#
Insurance_DS <- read_excel("<dataset>")
#View(Insurance_DS)

#Renaming Column Names for better readability
names(Insurance_DS)[names(Insurance_DS) == "id"] <- "Customer_ID"
names(Insurance_DS)[names(Insurance_DS) == "perc_premium_paid_by_cash_credit"] <- "Percentage_Premium_PaidByCash"
names(Insurance_DS)[names(Insurance_DS) == "age_in_days"] <- "Age"
names(Insurance_DS)[names(Insurance_DS) == "Marital Status"] <- "Marital_Status"
names(Insurance_DS)[names(Insurance_DS) == "Veh_Owned"] <- "NumberOf_Vehicle_Owned"
names(Insurance_DS)[names(Insurance_DS) == "Count_3-6_months_late"] <- "NumberOf_Premium_Paid_Late_3to6"
names(Insurance_DS)[names(Insurance_DS) == "Count_6-12_months_late"] <- "NumberOf_Premium_Paid_Late_6to12"
names(Insurance_DS)[names(Insurance_DS) == "Count_more_than_12_months_late"] <- "NumberOf_Premium_Paid_Late_After12"
names(Insurance_DS)[names(Insurance_DS) == "risk_score"] <- "Risk_Score"
names(Insurance_DS)[names(Insurance_DS) == "no_of_premiums_paid"] <- "Count_PremiumPaid"
names(Insurance_DS)[names(Insurance_DS) == "sourcing_channel"] <- "Sourcing_Channel"
names(Insurance_DS)[names(Insurance_DS) == "residence_area_type"] <- "Living_Area"
names(Insurance_DS)[names(Insurance_DS) == "premium"] <- "Premium_Paid"
names(Insurance_DS)[names(Insurance_DS) == "default"] <- "Premium_Defaulted"
names(Insurance_DS)[names(Insurance_DS) == "No_of_dep"] <- "NumberOf_Dependents"
names(Insurance_DS)[names(Insurance_DS) == "Accomodation"] <- "Accomadation_Owned_Rented"
names(Insurance_DS)

table(Insurance_DS$Premium_Defaulted)

# Conversion - Categorical Variables
Insurance_DS$Premium_Defaulted = ifelse(Insurance_DS$Premium_Defaulted=='0',"1","0")
Insurance_DS$Premium_Defaulted = as.factor(Insurance_DS$Premium_Defaulted)

str(Insurance_DS)
# Convert Variable Age to year to have better understanding
Insurance_DS$Age <- round(Insurance_DS$Age/365)


#Outliers#


#Outliers
library(e1071)

#Skewness before capping
skewness(Insurance_DS$Age)
quantile(Insurance_DS$Age, c(0.99))
Insurance_DS$Age[which(Insurance_DS$Age>86)]<- 86
#Skewness after capping
skewness(Insurance_DS$Age)

#Skewness before capping
skewness(Insurance_DS$Income)
quantile(Insurance_DS$Income, c(0.99))
Insurance_DS$Income[which(Insurance_DS$Income>771078 )]<- 771078
#Skewness after capping
skewness(Insurance_DS$Income)

# Variable Drop - Customer ID as its not essential variable for prediction
Insurance_DS$Customer_ID <-  NULL
str(Insurance_DS)

#Living Area 
Insurance_DS$Living_Area <- ifelse(Insurance_DS$Living_Area == "Rural",1,0)

#One hot encoding - Sourcing Channel
library(caret)
PremiumDefault <- Insurance_DS$Premium_Defaulted
Insurance_DS$Premium_Defaulted <-  NULL
dummy <- dummyVars(" ~ .", data=Insurance_DS)
Insurance_DS <- data.frame(predict(dummy, newdata = Insurance_DS))

library(caTools)
#set seed to constant value
set.seed(420)

#Spilt Train and Test Dataset
split <-sample.split(Insurance_DS$Premium_Defaulted,  SplitRatio= 0.70)
Insurance_train<- subset(Insurance_DS, split == TRUE)
Insurance_test<- subset(Insurance_DS, split == FALSE)
table(Insurance_DS$Premium_Defaulted)
table(Insurance_train$Premium_Defaulted)
table(Insurance_test$Premium_Defaulted)
str(Insurance_DS)
