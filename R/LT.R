# Calculations for Lethal Temperatures

# Most of these are required for data manipulation and modelling
library(dplyr)

# Variables used
#(Temp_max.lag1)
#(CU_1119) 
#(FU_acc_log)
#(FU_state)

# load in the fitted Model10, Model50, Model90


# Predict LT10 using Model10

LT10 <- function(Temp_max.lag1=NULL, CU_1119=NULL, FU_acc_log=NULL, FU_state=NULL){
  # load model data
  load("Data/Models.RData")
  
  # predict the LT10 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- (predict(Model10, newdata = data.frame(calculate_CU_FU), se= TRUE)) # this calculate_CU_FU() is what occurs in the app
  
  # Calculate confidence intervals and create data frames out of them
  LT10.CIUpper <- (PredictLT10$Model10 + PredictLT10$se.fit*1.96)
  LT10.CILower <- (PredictLT10$Model10 - PredictLT10$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT10 <- cbind(LT10.CIUpper, LT10.CILower, PredictLT10)
  PredictLT10 <- as.data.frame(PredictLT10)
  PredictLT10
}


# Predict LT50 using Model50

LT50 <- function(Temp_max.lag1=NULL, CU_1119=NULL, FU_acc_log=NULL, FU_state=NULL){
  # load model data
  load("Data/Models.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- (predict(Model50, newdata = data.frame(calculate_CU_FU), se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT50.CIUpper <- (PredictLT50$Model50 + PredictLT50$se.fit*1.96)
  LT50.CILower <- (PredictLT50$Model50 - PredictLT50$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT50 <- cbind(LT50.CIUpper, LT50.CILower, PredictLT50)
  PredictLT50 <- as.data.frame(PredictLT50)
  PredictLT50
}

# Predict LT10 using Model10

LT90 <- function(Temp_max.lag1=NULL, CU_1119=NULL, FU_acc_log=NULL, FU_state=NULL){
  # load model data
  load("Data/Models.RData")
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- (predict(Model90, newdata = data.frame(calculate_CU_FU), se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT90.CIUpper <- (PredictLT90$Model90 + PredictLT90$se.fit*1.96)
  LT90.CILower <- (PredictLT90$Model90 - PredictLT90$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT90 <- cbind(LT90.CIUpper, LT90.CILower, PredictLT90)
  PredictLT90 <- as.data.frame(PredictLT90)
  PredictLT90
}