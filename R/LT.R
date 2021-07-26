# Calculations for Lethal Temperatures

# Most of these are required for data manipulation and modelling
library(plyr)
library(tidyverse)
library(MuMIn)
library(nlme)
library(report)
library(hydroGOF)
library(Metrics)
library(knitr)
library(lubridate)
library(zoo)

# load in the fitted Model10, Model50, Model90
load("Data/Models.RData")

# Predict LT10 using Model10

LT10 <- function(CUFUcalculations){
  
  # predict the LT10 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- (predict(Model10, newdata = CUFUcalculations, se= TRUE)) # this CUFUcalculations is what occurs in the app
  
  # Calculate confidence intervals and create data frames out of them
  LT10.CIUpper <- (PredictLT10$Model10 + PredictLT10$se.fit*1.96)
  LT10.CILower <- (PredictLT10$Model10 - PredictLT10$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT10 <- cbind(LT10.CIUpper, LT10.CILower, PredictLT10)
  PredictLT10 <- as.data.frame(PredictLT10)
  PredictLT10
}


# Predict LT50 using Model50

LT50 <- function(){
  
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- (predict(Model50, newdata = CUFUcalculations, se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT50.CIUpper <- (PredictLT50$Model50 + PredictLT50$se.fit*1.96)
  LT50.CILower <- (PredictLT50$Model50 - PredictLT50$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT50 <- cbind(LT50.CIUpper, LT50.CILower, PredictLT50)
  PredictLT50 <- as.data.frame(PredictLT50)
  PredictLT50
}

# Predict LT10 using Model10

LT90 <- function(){
  
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- (predict(Model90, newdata = CUFUcalculations, se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT90.CIUpper <- (PredictLT90$Model90 + PredictLT90$se.fit*1.96)
  LT90.CILower <- (PredictLT90$Model90 - PredictLT90$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT90 <- cbind(LT90.CIUpper, LT90.CILower, PredictLT90)
  PredictLT90 <- as.data.frame(PredictLT90)
  PredictLT90
}