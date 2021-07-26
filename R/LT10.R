# Calculations for LT10

LT10 <- function(){
  # load in the fitted Model10
  readRDS("Data/Model10.rds")
  
  # predict the LT10 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- as.data.frame(predict(Model10, newdata= csv_input, se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT10.CIUpper <- (PredictLT10$Model10 + PredictLT10$se.fit.10*1.96)
  LT10.CILower <- (PredictLT10$Model10 - PredictLT10$se.fit.10*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT10 <- cbind(LT10.CIUpper, LT10.CILower, PredictLT10)
  PredictLT10 <- as.data.frame(PredictLT10)
}

load("Data/Models.RData")
Model10
Model50
