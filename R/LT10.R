# load in the fitted Model10, Model50, Model90
load("Data/Models.RData")

# Predict LT10 using Model10

LT10 <- function(){
  
  # predict the LT10 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- (predict(Model10, newdata = data_input, se= TRUE))
  
  # Calculate confidence intervals and create data frames out of them
  LT10.CIUpper <- (PredictLT10$Model10 + PredictLT10$se.fit*1.96)
  LT10.CILower <- (PredictLT10$Model10 - PredictLT10$se.fit*1.96)
  
  # Create one data frame out of the predictions and CIs
  PredictLT10 <- cbind(LT10.CIUpper, LT10.CILower, PredictLT10)
  PredictLT10 <- data.frame(PredictLT10)
  PredictLT10
}