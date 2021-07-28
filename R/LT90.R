# Variables used
#(Temp_max.lag1)
#(CU_1119) 
#(FU_acc_log)
#(FU_state)

# Predict LT90 using Model90

LT90 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("Data/Models.RData")
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- as.data.frame(predict(Model90, newdata = Calculated_CU_FU, se= TRUE)) # this calculate_CU_FU() is what occurs in the app
  
  # Calculate confidence intervals and create data frames out of them
  PredictLT90$LT90.CIUpper <- (PredictLT90$fit + PredictLT90$se.fit*1.96)
  PredictLT90$LT90.CILower <- (PredictLT90$fit - PredictLT90$se.fit*1.96)
  
  PredictLT90
}