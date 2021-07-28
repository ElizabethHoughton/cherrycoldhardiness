# Variables used
#(Temp_max.lag1)
#(CU_1119) 
#(FU_acc_log)
#(FU_state)

# Predict LT50 using Model50

LT50 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("Data/Models.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- as.data.frame(predict(Model50, newdata = Calculated_CU_FU, se= TRUE)) # this calculate_CU_FU() is what occurs in the app
  
  # Calculate confidence intervals and create data frames out of them
  PredictLT50$LT50.CIUpper <- (PredictLT50$fit + PredictLT50$se.fit*1.96)
  PredictLT50$LT50.CILower <- (PredictLT50$fit - PredictLT50$se.fit*1.96)
  
  PredictLT50
}