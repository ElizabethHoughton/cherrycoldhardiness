# Variables used
#(Temp_max.lag1)
#(CU_1119) 
#(FU_acc_log)
#(FU_state)

# Predict LT10 using Model10

LT10 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("Data/Models.RData")
  # predict the LT10 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- as.data.frame(predict(Model10, newdata = Calculated_CU_FU, se= TRUE)) # this calculate_CU_FU() is what occurs in the app

  # Calculate confidence intervals and create data frames out of them
  PredictLT10$LT10.CIUpper <- (PredictLT10$fit + PredictLT10$se.fit*1.96)
  PredictLT10$LT10.CILower <- (PredictLT10$fit - PredictLT10$se.fit*1.96)

  PredictLT10
}
