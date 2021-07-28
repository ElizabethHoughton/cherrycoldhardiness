#' Calculate 50 percent lethal temperatures
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details
#' Calculates the 50 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' @export
CH_LT50 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("data/Models.RData") # vs. readRDS("data/Model50.rds") which doesnt seem to be recognized
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- as.data.frame(stats::predict(Model50, newdata = Calculated_CU_FU, se= TRUE)) # this calculate_CU_FU() is what occurs in the app
  # Calculate confidence intervals and create data frames out of them
  PredictLT50$LT50.CIUpper <- (PredictLT50$fit + PredictLT50$se.fit*1.96)
  PredictLT50$LT50.CILower <- (PredictLT50$fit - PredictLT50$se.fit*1.96)
  PredictLT50
}