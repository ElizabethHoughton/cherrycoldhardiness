#' Calculate 10 percent lethal temperatures
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details Calculates the 10 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' @return data frame of lethal temperatures
#' 
#' @export
CH_LT10 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  readRDS("Data/Model10.rds") # vs. load("Data/Models.RData") which doesnt seem to be recognized
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- as.data.frame(predict(Model10, newdata = Calculated_CU_FU, se= TRUE)) # this calculate_CU_FU() is what occurs in the app
  # Calculate confidence intervals and create data frames out of them
  PredictLT10$LT10.CIUpper <- (PredictLT10$fit + PredictLT10$se.fit*1.96)
  PredictLT10$LT10.CILower <- (PredictLT10$fit - PredictLT10$se.fit*1.96)
  PredictLT10
}
