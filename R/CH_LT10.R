#' Calculate 10 percent lethal temperatures
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details 
#' Calculates the 10 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' @param Calculated_CU_FU a dataframe of calculated parameters
#' @param Model10 model for calculating 10 percent lethal temperatures
#' 
#' @return the calculated 10 percent lethal temperatures
#' 
#' @import dplyr
#' 
#' @export
CH_LT10 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("data/Model10.RData") # vs. readRDS("data/Model10.rds") which doesnt seem to be recognized
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT10 <- as.data.frame(stats::predict(Model10, newdata = Calculated_CU_FU, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT10$LT10.CIUpper <- (PredictLT10$fit + PredictLT10$se.fit*1.96)
  PredictLT10$LT10.CILower <- (PredictLT10$fit - PredictLT10$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU
  PredictLT10$YYYYMMDD <- Calculated_CU_FU$YYYYMMDD
  # rename fit to LT50
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10_standard_error = "se.fit")
  PredictLT10
}

