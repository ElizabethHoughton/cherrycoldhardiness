#' Calculate 90 percent lethal temperatures
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details
#' Calculates the 90 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' @param Calculated_CU_FU a dataframe of calculated parameters
#' @param Model90 model for calculating 90 percent lethal temperatures
#' 
#' @return the calculated 90 percent lethal temperatures
#' 
#' @import dplyr
CH_LT90 <- function(Calculated_CU_FU=NULL){
  # load in the fitted Model10, Model50, Model90
  load("data/Model90.RData") # vs. readRDS("data/Model90.rds") which doesnt seem to be recognized
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- as.data.frame(stats::predict(Model90, newdata = Calculated_CU_FU, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT90$LT90.CIUpper <- (PredictLT90$fit + PredictLT90$se.fit*1.96)
  PredictLT90$LT90.CILower <- (PredictLT90$fit - PredictLT90$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU
  PredictLT90$YYYYMMDD90 <- Calculated_CU_FU$YYYYMMDD
  # rename fit to LT50
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90_standard_error = "se.fit")
  PredictLT90
}