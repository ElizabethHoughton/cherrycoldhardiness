#' Calculate 50 percent lethal temperatures
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details
#' Calculates the 50 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' @param Calculated_CU_FU50 a dataframe of calculated parameters
#' 
#' @return the calculated 50 percent lethal temperatures
#' 
#' @import dplyr AICcmodavg
#' 
#' @export
CH_LT50 <- function(Calculated_CU_FU50=NULL){
  # load in the fitted Model10, Model50, Model90
  load("data/Model50.Rda") # vs. readRDS("data/Model50.rds") which doesnt seem to be recognized
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- as.data.frame(AICcmodavg::predictSE.gls(Model50, newdata = Calculated_CU_FU50, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT50$LT50.CIUpper <- (PredictLT50$fit + PredictLT50$se.fit*1.96)
  PredictLT50$LT50.CILower <- (PredictLT50$fit - PredictLT50$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU50
  PredictLT50$YYYYMMDD50 <- Calculated_CU_FU50$YYYYMMDD
  # rename fit to LT50
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50_standard_error = "se.fit")
  PredictLT50
}