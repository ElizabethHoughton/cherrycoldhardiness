#' Calculate chilling units and forcing units
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details 
#' Calculates chilling and focing units based on work by Neilsen et al. (2015)
#' 
#' @param data_input the user uploaded .csv file
#' 
#' @return the calculated 90 percent lethal temperatures
#' 
#' @import dplyr lubridate
#' 
#' @export
CU_FU <- function(data_input=NULL) {
  #uses the data_input from the .csv upload
  # select columns of interest
  data_input <- data_input[,c(3,5:10)]
  # rename columns
  colnames(data_input) <- c("Station.Name", "Date", "Year", "Month", "Day", "Time", "Temp")
  # make sure they are numeric
  data_input$Month <- as.numeric(data_input$Month)
  data_input$Day <- as.numeric(data_input$Day)
  data_input$Temp <- as.numeric(data_input$Temp)
  data_input$Time <- as.POSIXct(data_input$Time,format="%H:%M") # this converts the time out of being a factor but adds a day that doesn't make sense
  # sort by year, month, day, time
  data_input <- data_input[
    order( data_input[,3], data_input[,4], data_input[,5], data_input[,6]),
  ]
  # replace any NA values with the average of the above and below temp values (if needed)
  data_input$Temp <- (zoo::na.approx(data_input$Temp, rule = 2))
  
  data_input_max <- data_input #save to calculate daily max and lag temps
  
  # Calculate CU
  
  data_input <- data_input %>% dplyr::mutate(CU = case_when(Temp < -2 ~ 0, 
                                                            Temp >= 18 ~ -1, 
                                                            Temp >= 16 & Temp < 18 ~ -0.5,
                                                            Temp < 16 & Temp >= -2 ~
                                                              1-((46.8012*exp(-exp(-(Temp-9.8275)/2.1401)))/42.80757)))
  
  # Make a new column (CU_acc) that is the accumulation of all the calculated CU from each day prior and the current day
  
  data_input <- data.frame(data_input, CU_acc=cumsum(data_input$CU))
  
  #remove the first NA value with na.omit
  data_input <- stats::na.omit(data_input)
  
  # Anything after the minimum CU_acc, assign the accumulative CU, everything before assign 0
  # which.min outputs the row that the minimum value exists in
  
  # if the length of the data frame when selecting for values after the minimum CU_acc value is 
  # greater than 0, assign CU_acc_final to be the cumulative sum of CU, if it is shorter than 1,
  # assign CU_acc_final a value of zero
  
  if (nrow(data_input[which.min(data_input$CU_acc):nrow(data_input), ]) > 1){
    data_input_2 <- data_input[which.min(data_input$CU_acc):nrow(data_input), ]
    data_input_2 <-  data.frame(data_input_2, CU_acc_final=cumsum(data_input_2$CU))
    # remove Station.Name from data_input so there isnt duplicates in data upload
    station <- as.character(last(data_input$Station.Name)) # built in incase Station.Name problems
    data_input <- subset(data_input, select = -c(Station.Name))
    data_input <- merge(data_input, data_input_2, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc"), all = TRUE)
    data_input$Station.Name <- station
  } else {
    data_input <- data_input %>% dplyr::mutate(CU_acc_final = 0)
  }
  
  # Replace NA values with 0
  data_input[is.na(data_input)] <- 0
  
  # If CU_acc_final below 1119, assign acquiring, if CU_acc_final above 1119, assign complete
  data_input <- data_input %>% dplyr::mutate(CU_state = case_when(CU_acc_final < 1119 ~ 'acquiring', 
                                                                  CU_acc_final >= 1119 ~ 'complete'))
  
  # For the model, have a column that is 0-1119 (Call it CU_1119)
  data_input <- data_input %>% dplyr::mutate(CU_1119 = case_when(CU_acc_final < 1119 ~ CU_acc_final, 
                                                                 CU_acc_final >= 1119 ~ 1119))
  
  # FU Calculations
  # FU will begin accumulation after 1119 CU have been accumulated
  # Subset data that has complete the chilling requirements
  
  data_input_FU <- data_input %>% dplyr::filter(CU_state=="complete")
  
  # calculate the FU for these times
  # From Neilsen et al. 2015
  
  data_input_FU <- data_input_FU %>% dplyr::mutate(FU = case_when(Temp < 5 ~ 0, 
                                                                  Temp >= 5 ~ 1.5/(1+exp(-(Temp-22.8)/5.7))))
  
  # Make a new column (FU_acc) that is the accumulation of all the calculated FU from each day prior and the current day
  
  data_input_FU <- data.frame(data_input_FU, FU_acc=cumsum(data_input_FU$FU))
  
  # add FU to CU data frame
  
  data_input_final <- merge(data_input, data_input_FU, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc", "CU_acc_final", "CU_state", "CU_1119", "Station.Name"), all = TRUE)
  
  # Replace NA values with 0
  data_input_final[is.na(data_input_final)] <- 0
  
  # Optimal forcing reached at 68 FU, assign FU_state 
  data_input_final <- data_input_final %>% dplyr::mutate(FU_state = case_when(CU_acc_final < 1119 ~ 'not_applicable',
                                                                              FU_acc < 68 ~ 'acquiring', 
                                                                              FU_acc >= 68 ~ 'complete'))
  
  
  # Now, to bring to the resolution of daily maximum air temperature, keep only the daily data from the end (10:00) at each day (most growers will not be expecting an hourly resolution anyways)
  
  data_input_daily <- data_input_final %>% 
    dplyr::mutate(Hour = lubridate::hour(Time),
                  Minute = lubridate::minute(Time),
                  Second = lubridate::second(Time)) %>%  
    dplyr::filter(Hour == 10 & Minute == 0 & Second == 0)
  
  # add log transformed forcing units
  data_input_daily$FU_acc_log <- log(data_input_daily$FU_acc+10)
  
  
  ## Calculate Daily Air Temperatures from Hourly Data
  # Aggregate by Year, Month, Day
  # Max temp
  data_input_dailymax <- stats::aggregate(Temp ~ Year + Month + Day, data_input_max, max)
  # Min temp
  data_input_dailymin <- stats::aggregate(Temp ~ Year + Month + Day, data_input_max, min)
  
  # rename Temp to Temp_max and Temp to Temp_min
  data_input_dailymax <- data_input_dailymax %>% 
    dplyr::rename(Temp_max = Temp)
  
  data_input_dailymin <- data_input_dailymin %>% 
    dplyr::rename(Temp_min = Temp)
  
  # merge max and min by Year, Month, Day
  data_input_dailymax <- merge(data_input_dailymax, data_input_dailymin, by = c("Year", "Month", "Day"), all = TRUE)
  
  # sort by year, month, day
  data_input_dailymax <- data_input_dailymax[
    order(data_input_dailymax[,1], data_input_dailymax[,2], data_input_dailymax[,3]),
  ]
  
  
  # Assign 1st order temperature lags for max temp
  # For max temps
  data_input_dailymax <- data.frame(data_input_dailymax, Temp_max.lag1 = dplyr::lag(data_input_dailymax$Temp_max))
  
  # Merge CU_FU and daily max temps into one
  CU_FU1 <- merge(data_input_dailymax, data_input_daily, by = c("Year", "Month", "Day"), all = TRUE)
  
  # sort by year, month, day
  CU_FU1 <- CU_FU1[
    order(CU_FU1[,1], CU_FU1[,2], CU_FU1[,3]),
  ]
  
  #get a YYYYMMDD formatted date to use when graphing
  CU_FU1 <- CU_FU1 %>%
    dplyr::mutate(YYYYMMDD = lubridate::make_date(Year, Month, Day))
  
  # rename it for the webpage (call CU_FU1 so not to be confused with CU_FU function)
  
  CUFUcalculations <- as.data.frame(CU_FU1)
  
  # select for variables you will use in your model: 
  #YYYYMMDD
  #(Temp_max.lag1)
  #(CU_1119) 
  #(FU_acc_log)
  #(FU_state)
  #(Temp_min) for graphs
  
  CUFUcalculations <- CUFUcalculations %>% dplyr::select(c("Station.Name",
                                                           "YYYYMMDD", 
                                                           "Temp_min",
                                                           "Temp_max.lag1", 
                                                           "CU_1119", 
                                                           "FU_acc", 
                                                           "FU_acc_log", 
                                                           "FU_state"))
  
  # remove first row to deal with lag NA             
  CUFUcalculations <- CUFUcalculations[-1,]
  
  # if there is an issue with missing data, this will fill the NA value with the value from the row previous
  
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_max.lag1, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(CU_1119, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_acc, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_acc_log, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_state, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Station.Name, .direction= c("up"))
  
  # Remove last day in case there is only a partial day of data
  CUFUcalculations <- head(CUFUcalculations, -1)
  
  # assign all Station.Name values to the value of the last row (then it will be e.g. 'Kelowna' not 'KELOWNA UBCO')
  CUFUcalculations$Station.Name <- as.character(last(CUFUcalculations$Station.Name))
  CUFUcalculations
}