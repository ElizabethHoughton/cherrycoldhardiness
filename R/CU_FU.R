#' Calculate chilling units and forcing units
#' 
#' Variables used: Temp_max.lag1, CU_1119, FU_acc_log, FU_state
#' 
#' @details 
#' Calculates chilling and focing units based on work by Neilsen et al. (2015)
#' 
#' @export
CU_FU <- function(data_input=NULL) {
  #uses the data_input from the .csv upload
  # select columns of interest
  data_input <- data_input %>% select(5:10)

# rename columns
  colnames(data_input) <- c("Date", "Year", "Month", "Day", "Time", "Temp")
# make sure they are numeric
data_input$Month <- as.numeric(data_input$Month)
data_input$Day <- as.numeric(data_input$Day)
data_input$Temp <- as.numeric(data_input$Temp)
data_input$Time <- as.POSIXct(data_input$Time,format="%H:%M:%S") # this converts the time out of being a factor but adds a day that doesn't make sense
# sort by year, month, day, time
data_input <- data_input[
  order( data_input[,2], data_input[,3], data_input[,4], data_input[,5]),
]

# replace any NA values with the average of the above and below temp values (if needed)
data_input$Temp <- (na.approx(data_input$Temp, rule = 2))

data_input_max <- data_input #save to calculate daily max and lag temps

# FOR NOW ASSUME THEY ARE UPLOADING DATA STARTING JULY-MAY OF THE YEAR OF INTEREST

# Calculate CU

data_input <- data_input %>% mutate(CU = case_when(Temp < -2 ~ 0, 
                                                 Temp >= 18 ~ -1, 
                                                 Temp >= 16 & Temp < 18 ~ -0.5,
                                                 Temp < 16 & Temp >= -2 ~
                                                   1-((46.8012*exp(-exp(-(Temp-9.8275)/2.1401)))/42.80757)))

# Make a new column (CU_acc) that is the accumulation of all the calculated CU from each day prior and the current day

data_input <- data.frame(data_input, CU_acc=cumsum(data_input$CU))

#remove the first NA value with na.omit
data_input <- na.omit(data_input)

# Anything after the minimum CU_acc, assign the accumulative CU, everything before assign 0
# which.min outputs the row that the minimum value exists in

#select from the min CU_acc value to the final data row

data_input_2 <- data_input[which.min(data_input$CU_acc):nrow(data_input), ]

# Calculate the CU accumulation (CU_acc_final) from just these remaining chill units
data_input_2 <- data.frame(data_input_2, CU_acc_final=cumsum(data_input_2$CU))

# add data_input_2$CU_acc_final to your data_input so that you do not lose temp data from days previous

data_input_final <- merge(data_input, data_input_2, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc"), all = TRUE)

# Replace NA values with 0
data_input_final[is.na(data_input_final)] <- 0

# If CU_acc_final below 1119, assign acquiring, if CU_acc_final above 1119, assign complete
data_input_final <- data_input_final %>% mutate(CU_state = case_when(CU_acc_final < 1119 ~ 'acquiring', 
                                                             CU_acc_final >= 1119 ~ 'complete'))

# For the model, have a column that is 0-1119 (Call it CU_1119)
data_input_final <- data_input_final %>% mutate(CU_1119 = case_when(CU_acc_final < 1119 ~ CU_acc_final, 
                                                                  CU_acc_final >= 1119 ~ 1119))

# FU Calculations
# FU will begin accumulation after 1119 CU have been accumulated
# Subset data that has complete the chilling requirements

data_input_FU <- data_input_final %>% filter(CU_state=="complete")

# calculate the FU for these times
# From Neilsen et al. 2015

data_input_FU <- data_input_FU %>% mutate(FU = case_when(Temp < 5 ~ 0, 
                                                    Temp >= 5 ~ 1.5/(1+exp(-(Temp-22.8)/5.7))))

# Make a new column (FU_acc) that is the accumulation of all the calculated FU from each day prior and the current day

data_input_FU <- data.frame(data_input_FU, FU_acc=cumsum(data_input_FU$FU))

# add FU to CU data frame

data_input_final <- merge(data_input_final, data_input_FU, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc", "CU_acc_final", "CU_state", "CU_1119"), all = TRUE)

# Replace NA values with 0
data_input_final[is.na(data_input_final)] <- 0

# Optimal forcing reached at 68 FU, assign FU_state 
data_input_final <- data_input_final %>% mutate(FU_state = case_when(CU_acc_final < 1119 ~ 'not_applicable',
                                                                   FU_acc < 68 ~ 'acquiring', 
                                                                   FU_acc >= 68 ~ 'complete'))


# Now, to bring to the resolution of daily maximum air temperature, keep only the daily data from the end (10:00) at each day (most growers will not be expecting an hourly resolution anyways)

data_input_daily <- data_input_final %>% 
  mutate(Hour = hour(Time),
         Minute = minute(Time),
         Second = second(Time)) %>%  
  dplyr::filter(Hour == 10 & Minute == 0 & Second == 0)

# add log transformed forcing units
data_input_daily$FU_acc_log <- log(data_input_daily$FU_acc+10)


## Calculate Daily Air Temperatures from Hourly Data
# Aggregate by Year, Month, Day
# Max temp
data_input_dailymax <- aggregate(Temp ~ Year + Month + Day, data_input_max, max)

# rename Temp to Temp_max
data_input_dailymax <- data_input_dailymax %>% 
  dplyr::rename(Temp_max = Temp)

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
  mutate(YYYYMMDD = make_date(Year, Month, Day))

# rename it for the webpage (call CU_FU1 so not to be confused with CU_FU function)

CUFUcalculations <- as.data.frame(CU_FU1)

# select for variables you will use in your model: 
#YYYYMMDD
#(Temp_max.lag1)
#(CU_1119) 
#(FU_acc_log)
#(FU_state)

CUFUcalculations <- CUFUcalculations %>% select(c("YYYYMMDD", 
                                                  "Temp_max.lag1", 
                                                  "CU_1119", 
                                                  "FU_acc", 
                                                  "FU_acc_log", 
                                                  "FU_state"))

# remove first row to deal with lag NA             
CUFUcalculations <- CUFUcalculations[-1,]

CUFUcalculations

}
