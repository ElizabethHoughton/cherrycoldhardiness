# Manipulate .csv inputs to calculate CU, FU, and FU state (data_input)

# select for column 5 to 10
data_input <- data_input %>% select(5:10)

# rename columns
data_input <- data_input %>% 
  rename(Date = "Date/Time (LST)")

data_input <- data_input %>% 
  rename(Time = "Time (LST)")

data_input <- data_input %>% 
  rename(Temp = "Temp (°C)")

#get a YYYYMMDD formatted date to use when graphing
data_input <- data_input %>%
  mutate(YYYYMMDD = make_date(Year, Month, Day))


# make sure they are numeric
data_input$Month <- as.numeric(data_input$Month)
data_input$Day <- as.numeric(data_input$Day)
data_input$Temp <- as.numeric(data_input$Temp)
data_input$YYYYMMDD <- as.Date(data_input$YYYYMMDD)



# sort by year, month, day, time
data_input <- data_input[
  order( data_input[,2], data_input[,3], data_input[,4], data_input[,5]),
]