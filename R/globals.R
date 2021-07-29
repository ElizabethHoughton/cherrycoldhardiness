# using this script to try and solve "no visible binding for global variable ' ' warning when running check()

utils::globalVariables(c("Model10", 
                         "Model50", 
                         "Model90", 
                         "CU_state",
                         "Time",
                         "Hour",
                         "Minute",
                         "Second",
                         "Temp",
                         "Year",
                         "Month",
                         "Day",
                         "YYYYMMDD",
                         "LT10",
                         "LT50",
                         "LT90"))