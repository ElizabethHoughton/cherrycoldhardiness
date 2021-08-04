#' Shiny app for cold hardiness estimations
#' 
#' Web document to calculate cold hardiness estimations
#' 
#' @details Calculates the 10. 50, and 90 percent lethal temperatures for Sweetheart sweet cherries in the Okanagan Valley based on daily air temperatures.
#' 
#' 
#' @return web application, data frame of lethal temperatures
#' 
#' @import shiny shinydashboard shinythemes ggplot2 png utils
#' @importFrom graphics legend lines
#' 
#' @export
cherrycoldhardiness <- function() {
  
  # the user interface
  ui <- fluidPage(theme = shinytheme("flatly"),
                  # Navbar title
                  navbarPage("Sweet Cherry Cold Hardiness Estimations",
                             # first tab panel
                             tabPanel("About",
                                      # inputs for first tab pane;
                                      mainPanel(
                                        h3("Modelling Approach for Sweet Cherry 
                                           Cold Hardiness Estimations in the Okanagan 
                                           Valley of British Columbia"), # size and text for paragraph
                                        p("The cold hardiness of perennial plants refers to their ability to 
                                          tolerate freezing temperatures. This model has been developed to help 
                                          estimate cold hardiness by determining the temperatures that cause cold damage to the flower buds of the 
                                          sweet cherry cultivar 'Sweetheart' grafted on Mazzard rootstock located within 
                                          the Okanagan Valley of British Columbia."),
                                        br(),
                                        p("This model relies on hourly air temperature data and estimates the lethal temperatures that would result in
                                        10%, 50%, and 90% damage to the cherry fruit buds. These estimates are made based on the maximum daily air 
                                        temperatures from the day before and calculations of the daily chilling and heat requirement accumulation based on models developed by",
                                        a(href ="https://www.researchgate.net/publication/282060426_Development_of_chilling_and_forcing_relationships_for_modeling_spring_phenology_of_apple_and_sweet_cherry", "Neilsen et al. (2015)"),
                                        "."),
                                        br(),
                                        h3("Website Information"),
                                        p("Questions and comments about this website can be sent to Elizabeth Houghton (elizabeth.houghton@ubc.ca).
                                          The website source content can be viewed on",
                                          a(href="https://github.com/ElizabethHoughton/cherry-cold-hardiness", "GitHub"), #active weblink to GitHub
                                          "."))),
                             # second tab panel
                             tabPanel("How to Use",
                                      mainPanel(
                                        h3("Data Requirements"), # size and text for paragraph
                                        p("This model requires hourly air temperature data starting in September of the season you would like to estimate
                                        lethal temperatures. If you are interested in using your own hourly temperature data, it must be uploaded as
                                        a CSV file in the same format as the following template:"),
                                        br(), #line break
                                        downloadButton("downloadTemplate", 
                                                       "Download file template"),
                                        h3("Don't have your own data?"),
                                        p("That's okay! Choose one of four loactions currently available and hourly temperature data from the",
                                          a(href="https://climate.weather.gc.ca/historical_data/search_historic_data_e.html", "Government of Canada's Historical Database"),
                                        "will be used in the model. This option may take up to a few minutes to run. You can find the exact weather stations used at each location here:",
                                          a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2007-11-30%7C2021-08-03&dlyRange=2005-01-01%7C2021-08-02&mlyRange=%7C&StationID=46987&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=2&searchMethod=contains&Month=8&Day=3&txtStationName=vernon&timeframe=1&Year=2021", "Vernon"),
                                          ",",
                                          a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-12-13%7C2021-08-03&dlyRange=2013-12-16%7C2021-08-02&mlyRange=%7C&StationID=51117&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=16&searchMethod=contains&Month=8&Day=3&txtStationName=kelowna&timeframe=1&Year=2021", "Kelowna"),
                                          ",",
                                          a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=1994-02-01%7C2021-08-03&dlyRange=1990-06-01%7C2021-08-02&mlyRange=1990-01-01%7C2006-11-01&StationID=979&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=3&searchMethod=contains&Month=8&Day=3&txtStationName=summerland&timeframe=1&Year=2021", "Summerland"),
                                          ",",
                                          a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-05-07%7C2021-08-03&dlyRange=2012-05-10%7C2021-08-02&mlyRange=%7C&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=1&searchMethod=contains&Month=8&Day=3&txtStationName=penticton&timeframe=1&Year=2021", "Penticton"),
                                          "."
                                          )
                                      )),
                             # third tab panel
                             tabPanel(
                               title = "Estimations",
                               sidebarLayout(
                                 sidebarPanel(
                                   title = "Inputs",
                                   fileInput("csv_input", "Select CSV File to Import", accept = ".csv", multiple = TRUE),
                                   selectInput("location", "Or Select Closest Location", choices = c("Not Selected"= "not_sel",
                                                                                   "Penticton"="penticton",
                                                                                   "Summerland"="summerland",
                                                                                   "Kelowna"="kelowna",
                                                                                   "Vernon"="vernon"
                                                                                   )),
                                   actionButton("run_button", 
                                                "Run Analysis",
                                                style="margin-top: 12%"),
                                   #Add a button for saving any calculated corrections as a .csv
                                   downloadButton("downloadData", 
                                                  "Save results",
                                                  style= "margin-top: 12%")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       title = "LT10",
                                       plotOutput("plot_LT10")),
                                     tabPanel(
                                       title = "LT50",
                                       plotOutput("plot_LT50")),
                                     tabPanel(
                                       title = "LT90",
                                       plotOutput("plot_LT90"))
                                   )
                                   
                                 )
                               )
                             )
                  )
  )
  
  # The plots that will be drawn when your Run Analysis (or run_button) is hit, do not treat Calculate_LT10 as a function
  # (like Calculated_LT10()) because it is within a function
  # plot LT10
  
  draw_plot_LT10 <- function(Calculated_LT10)
  {plot(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10, # CHANGE THIS IF YOU RENAME YOUR data_input 
        main= "Lethal Temperature for 10% Bud Damage",
        pch= NA,
        xlab="Date", 
        ylab="LT10 (C)",
        xlim=c((as.Date(min(Calculated_LT10$YYYYMMDD))+30), as.Date(max(Calculated_LT10$YYYYMMDD))),
        cex.lab=1, ylim=c(-25, 0))
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10, lty = 1, lwd=1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10.CIUpper, lty = 2, col="grey40") #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10.CILower, lty = 2, col="grey40") #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$Temp_min, lty = 1, col= "blue", lwd=1) # daily minimum temp
    legend("bottomleft", legend=c("Estimated Lethal Temp", "95% Confidence Intervals", "Daily Minimum Temp"), 
           col=c("black", "grey40", "blue"), lty=c(1, 2, 1), lwd=c(2, 2, 2), cex=0.8, bty = "n") # add legend
  }
  
  # plot LT50
  
  draw_plot_LT50 <- function(Calculated_LT50)
  {plot(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50, # CHANGE THIS IF YOU RENAMTE YOUR data_input 
        main= "Lethal Temperature for 50% Bud Damage",
        pch= NA,
        xlab="Date", 
        ylab="LT50 (C)",
        xlim=c((as.Date(min(Calculated_LT50$YYYYMMDD50))+30), as.Date(max(Calculated_LT50$YYYYMMDD50))),
        cex.lab=1, ylim=c(-25, 0))
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50, lty = 1, lwd=1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50.CIUpper, lty = 2, col="grey40") #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50.CILower, lty = 2, col="grey40")#WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$Temp_min, lty = 1, col= "blue", lwd=1) # daily minimum temp
    legend("bottomleft", legend=c("Estimated Lethal Temp", "95% Confidence Intervals", "Daily Minimum Temp"), 
           col=c("black", "grey40", "blue"), lty=c(1, 2, 1), lwd=c(2, 2, 2), cex=0.8, bty = "n") # add legend
  }
  
  # plot LT90
  
  draw_plot_LT90 <- function(Calculated_LT90)
  {plot(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90, # CHANGE THIS IF YOU RENAMTE YOUR data_input 
        main= "Lethal Temperature for 90% Bud Damage",
        pch=NA,
        xlab="Date", 
        ylab="LT90 (C)",
        xlim=c((as.Date(min(Calculated_LT90$YYYYMMDD90))+30), as.Date(max(Calculated_LT90$YYYYMMDD90))),
        cex.lab=1, ylim=c(-25, 0))
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90, lty = 1, lwd=1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90.CIUpper, lty = 2, col="grey40") #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90.CILower, lty = 2, col="grey40")#WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$Temp_min, lty = 1, col= "blue", lwd=1) # daily minimum temp
    legend("bottomleft", legend=c("Estimated Lethal Temp", "95% Confidence Intervals", "Daily Minimum Temp"), 
           col=c("black", "grey40", "blue"), lty=c(1, 2, 1), lwd=c(2, 2, 2), cex=0.8, bty = "n") # add legend, bty removes box outline
  }
  
  # The current year and last year
  
  # LETS CHANGE THIS TO SEE IF IT WORKS IF I PRETEND IT IS JANUARY 2021 ###
  current_year <- as.numeric(2021)
  last_year <- as.numeric(2020)
  current_month <- as.numeric(1)
  today_date <- as.POSIXct("2021-01-15 01:00:00", format="%Y-%m-%d %H:%M:%S")
  
  
  ########## REPLACE WITH THESE VALUES TO MAKE THE APP AUTOMATICALLY CALCULATE BASED ON TODAYS DATE####
  #current_year <- as.integer(format(Sys.Date(), "%Y"))
  #last_year <- as.integer(format(Sys.Date(), "%Y")) - 1
  #current_month <- as.integer(Sys.Date(), "%M")
  #today_date <- format(Sys.Date(), format = "%Y-%m-%d")
  ###########
  
  # to create a date function
  #create a year-month-day data frame that makes it aug of this or last year
  Year <- c(last_year)
  Year2 <- c(current_year)
  Month<- 8
  Day <- 1

  
  TDAY <- c(today_date)
  select_date1 <- data.frame(Year, Year2, Month, Day)
  FROM <- (ISOdatetime(select_date1$Year, select_date1$Month, select_date1$Day, hour=1, min=0, sec=0))
  FROM2 <- (ISOdatetime(select_date1$Year2, select_date1$Month, select_date1$Day, hour=1, min=0, sec=0))
  select_date1$FROM <- FROM
  select_date1$TDAY <-TDAY
  
  # for selecting dates from the year prior to this year
  select_date1$TDAY <- as.POSIXct(select_date1$TDAY, format="%Y-%m-%d %H:%M:%S")
  select_date1$FROM <- as.POSIXct(select_date1$FROM, format="%Y-%m-%d %H:%M:%S")
  
  # for selecting dates in the same year
  select_date2 <- select_date1
  select_date2$FROM <- FROM2

  
  # Define server logic required
  # I think adding 'session' into the function allows you to access the functions created in your other .R files?
  server <- function(input, output, session) {
    # data input as a data.table called "data_input" (data.table is similar in function to data.frame, this is reactive
    # so you must call on it like it is a function from here on as "data_input()", will update with each new upload)
    data_input <- reactive({
      if(!is.null(input$csv_input) & input$location == "not_sel"){ # if a .csv file is uploaded (not null) that is your data input
      infile <- input$csv_input
      read.csv(infile$datapath)
      #if summerland is selected from the drop down menu
      } else if (input$location == "summerland"){ 
        # if the current date is in the winter-spring (before June)
        if(current_month < 6){                    
        summerland_station <- data.frame(StationID = c(979),
                                         start = rep(last_year, 1),
                                         end = rep(current_year, 1))
        met <- getData(summerland_station, folder = "./data/summerland", verbose = FALSE)
        summerland_data <- dplyr::bind_rows(met)
        # select data from august the year prior to today's date
        summerland_data <- summerland_data %>% dplyr::filter(summerland_data[,6] >= select_date1$FROM & 
                                                        summerland_data[,6] <= select_date1$TDAY)
        # remove the first column
        infile = summerland_data[,-1]
      }else{
        # if the current date is in the spring-winter (after June)
        summerland_station <- data.frame(StationID = c(979),
                                         start = rep(current_year, 1),
                                         end = rep(current_year, 1))
        met <- getData(summerland_station, folder = "./data/summerland", verbose = FALSE)
        summerland_data <- dplyr::bind_rows(met)
        # select data from august this year to today's date
        summerland_data <- summerland_data %>% dplyr::filter(summerland_data[,6] >= select_date2$FROM & 
                                                        summerland_data[,6] <= select_date2$TDAY)
        # remove the first column
        infile <- summerland_data[,-1]
      }
      # if penticton is selected from the drop down menu    
      }else if (input$location == "penticton"){ 
        # if the current date is in the winter-spring (before June)
        if(current_month < 6){                    
          penticton_station <- data.frame(StationID = c(50269),
                                           start = rep(last_year, 1),
                                           end = rep(current_year, 1))
          met <- getData(penticton_station, folder = "./data/penticton", verbose = FALSE)
          penticton_data <- dplyr::bind_rows(met)
          # select data from august the year prior to today's date
          penticton_data <- penticton_data %>% dplyr::filter(penticton_data[,6] >= select_date1$FROM & 
                                                          penticton_data[,6] <= select_date1$TDAY)
          # remove the first column
          infile = penticton_data[,-1]
        }else{
          # if the current date is in the spring-winter (after June)
          penticton_station <- data.frame(StationID = c(50269),
                                           start = rep(current_year, 1),
                                           end = rep(current_year, 1))
          met <- getData(penticton_station, folder = "./data/penticton", verbose = FALSE)
          summerland_data <- dplyr::bind_rows(met)
          # select data from august this year to today's date
          penticton_data <- penticton_data %>% dplyr::filter(penticton_data[,6] >= select_date2$FROM & 
                                                          penticton_data[,6] <= select_date2$TDAY)
          # remove the first column
          infile <- penticton_data[,-1]
        }
        # if kelowna is selected from the drop down menu  
      }else if (input$location == "kelowna"){ 
        # if the current date is in the winter-spring (before June)
        if(current_month < 6){                    
          kelowna_station <- data.frame(StationID = c(51117),
                                          start = rep(last_year, 1),
                                          end = rep(current_year, 1))
          met <- getData(kelowna_station, folder = "./data/kelowna", verbose = FALSE)
          kelowna_data <- dplyr::bind_rows(met)
          # select data from august the year prior to today's date
          kelowna_data <- kelowna_data %>% dplyr::filter(kelowna_data[,6] >= select_date1$FROM & 
                                                          kelowna_data[,6] <= select_date1$TDAY)
          # remove the first column
          infile = kelowna_data[,-1]
        }else{
          # if the current date is in the spring-winter (after June)
          kelowna_station <- data.frame(StationID = c(51117),
                                          start = rep(current_year, 1),
                                          end = rep(current_year, 1))
          met <- getData(kelowna_station, folder = "./data/kelowna", verbose = FALSE)
          kelowna_data <- dplyr::bind_rows(met)
          # select data from august this year to today's date
          kelowna_data <- kelowna_data %>% dplyr::filter(kelowna_data[,6] >= select_date2$FROM & 
                                                          kelowna_data[,6] <= select_date2$TDAY)
          # remove the first column
          infile <- kelowna_data[,-1]
        }
        
      }else if (input$location == "vernon"){ 
        # if the current date is in the winter-spring (before June)
        if(current_month < 6){                    
          vernon_station <- data.frame(StationID = c(46987),
                                          start = rep(last_year, 1),
                                          end = rep(current_year, 1))
          met <- getData(vernon_station, folder = "./data/vernon", verbose = FALSE)
          vernon_data <- dplyr::bind_rows(met)
          # select data from august the year prior to today's date
          vernon_data <- vernon_data %>% dplyr::filter(vernon_data[,6] >= select_date1$FROM & 
                                                          vernon_data[,6] <= select_date1$TDAY)
          # remove the first column
          infile = vernon_data[,-1]
        }else{
          # if the current date is in the spring-winter (after June)
          vernon_station <- data.frame(StationID = c(46987),
                                          start = rep(current_year, 1),
                                          end = rep(current_year, 1))
          met <- getData(vernon_station, folder = "./data/vernon", verbose = FALSE)
          vernon_data <- dplyr::bind_rows(met)
          # select data from august this year to today's date
          vernon_data <- vernon_data %>% dplyr::filter(vernon_data[,6] >= select_date2$FROM & 
                                                          vernon_data[,6] <= select_date2$TDAY)
          # remove the first column
          infile <- vernon_data[,-1]
        }
        #if nothing is selected or uploaded (so site doesn't immediately crash)
      } else if (is.null(infile)) {
        return(NULL)
      }
      
  
      })

    # alternate approach
    #req(input$csv_input)
    # gives error code if not a .csv file
    #ext <- tools::file_ext(input$csv_input$name)
    #validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
    #fread(input$csv_input$datapath)
    # })
    # will need to add 'data.table' package if you use this approach
    
    # create a data frame out of data_input() with calculated CU and FU using CU_FU function
    # you need 3 seperate Calculated_CU_FU variables so that the same variable is not being called on in each 
    # CH_LT10, CH_LT50, CH_LT90 as this causes errors when trying to download the app somewhere else
    Calculated_CU_FU10 <- reactive({
      CUFU <- CU_FU(data_input())
      CUFU
    })
    Calculated_CU_FU50 <- reactive({
      CUFU <- CU_FU(data_input())
      CUFU
    })
    Calculated_CU_FU90 <- reactive({
      CUFU <- CU_FU(data_input())
      CUFU
    })
    
    # create a data frame out of data_input() with calculated LT using LT10/LT50/LT90 functions
    Calculated_LT10 <- reactive({
      LT10calc <- CH_LT10(Calculated_CU_FU10())
      LT10calc
    })
    
    
    Calculated_LT50 <- reactive({
      LT50calc <- CH_LT50(Calculated_CU_FU50())
      LT50calc
    })
    
    Calculated_LT90 <- reactive({
      LT90calc <- CH_LT90(Calculated_CU_FU90())
      LT90calc
    })
    
    # merge Calculated_LT10, Calculated_LT50, and Calculated_LT90, select only important columns (for download)
    All_LTs <- reactive({
      LTs <- cbind(Calculated_LT10(), Calculated_LT50(), Calculated_LT90())
      LTs <- LTs %>% dplyr::select(YYYYMMDD, LT10, LT50, LT90)
      # round to 2 decimal places
      LTs$LT10 <- format(round(LTs$LT10, 2), nsmall = 2) 
      LTs$LT50 <- format(round(LTs$LT50, 2), nsmall = 2)
      LTs$LT90 <- format(round(LTs$LT90, 2), nsmall = 2)
      LTs
    })
    
    # When the run button is hit, pltos should be drawn
    # plot LT10
    plot_LT10 <- eventReactive(input$run_button,{
      draw_plot_LT10(Calculated_LT10())
    })
    
    output$plot_LT10 <- renderPlot(plot_LT10())
    
    # plot LT50
    plot_LT50 <- eventReactive(input$run_button,{
      draw_plot_LT50(Calculated_LT50())
    })
    
    output$plot_LT50 <- renderPlot(plot_LT50())
    
    # plot LT90
    plot_LT90 <- eventReactive(input$run_button,{
      draw_plot_LT90(Calculated_LT90())
    })
    
    output$plot_LT90 <- renderPlot(plot_LT90())
    
    # Downloadable csv of LTs ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Lethal_Temp_Estimates",".csv", sep = "")
      },
      content = function(file) {
        write.csv(All_LTs(), file, row.names = FALSE)
      }
    )
    
    output$downloadTemplate <- downloadHandler( #allow users to download a template of the data needed for uploads
      filename <- function() {
        "DataTemplate.csv"
      },
      content <- function(file) {
        file.copy("./data/DataTemplate.csv", file)
      }
    )
  
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}

# NOTES

# a reactive expression looks like a function but it will only run the first time 
# it is called and then it caches the results until it needs to be updated

# you cant call things from within observeEvent (e.g. you cant create a "dataframe' 
# from your functions applied to input_data this way)




# EXTRA
# Attempt to do with just one file for now

# My current approach

#  data <- reactive({
#    rbindlist(lapply(input$files$datapath, fread),
#              use.names = TRUE, fill= TRUE)
#    }) # merge all the data files that have been uploaded into one
# df() <- data() %>% select(5:10)
# rename columns
#df() <- df() %>% 
#  rename(Date = "Date/Time (LST)")

#df() <- df() %>% 
#  rename(Time = "Time (LST)")

#df() <- df() %>% 
#  rename(Temp = "Temp (C)") # had to remove degree symbol from this sction (non-ASCII character)

# make df() a data frame
#df() <- as.data.frame(df())
#df()$Month <- as.numeric(df()$Month)
#df()$Day <- as.numeric(df()$Day)
#df()$Temp <- as.numeric(df()$Temp)


# sort by year, month, day, time
#df() <- df()[
#  order( df()[,2], df()[,3], df()[,4], df()[,5]),
#]

#  output$LT10 <- renderPlot({#plug df() into your code for LT10 model
#  })
#  output$LT50 <- renderPlot({#plug df() into your code for LT50 model
#  })
#  output$LT90 <- renderPlot({#plug df() into your code for LT90 model
#  })
#}



# Notes
# Gov of Canada API https://fromthebottomoftheheap.net/2016/05/24/harvesting-more-canadian-climate-data/



# other code

#"Estimations",
#fileInput(inputId='csv_input', # try with one file
#   label="Upload Hourly Temperature Data (.csv)",
#  multiple = TRUE,
#  accept = '.csv',
#  buttonLabel = "Browse...",
#  placeholder = "No file selected"),
# add action button to generate the estimations
#actionButton("estimate",
#             "Estimate"),
#plotOutput("LT10"),
#plotOutput("LT50"),
#plotOutput("LT90")))
#)


#to validata data inputs? (error if other than .csv?)
#data_input <- reactive({
#  req(input$csv_input)
# gives error code if not a .csv file
#  ext <- tools::file_ext(input$csv_input$name)
#  validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
#  fread(input$csv_input$datapath)
#})