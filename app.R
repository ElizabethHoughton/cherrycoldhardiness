#' Shiny app for cold hardiness estimations
#' 
#' Web document to calculate cold hardiness estimations
#' 
#' @details Calculates the 10, 50, and 90% lethal temperatures for 'Sweetheart' and 'Lapins' sweet cherries in the Okanagan Valley, BC, Canada based on daily air temperatures.
#' 
#' 
#' @return web application, data frame of lethal temperatures
#' 
#' @import shiny shinydashboard shinythemes utils owmr jsonlite zoo dplyr tidyr leaflet ChillModels
#' @importFrom graphics legend lines
#' 
#' @export

# For shinyapp.io
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(zoo)
library(owmr)
library(jsonlite)
library(AICcmodavg)
library(lubridate)
library(tidyr)
library(leaflet)
library(ChillModels)

# remove it from a funciton when deploying it on shinyapp.io
#cherrycoldhardiness <- function() {
# the user interface
ui <- fluidPage(theme = shinytheme("flatly"),
                # Navbar title
                titlePanel(HTML("<b>Estimating the Cold Hardiness of Sweet Cherry in Cold Climate Regions</b>")),
                navbarPage("",
                           # first tab panel
                           tabPanel(strong("Background"),
                                    h3("Sweet cherry cold hardiness estimations in the Okanagan Valley, BC", style = "color:black;font-size:27px;"),
                                    br(),
                                    p(HTML(paste0("The cold hardiness of perennial plants refers to their ability to 
                                          tolerate freezing temperatures. These models have been developed to help 
                                          estimate cold hardiness by determining the temperatures that cause cold damage to the flower buds of the 
                                          sweet cherry cultivars 'Sweetheart' and 'Lapins' located within 
                                          the Okanagan Valley of British Columbia, Canada. These models use hourly air temperature data to estimate the lethal temperatures that would result in
                                        10%, 50%, and 90% damage (LT", tags$sub("10"),", LT", tags$sub("50"),", LT", tags$sub("90"),") to the flower buds. It is important to note that estimations made using these models from the months 
                                      of May to mid- to late-September generally are not meaniningful as sweet cherry flower buds often have not yet acclimated in this region.")), style = "font-size:17px;color:black;"),
                                    br(),
                                    p(
                           
                           img(src ="seasons1.jpg", height = 255.5, width = 947.5, style="display: block; margin-left: auto; margin-right: auto;"),),
                           br(),
                           hr(),
                           p(h4("Help and feedback", style = "color:black;")),
                           p("For additional help, to submit feedback, or to report a bug, please contact: Elizabeth Houghton (elizabeth.houghton@ubc.ca). The website source content can be viewed on", style = "font-size:17px;color:black;",
                             a(href="https://github.com/ElizabethHoughton/cherry-cold-hardiness", "GitHub", style = "font-size:15px;color:teal;"), #active weblink to GitHub
                             ".", style = "font-size:15px;color:black;"),
                           p(h4("Funding", style = "color:black;"), p("This project is supported by the Canadian Agricultural Partnership, a federal-provincial-territorial initiative, as well as the BC Cherry Association and a Private Foundation.
                           The Farm Adaptation Innovator program is delivered by the Investment Agriculture Foundation of BC.",style="font-size:15px;color:black;font-size:15px;color:black;"),
                             img(src ="Funding_statement.jpg", height = 129.67, width = 705.67, style="display: block; margin-left: auto; margin-right: auto;")),
                           p("Opinions expressed in this website are those of the author and not necessarily those of the Governments of Canada and British Columbia, BC Cherry Association, 
                                   Private Foundation or the Investment Agriculture Foundation of BC. The Governments of Canada and British Columbia, BC Cherry Association, Private Foundation and the 
                                   Investment Agriculture Foundation of BC, and their directors, agents, employees, or contractors as well as the website's content contributers and web-developers, 
                                   will not be liable for any claims, damages, or losses of any kind whatsoever arising out of the use of, or reliance upon, this information.", style="text-align:center;font-size:13px;color:grey;")),

                           # second tab panel
                           tabPanel(
                             title = strong("About the Models"),
                             h3("Equations used to estimate cold hardiness", style = "color:black;font-size:27px;"),
                             br(),
                             p("Seven seasons of 'Sweetheart' and three seasons of 'Lapins' sweet cherry flower bud cold hardiness measurements from orchards in Summerland, British Columbia, Canada were used to develop these models. Additionally, 
                             hourly air temperatures were aquired from near by weather stations. To improve models fit in the spring, when flower buds rapidly lose cold hardiness, models were split into two time periods: 
                             T1 (fall to later winter) and T2 (spring). The equations of the final models used to predict flower bud lethal temperatures during T1 (fall to late winter) are as follows:", style = "font-size:17px;color:black;"),
                             br(),
                             p(strong(HTML(paste0("LT", tags$sub("10, 50, 90 (Sweetheart)"), " = &beta;&#770;", tags$sub("S0"), 
                                                  " + &beta;&#770;", tags$sub("S1"), "T", tags$sub("mean.lag1"), 
                                                  " + &beta;&#770;", tags$sub("S2"), "T", tags$sub("mean.lag3"), 
                                                  " + &beta;&#770;", tags$sub("S3"),
                                                  "CU + &beta;&#770;", tags$sub("S4"),
                                                  "FU + &epsilon;", tags$sub("S")))), style = "text-align:center;font-size:17px;color:black;"),
                             br(),
                             p(strong(HTML(paste0("LT", tags$sub("10, 50, 90 (Lapins)"), " = &beta;&#770;", tags$sub("L0"), 
                                                  " + &beta;&#770;", tags$sub("L1"), "T", tags$sub("mean.lag1"),  
                                                  " + &beta;&#770;", tags$sub("L2"), "T", tags$sub("mean.lag2"), 
                                                  "+ &beta;&#770;", tags$sub("L3"), 
                                                  "CU + &beta;&#770;", tags$sub("L4"), 
                                                  "FU + &epsilon;", tags$sub("L")))), style = "text-align:center;font-size:17px;color:black;"),
                             br(),
                             p(HTML(paste0("Where LT", tags$sub("10, 50, 90 (Sweetheart)"),"  and LT", tags$sub("10, 50, 90 (Lapins)"),"represent the lethal temperature that causes 10%, 50%, and 90% damage (LT", tags$sub("10"),", LT", tags$sub("50"),", LT", tags$sub("90"),") to 'Sweetheart' and 'Lapins' flower buds, respectively. Unique models using the same parameters were developed for LT", tags$sub("10"),", LT", tags$sub("50"),", and LT", tags$sub("90"),". &beta;", tags$sub("S0"), " and &beta;", tags$sub("L0"), " are the intercepts, &beta;", tags$sub("S1")," and &beta;", tags$sub("L1")," are the coefficients of the mean daily temperature from one day prior, &beta;", tags$sub("S2"), 
                             "  is the coefficient of the mean daily temperature from three days prior, &beta;", tags$sub("L2"), 
                             "  is the coefficient of the mean daily temperature from two days prior, &beta;", tags$sub("S3"), " and &beta;", tags$sub("L3")," are the coefficients of the accumulated chilling units, &beta;", tags$sub("4"), " are the coefficients of the log transformed accumulated forcing units, and &epsilon;", tags$sub("S"), " and &epsilon;", tags$sub("L")," are the error terms. T", tags$sub("mean.lag1"), ", T", tags$sub("mean.lag2"), " and T", tags$sub("mean.lag3"),
                             " are the values of the mean daily temperature (˚C) from one, two, and three days prior, respectively. CU is the accumulated chilling units and FU is the log transformed accumulated forcing units. 
                             Estimates of accumulated daily chilling and forcing were calculated from hourly air temperatures using chilling and heat requirement models developed by ", a(href ="https://www.researchgate.net/publication/282060426_Development_of_chilling_and_forcing_relationships_for_modeling_spring_phenology_of_apple_and_sweet_cherry", "Neilsen et al. (2015)", style = "font-size:17px;color:teal;"), 
                             ". The final best fit models used to predict LT during T2 (spring) for both 'Sweetheart' and 'Lapins' included the intercept only meaning these estimates are equivalent to the mean lethal temperatures measured during this time period. <b>Caution must be taken when using this model to predict spring lethal temperatures as limited data were available for model development during this period.</b>"
                                           )), style = "font-size:17px;color:black;"),
                             br(),
                             h3("Model evaluation", style = "color:black;font-size:27px;"),
                             br(),
                             p("Plotting the models’ predicted lethal temperatures against the actual lethal temperatures that were measured shows excellent agreement between predicted and observed LT10, LT50, and LT90 for both 'Sweetheart' and 'Lapins'.", style = "font-size:17px;color:black;"),
                             img(src ="Model_Eval.jpg", height =507.6, width = 718.4, style="display: block; margin-left: auto; margin-right: auto;"),
                             p(em("One-to-one regression of the predicted and observed lethal temperature values for 'Sweetheart' and 'Lapins'"), style = "text-align:center;font-size:14px;"),
                             br(),
                             h3("'Sweetheart' model validation", style = "color:black;font-size:27px;"),
                             br(),
                             p(HTML("In general, the models did a good job predicting lethal temperatures when compared to years of data not used in model development and to several locations within Summerland, BC.
                             However, <b>caution must be taken when using these models to predict spring lethal temperatures as limited data were available for model development during this period</b>. Lethal temperature estimates
                               were also compared to lethal temperature measurements for the varieties 'Skeena', 'Sonata', and 'Staccato'. It must be noted that although model fit looks good during the fall and winter season,
                               <b>caution must be taken when applying the 'Sweetheart' models to other varieties, especially in the spring time if bloom times vary greatly between varieties.</b>"), style = "font-size:17px;color:black;"),
                             br(),
                             img(src ="Sweetheart_Validation.jpg", height = 180,  style="display: block; margin-left: auto; margin-right: auto;"),
                             p(em("Examples of model estimates that were compared to year's of lethal temperature data that were not included in model development collected from orchards varying in elevation in Summerland, BC. 
                                  These plots show the estimated temperature that will cause 50% of the buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shade area.
                                  (a) high elevation orchard 2020-2021, (b) mid-elevation orchard 2020-2021, (c) low-elevation orchard 2021-2022."), style = "text-align:center;font-size:14px;"),
                             br(),
                             img(src ="Sweetheart_Validation_Other.jpg", height = 180,  style="display: block; margin-left: auto; margin-right: auto;"),
                             p(em("'Sweetheart' lethal temperature estimates during T1 compared to other varities of sweet cherry sampled from orchards located in Summerland, BC. 
                                  The 95% confidence intervals on the estimations are shown by the shade area.
                                  (a) 'Skeena' variety 2015-2016, (b) 'Sonata' variety 2015-2016, (c) 'Staccato' variety 2015-2016."), style = "text-align:center;font-size:14px;"),
                             br(),
                             h3("'Lapins' model validation", style = "color:black;font-size:27px;"),
                             br(),
                             p(HTML("Much like the 'Sweetheart' models, these models did a fairly good job predicting lethal temperatures when compared to one year of data that was not used in model development. Again, <b>caution must be taken when using these model to predict spring lethal temperatures as limited data were available during this period.</b> 
                               For comparison, these model predictions were also plotted against lethal temperatures measured from other varieties."), style = "font-size:17px;color:black;"),
                             br(),
                             img(src ="Lapins_Validation.jpg", height = 180,  style="display: block; margin-left: auto; margin-right: auto;"),
                             p(em("Examples of model estimates that were compared to one year of lethal temperature data that was not included in model development collected from an AAFC research orchard in Summerland, BC in the 2014-2015 season. 
                                  These plots show the estimated temperature that will cause 10%, 50%, and 90% of the buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shade area.
                                  (a) 10% lethal temperature, (b) 50% lethal temperature, (c) 90% lethal temperature."), style = "text-align:center;font-size:14px;"),
                             br(),
                             img(src ="Lapins_Validation_Other.jpg", height = 180,  style="display: block; margin-left: auto; margin-right: auto;"),
                             p(em("'Lapins' lethal temperature estimates during T1 compared to other varities of sweet cherry sampled from orchards located in Summerland, BC. 
                                  The 95% confidence intervals on the estimations are shown by the shade area.
                                  (a) 'Skeena' variety 2015-2016, (b) 'Sonata' variety 2015-2016, (c) 'Staccato' variety 2015-2016."), style = "text-align:center;font-size:14px;"),
                            
                             
                             ),
                           
                           
                           # third tab panel
                           tabPanel(strong("How to Use"),
                                    h3("Data requirements", style = "color:black;font-size:27px;"),
                                    br(),
                                    p("This model requires hourly air temperature data starting in September of the season you would like to estimate
                                        lethal temperatures. If you are interested in using your own hourly temperature data, it must be uploaded as a CSV file in the same format as
                                        the following example. A correct Climate ID is not required for this model to run, however, this template is in the same format that climate data is provided from the", style = "font-size:17px;color:black;",  
                                      a(href="https://climate.weather.gc.ca/historical_data/search_historic_data_e.html", "Government of Canada's historical climate database", style = "font-size:17px;color:teal;"),
                                        ". A downloadable CSV file template has also been provided for you to use.", style = "font-size:17px;color:black;"), 
                                    br(),
                                    img(src ="Data_example.jpg", height = 181.44, width = 820.08, style="display: block; margin-left: auto; margin-right: auto;"),  
                                    br(),
                                    br(),
                                    downloadButton("downloadTemplate", 
                                                   "Download file template",  style="width:33%; display: block; margin-left: auto; margin-right: auto;"),
                                    br(),
                                    h3("Don't have your own weather data?", style = "color:black;font-size:27px;"),
                                    br(),
                                    p("Choose one of four locations currently available and hourly temperature data from the", style = "font-size:17px;color:black;",
                                      a(href="https://climate.weather.gc.ca/historical_data/search_historic_data_e.html", "Government of Canada's historical climate database", style = "font-size:17px;color:teal;"),
                                      "will be used in these models. You can find the exact weather stations used at each location here:", style = "font-size:17px;color:black;",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2007-11-30%7C2021-08-03&dlyRange=2005-01-01%7C2021-08-02&mlyRange=%7C&StationID=46987&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=2&searchMethod=contains&Month=8&Day=3&txtStationName=vernon&timeframe=1&Year=2021", "Vernon", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-12-13%7C2021-08-03&dlyRange=2013-12-16%7C2021-08-02&mlyRange=%7C&StationID=51117&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=16&searchMethod=contains&Month=8&Day=3&txtStationName=kelowna&timeframe=1&Year=2021", "Kelowna", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=1994-02-01%7C2021-08-03&dlyRange=1990-06-01%7C2021-08-02&mlyRange=1990-01-01%7C2006-11-01&StationID=979&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=3&searchMethod=contains&Month=8&Day=3&txtStationName=summerland&timeframe=1&Year=2021", "Summerland", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-05-07%7C2021-08-03&dlyRange=2012-05-10%7C2021-08-02&mlyRange=%7C&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=1&searchMethod=contains&Month=8&Day=3&txtStationName=penticton&timeframe=1&Year=2021", "Penticton", style = "font-size:17px;color:teal;"),
                                      ". If you select one of these weather stations, three days worth of weather forecast from", style = "font-size:17px;color:black;",
                                      a(href="https://openweathermap.org", "OpenWeather", style = "font-size:17px;color:teal;"),
                                      HTML(" will also be inputted to make predictions using these models. <b>This option may take several minutes to load</b>.")
                                    )),
                          

                           
                           # fourth tab panel
                           tabPanel(
                             title = strong("Estimations"),
                             sidebarLayout(
                               sidebarPanel(
                                 title = "Inputs",
                                 
                                 # Area to upload a .csv file with hourly temperature data to run through models 
                                 fileInput("csv_input", "Import CSV file of climate data", accept = ".csv", multiple = TRUE),
                                 
                                 # Drop down menu to select a location to pull weather data from Gov. of Canada's Historic Climate database
                                 selectInput("location", "Or choose closest weather station", choices = c("Not Selected"= "not_sel",
                                                                                                          "Penticton"="penticton",
                                                                                                          "Summerland"="summerland",
                                                                                                          "Kelowna"="kelowna",
                                                                                                          "Vernon"="vernon"
                                 )),
                                 
                                 selectInput("variety", "Select sweet cherry variety", choices = c("Sweetheart"="sweetheart",
                                                                                                   "Lapins"="lapins"
                                 )),
                                 
                                 
                                 # Button to run the analysis
                                 actionButton("run_button", 
                                              "Run Analysis",
                                              style="margin-top: 8%"),
                                 # Button for saving any calculated corrections as a .csv
                                 downloadButton("downloadData", 
                                                "Save results",
                                                style= "margin-top: 8%"),
                                 h5("The results may take several minutes to load", style = "font-size:14px;margin-top:7%"),
                                 
                                 # plot leaflet object (map)
                                 br(),
                                 h5(HTML("<b>Weather station locations</b>"), style = "font-size:15px;margin-top:10%"),
                                 leafletOutput("map")
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel(
                                     title = (HTML(paste0("LT", tags$sub("10")))),
                                     plotOutput("plot_LT10", width= "100%", height=600),
                                     verbatimTextOutput("description1"), #verbatimTextOutput can be used in a panel
                                     tags$style(type="text/css", "#description1 {white-space: pre-wrap; word-break: break-word; # wraps text and makes sure it breaks with each word (not cutting up words)
                                                background-color: rgba(255,255,255,0.40); color: black; border-style: none; font-family: calibri; font-size: 16px;}")), 
                                   tabPanel(
                                     title = (HTML(paste0("LT", tags$sub("50")))),
                                     plotOutput("plot_LT50", width= "100%", height=600),
                                     verbatimTextOutput("description2"),
                                     tags$style(type="text/css", "#description2 {white-space: pre-wrap; word-break: break-word; # wraps text and makes sure it breaks with each word (not cutting up words)
                                                background-color: rgba(255,255,255,0.40); color: black; border-style: none; font-family: calibri; font-size: 16px;}")), 
                                   tabPanel(
                                     title = (HTML(paste0("LT", tags$sub("90")))),
                                     plotOutput("plot_LT90", width= "100%", height=600),
                                     verbatimTextOutput("description3"),
                                     tags$style(type="text/css", "#description3 {white-space: pre-wrap; word-break: break-word; # wraps text and makes sure it breaks with each word (not cutting up words)
                                                background-color: rgba(255,255,255,0.40); color: black; border-style: none; font-family: calibri; font-size: 16px;}"))
                                 )
                                 
                               )
                             )
                           )
)
)
                                 

#############################################################################
# Set values to use to select data range for uploaded data and data accessed through the Gov. of Canada's Historic Climate database
current_year <- as.numeric(format(Sys.Date(), "%Y"))
last_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
current_month <- as.numeric(format(Sys.Date(), "%m"))
today_date <- as.POSIXct(format(Sys.Date(), format = "%Y-%m-%d"))
yesterday <- as.POSIXct(format(Sys.Date()-1, format = "%Y-%m-%d"))
Year <- c(last_year)
Year2 <- c(current_year)
Month<- 8
Day <- 1
TDAY <- c(today_date)
YDAY <- c(yesterday)
select_date1 <- data.frame(Year, Year2, Month, Day)
FROM <- (ISOdatetime(select_date1$Year, select_date1$Month, select_date1$Day, hour=1, min=0, sec=0))
FROM2 <- (ISOdatetime(select_date1$Year2, select_date1$Month, select_date1$Day, hour=1, min=0, sec=0))
select_date1$FROM <- FROM
select_date1$YDAY <-YDAY

# select_date1 will be used when users access this website from January-June
# for selecting dates from the year prior to this year
select_date1$YDAY <- as.POSIXct(select_date1$YDAY, format="%Y-%m-%d %H:%M:%S")
select_date1$FROM <- as.POSIXct(select_date1$FROM, format="%Y-%m-%d %H:%M:%S")

# select_date2 will be used when users access this website from June-December
# for selecting dates in the same year
select_date2 <- select_date1
select_date2$FROM <- FROM2

# For retrieving data from the Gov. of Canada's Historic Climate database
# Store api key in an environment variable called OWM_API_KEY (this api is your personal one from the openweathermaps site)
Sys.setenv(OWM_API_KEY = "d99f77bed68ff58e6dec11cdc2bbb127")

# Functions to use to draw plots in the server
# plot LT10

draw_plot_LT10 <- function(Calculated_LT10, Calculated_LT90)
{#select for values greater than or equal to Sys.Date()
  Calculated_LT10_1 <- Calculated_LT10 %>% dplyr::filter(YYYYMMDD >= as.Date(Sys.Date()))
  Calculated_LT10_2 <- Calculated_LT10 %>% dplyr::filter(YYYYMMDD <= as.Date(Sys.Date()))
  par(mar = c(3.6, 4.8, 4.1, 2.1)) # set margins so that there is room for y axis label 
  plot(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10, # CHANGE THIS IF YOU RENAME YOUR data_input 
       main= "Lethal Temperature for 10% Bud Damage",
       pch= NA,
       xlab="", 
       ylab= expression("LT"[10]*" (˚C)"),
       xlim=c((as.Date(min(Calculated_LT10$YYYYMMDD))+30), as.Date(max(Calculated_LT10$YYYYMMDD)) - 1),
       cex.axis=1.5,
       cex.lab=1.5,
       cex.main=1.75,
       ylim=c(((min(Calculated_LT90$LT90))-1), 0))
  polygon(c(Calculated_LT10_2$YYYYMMDD, rev(Calculated_LT10_2$YYYYMMDD)), c(Calculated_LT10_2$LT10.CILower, rev(Calculated_LT10_2$LT10.CIUpper)), col="grey86", border= FALSE) # add CI as band of colour not lines
  polygon(c(Calculated_LT10_1$YYYYMMDD, rev(Calculated_LT10_1$YYYYMMDD)), c(Calculated_LT10_1$LT10.CILower, rev(Calculated_LT10_1$LT10.CIUpper)), col="mistyrose", border=FALSE) # add CI as band of colour not lines
  lines(Calculated_LT10_2$YYYYMMDD, Calculated_LT10_2$LT10, lty = 1, lwd=2, col = Calculated_LT10_2$Colour)
  lines(Calculated_LT10_1$YYYYMMDD, Calculated_LT10_1$LT10, lty = 1, lwd=2, col = Calculated_LT10_1$Colour)
  lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$Temp_min, lty = 1, col= "blue", lwd=2) # daily minimum temp
  legend("bottomleft", legend=c(expression(paste('LT'[10], ' (Recorded Weather)')),
                                expression(paste('LT'[10], ' (Weather Predictions)')),
                                expression(paste('Daily Minimum Temperature'))),
         col=c("black", "red","blue"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), cex=1.3, bty = "n") # add legend
}

# plot LT50
draw_plot_LT50 <- function(Calculated_LT50, Calculated_LT90)
{#select for values greater than or equal to Sys.Date()
  Calculated_LT50_1 <- Calculated_LT50 %>% dplyr::filter(YYYYMMDD50 >= as.Date(Sys.Date()))
  Calculated_LT50_2 <- Calculated_LT50 %>% dplyr::filter(YYYYMMDD50 <= as.Date(Sys.Date()))
  par(mar = c(3.6, 4.8, 4.1, 2.1)) # set margins so that there is room for y axis label 
  plot(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50, # CHANGE THIS IF YOU RENAME YOUR data_input 
       main= "Lethal Temperature for 50% Bud Damage",
       pch= NA,
       xlab="", 
       ylab= expression("LT"[50]*" (˚C)"),
       xlim=c((as.Date(min(Calculated_LT50$YYYYMMDD50))+30), as.Date(max(Calculated_LT50$YYYYMMDD50)) - 1),
       cex.axis=1.5,
       cex.lab=1.5,
       cex.main=1.75,
       ylim=c(((min(Calculated_LT90$LT90))-1), 0))
  polygon(c(Calculated_LT50_2$YYYYMMDD50, rev(Calculated_LT50_2$YYYYMMDD50)), c(Calculated_LT50_2$LT50.CILower, rev(Calculated_LT50_2$LT50.CIUpper)), col="grey86", border= FALSE) # add CI as band of colour not lines
  polygon(c(Calculated_LT50_1$YYYYMMDD50, rev(Calculated_LT50_1$YYYYMMDD50)), c(Calculated_LT50_1$LT50.CILower, rev(Calculated_LT50_1$LT50.CIUpper)), col="mistyrose", border=FALSE) # add CI as band of colour not lines
  lines(Calculated_LT50_2$YYYYMMDD50, Calculated_LT50_2$LT50, lty = 1, lwd=2, col = Calculated_LT50_2$Colour)
  lines(Calculated_LT50_1$YYYYMMDD50, Calculated_LT50_1$LT50, lty = 1, lwd=2, col = Calculated_LT50_1$Colour)
  lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$Temp_min, lty = 1, col= "blue", lwd=2) # daily minimum temp
  legend("bottomleft", legend=c(expression(paste('LT'[50], ' (Recorded Weather)')),
                                expression(paste('LT'[50], ' (Weather Predictions)')),
                                expression(paste('Daily Minimum Temperature'))),
         col=c("black", "red","blue"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), cex=1.3, bty = "n") # add legend
}

# plot LT90
draw_plot_LT90 <- function(Calculated_LT90)
{#select for values greater than or equal to Sys.Date()
  Calculated_LT90_1 <- Calculated_LT90 %>% dplyr::filter(YYYYMMDD90 >= as.Date(Sys.Date()))
  Calculated_LT90_2 <- Calculated_LT90 %>% dplyr::filter(YYYYMMDD90 <= as.Date(Sys.Date()))
  par(mar = c(3.6, 4.8, 4.1, 2.1)) # set margins so that there is room for y axis label 
  plot(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90, # CHANGE THIS IF YOU RENAME YOUR data_input 
       main= "Lethal Temperature for 90% Bud Damage",
       pch= NA,
       xlab="", 
       ylab= expression("LT"[90]*" (˚C)"),
       xlim=c((as.Date(min(Calculated_LT90$YYYYMMDD90))+ 30), as.Date(max(Calculated_LT90$YYYYMMDD90) - 1)),
       cex.axis=1.5,
       cex.lab=1.5,
       cex.main=1.75,
       ylim=c(((min(Calculated_LT90$LT90))-1), 0))
  polygon(c(Calculated_LT90_2$YYYYMMDD90, rev(Calculated_LT90_2$YYYYMMDD90)), c(Calculated_LT90_2$LT90.CILower, rev(Calculated_LT90_2$LT90.CIUpper)), col="grey86", border= FALSE) # add CI as band of colour not lines
  polygon(c(Calculated_LT90_1$YYYYMMDD90, rev(Calculated_LT90_1$YYYYMMDD90)), c(Calculated_LT90_1$LT90.CILower, rev(Calculated_LT90_1$LT90.CIUpper)), col="mistyrose", border=FALSE) # add CI as band of colour not lines
  lines(Calculated_LT90_2$YYYYMMDD90, Calculated_LT90_2$LT90, lty = 1, lwd=2, col = Calculated_LT90_2$Colour)
  lines(Calculated_LT90_1$YYYYMMDD90, Calculated_LT90_1$LT90, lty = 1, lwd=2, col = Calculated_LT90_1$Colour)
  lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$Temp_min, lty = 1, col= "blue", lwd=2) # daily minimum temp
  legend("bottomleft", legend=c(expression(paste('LT'[90], ' (Recorded Weather)')),
                                expression(paste('LT'[90], ' (Weather Predictions)')),
                                expression(paste('Daily Minimum Temperature'))),
         col=c("black", "red","blue"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), cex=1.3, bty = "n") # add legend
  
}

###############################################################################
# Currently placing externally accessed functions in the app.R file to simply deploy to shinyapp.io

# CU_FU function to calculate chill and forcing units
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
  
  data_input_mean <- data_input #save to calculate daily max and lag temps
  
  # Calculate CU
  
  data_input$CU <- ChillModels::cherry_model(data_input$Temp, total=FALSE)
  
  # Make a new column (CU_acc) that is the accumulation of all the calculated CU from each day prior and the current day
  
  data_input <- data.frame(data_input, CU_acc=cumsum(data_input$CU))
  
  #remove the first NA value with na.omit
  data_input <- stats::na.omit(data_input)
  
  # Anything after the minimum CU_acc, assign the accumulative CU, everything before assign 0
  # which.min outputs the row that the minimum value exists in
  
  # if the length of the data frame when selecting for values after the minimum CU_acc value is 
  # greater than 0, assign CU_acc_final to be the cumulative sum of CU, if it is shorter than 1,
  # assign CU_acc_final a value of zero
  
  if (nrow(data_input[(which.min(data_input$CU_acc)):nrow(data_input), ]) > 1){
    data_input_2 <- data_input[(which.min(data_input$CU_acc) + 1):nrow(data_input), ]
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
  
  # assign forcing a value of below 2000 chill accumulation to 0 to give early forcing accumulated during warm snaps in the winter less weight
  data_input_final$FU_acc_0 <- 0
  
  data_input_final$FU_acc <- ifelse(data_input_final$CU_acc_final <= 2000, # if cherry model is less than 2000
                                    data_input_final$FU_acc_0, # assign Sweetheart_cherry_forcingBB_final to 0
                                    data_input_final$FU_acc) # otherwise give it its original values
  
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
  # mean temp
  data_input_dailymean <- stats::aggregate(Temp ~ Year + Month + Day, data_input_mean, mean)
  # Min temp
  data_input_dailymin <- stats::aggregate(Temp ~ Year + Month + Day, data_input_mean, min)
  
  # rename Temp to Temp_mean and Temp to Temp_min
  data_input_dailymean <- data_input_dailymean %>% 
    dplyr::rename(Temp_mean = Temp)
  
  data_input_dailymin <- data_input_dailymin %>% 
    dplyr::rename(Temp_min = Temp)
  
  # merge mean and min by Year, Month, Day
  data_input_dailymean <- merge(data_input_dailymean, data_input_dailymin, by = c("Year", "Month", "Day"), all = TRUE)
  
  # sort by year, month, day
  data_input_dailymean <- data_input_dailymean[
    order(data_input_dailymean[,1], data_input_dailymean[,2], data_input_dailymean[,3]),
  ]
  
  
  # Assign 1st order temperature lags for mean temp
  # For mean temps
  data_input_dailymean <- data.frame(data_input_dailymean, Temp_mean.lag1 = dplyr::lag(data_input_dailymean$Temp_mean))
  data_input_dailymean <- data.frame(data_input_dailymean, Temp_mean.lag2 = dplyr::lag(data_input_dailymean$Temp_mean, 2))
  data_input_dailymean <- data.frame(data_input_dailymean, Temp_mean.lag3 = dplyr::lag(data_input_dailymean$Temp_mean, 3))
  
  # Merge CU_FU and daily mean temps into one
  CU_FU1 <- merge(data_input_dailymean, data_input_daily, by = c("Year", "Month", "Day"), all = TRUE)
  
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
  #(Temp_mean.lag1)
  #(CU_1119) 
  #(FU_acc_log)
  #(FU_state)
  #(Temp_min) for graphs
  
  CUFUcalculations <- CUFUcalculations %>% dplyr::select(c("Station.Name",
                                                           "YYYYMMDD", 
                                                           "Temp_min",
                                                           "Temp_mean.lag1", 
                                                           "Temp_mean.lag2",
                                                           "Temp_mean.lag3",
                                                           "CU_1119", 
                                                           "FU_acc", 
                                                           "FU_acc_log", 
                                                           "FU_state"))
  
  # remove first row to deal with lag NA             
  CUFUcalculations <- CUFUcalculations[-1,]
  
  # if there is an issue with missing data, this will fill the NA value with the value from the row previous
  
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_mean.lag1, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_mean.lag2, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_mean.lag3, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(CU_1119, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_acc, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_acc_log, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(FU_state, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Station.Name, .direction= c("up"))
  
  # rename columns to match saved models
  CUFUcalculations <- CUFUcalculations %>%
    dplyr::rename(cherry_model_final = CU_1119)

  CUFUcalculations <- CUFUcalculations %>%
    dplyr::rename(Sweetheart_cherry_forcingBB_final_log = FU_acc_log)
  
  # Remove last day in case there is only a partial day of data
  CUFUcalculations <- head(CUFUcalculations, -1)
  
  # assign all Station.Name values to the value of the last row (then it will be e.g. 'Kelowna' not 'KELOWNA UBCO')
  CUFUcalculations$Station.Name <- as.character(last(CUFUcalculations$Station.Name))
  CUFUcalculations
}

###############################################################################


CH_LT10 <- function(Calculated_CU_FU10=NULL){
  # load in the fitted Model10
  load("./Model10_Sweetheart1.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  # Calculated_CU_FU10.1 <- subset(Calculated_CU_FU10, FU_acc < 30)
  PredictLT10 <- as.data.frame(AICcmodavg::predictSE.gls(Model10.m, newdata = Calculated_CU_FU10, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT10$LT10.CIUpper <- (PredictLT10$fit + PredictLT10$se.fit*1.96)
  PredictLT10$LT10.CILower <- (PredictLT10$fit - PredictLT10$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU10
  PredictLT10$YYYYMMDD <- Calculated_CU_FU10$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU10
  PredictLT10$Temp_min <- Calculated_CU_FU10$Temp_min
  # rename fit to LT50
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10_standard_error = "se.fit")
  PredictLT10$Colour = "black"
  PredictLT10$Colour[PredictLT10$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT10$Station.Name <- Calculated_CU_FU10$Station.Name # add station name just to LT10 so you can cbind LT10, 50, and 90 for download without name duplication
  PredictLT10$FU_acc <- Calculated_CU_FU10$FU_acc

  PredictLT10$LT10.30 <- -4.30928571
  PredictLT10$LT10.CIUpper.30 <- -3.83034136
  PredictLT10$LT10.CILower.30 <- -4.7882301
  PredictLT10$LT10_standard_error.30 <- 0.2443594
  
  PredictLT10$LT10 <- ifelse(PredictLT10$FU_acc < 30, # if FU_accl is less than 30
                             PredictLT10$LT10, # assign  it its original values
                             PredictLT10$LT10.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CIUpper <- ifelse(PredictLT10$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                     PredictLT10$LT10.CIUpper, # assign  it its original values
                                     PredictLT10$LT10.CIUpper.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CILower <- ifelse(PredictLT10$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                     PredictLT10$LT10.CILower,# assign  it its original values
                                     PredictLT10$LT10.CILower.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10_standard_error <- ifelse(PredictLT10$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                            PredictLT10$LT10_standard_error, # assign  it its original values
                                            PredictLT10$LT10_standard_error.30) # otherwise give it LT10 to the value of that averaged model
  PredictLT10
}

###############################################################################

# CH_LT50 to calculate the 50% lethal temperature

CH_LT50 <- function(Calculated_CU_FU50=NULL){
  # load in the fitted Model50
  load("./Model50_Sweetheart1.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- as.data.frame(AICcmodavg::predictSE.gls(Model50.m, newdata = Calculated_CU_FU50, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT50$LT50.CIUpper <- (PredictLT50$fit + PredictLT50$se.fit*1.96)
  PredictLT50$LT50.CILower <- (PredictLT50$fit - PredictLT50$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU50
  PredictLT50$YYYYMMDD50 <- Calculated_CU_FU50$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU50
  PredictLT50$Temp_min <- Calculated_CU_FU50$Temp_min
  # rename fit to LT50
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50_standard_error = "se.fit")
  PredictLT50$Colour = "black"
  PredictLT50$Colour[PredictLT50$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT50$FU_acc <- Calculated_CU_FU50$FU_acc
  
  PredictLT50$LT50.30 <- -5.832143
  PredictLT50$LT50.CIUpper.30 <- -5.337064
  PredictLT50$LT50.CILower.30 <- -6.327222
  PredictLT50$LT50_standard_error.30 <- 0.2525914
  
  
  PredictLT50$LT50 <- ifelse(PredictLT50$FU_acc < 30, # if FU_accl is less than 30
                             PredictLT50$LT50, # assign  it its original values
                             PredictLT50$LT50.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50.CIUpper <- ifelse(PredictLT50$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                     PredictLT50$LT50.CIUpper, # assign  it its original values
                                     PredictLT50$LT50.CIUpper.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50.CILower <- ifelse(PredictLT50$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                     PredictLT50$LT50.CILower,# assign  it its original values
                                     PredictLT50$LT50.CILower.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50_standard_error <- ifelse(PredictLT50$FU_acc < 30, # if FU_accl is greater than or euqal to 30
                                            PredictLT50$LT50_standard_error, # assign  it its original values
                                            PredictLT50$LT50_standard_error.30) # otherwise give it LT50 to the value of that averaged model
  PredictLT50
}


###############################################################################

# CH_LT90 to calculate the 90% lethal temperature

CH_LT90 <- function(Calculated_CU_FU90=NULL){
  # load in the fitted Model90
  load("./Model90_Sweetheart1.RData")
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- as.data.frame(AICcmodavg::predictSE.gls(Model90.m, newdata = Calculated_CU_FU90, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT90$LT90.CIUpper <- (PredictLT90$fit + PredictLT90$se.fit*1.96)
  PredictLT90$LT90.CILower <- (PredictLT90$fit - PredictLT90$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU90
  PredictLT90$YYYYMMDD90 <- Calculated_CU_FU90$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU90
  PredictLT90$Temp_min <- Calculated_CU_FU90$Temp_min
  # rename fit to LT50
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90_standard_error = "se.fit")
  PredictLT90$Colour = "black"
  PredictLT90$Colour[PredictLT90$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT90$FU_acc <- Calculated_CU_FU90$FU_acc
  
  PredictLT90$LT90.30 <- -7.352857
  PredictLT90$LT90.CIUpper.30 <- -6.58457
  PredictLT90$LT90.CILower.30 <- -8.121145
  PredictLT90$LT90_standard_error.30 <- 0.3919834
  
  PredictLT90$LT90 <- ifelse(PredictLT90$FU_acc < 30, # if FU_accl is less than 30
                             PredictLT90$LT90, # assign  it its original values
                             PredictLT90$LT90.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90.CIUpper <- ifelse(PredictLT90$FU_acc < 30, # if FU_accl is less than 30
                                     PredictLT90$LT90.CIUpper, # assign  it its original values
                                     PredictLT90$LT90.CIUpper.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90.CILower <- ifelse(PredictLT90$FU_acc < 30, # if FU_accl is less than 30
                                     PredictLT90$LT90.CILower,# assign  it its original values
                                     PredictLT90$LT90.CILower.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90_standard_error <- ifelse(PredictLT90$FU_acc < 30, # if FU_accl is less than 30
                                            PredictLT90$LT90_standard_error, # assign  it its original values
                                            PredictLT90$LT90_standard_error.30) # otherwise give it LT90 to the value of that averaged model

  

  PredictLT90
}


###############################################################################


CH_LT10_lapins <- function(Calculated_CU_FU10=NULL){
  # load in the fitted Model10
  load("./Model10_Lapins1.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  # Calculated_CU_FU10.1 <- subset(Calculated_CU_FU10, FU_acc < 30)
  PredictLT10 <- as.data.frame(AICcmodavg::predictSE.gls(Model10.m, newdata = Calculated_CU_FU10, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT10$LT10.CIUpper <- (PredictLT10$fit + PredictLT10$se.fit*1.96)
  PredictLT10$LT10.CILower <- (PredictLT10$fit - PredictLT10$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU10
  PredictLT10$YYYYMMDD <- Calculated_CU_FU10$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU10
  PredictLT10$Temp_min <- Calculated_CU_FU10$Temp_min
  # rename fit to LT50
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT10 <- PredictLT10 %>% 
    dplyr::rename(LT10_standard_error = "se.fit")
  PredictLT10$Colour = "black"
  PredictLT10$Colour[PredictLT10$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT10$Station.Name <- Calculated_CU_FU10$Station.Name # add station name just to LT10 so you can cbind LT10, 50, and 90 for download without name duplication
  PredictLT10$FU_acc <- Calculated_CU_FU10$FU_acc
  
  PredictLT10$LT10.30 <- -4.08375
  PredictLT10$LT10.CIUpper.30 <- -3.497831
  PredictLT10$LT10.CILower.30 <- -4.669669
  PredictLT10$LT10_standard_error.30 <- 0.2989382
  
  PredictLT10$LT10 <- ifelse(PredictLT10$FU_acc < 25, # if FU_accl is less than 25
                             PredictLT10$LT10, # assign  it its original values
                             PredictLT10$LT10.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CIUpper <- ifelse(PredictLT10$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT10$LT10.CIUpper, # assign  it its original values
                                     PredictLT10$LT10.CIUpper.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CILower <- ifelse(PredictLT10$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT10$LT10.CILower,# assign  it its original values
                                     PredictLT10$LT10.CILower.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10_standard_error <- ifelse(PredictLT10$FU_acc < 25, # if FU_accl is less than 25
                                            PredictLT10$LT10_standard_error, # assign  it its original values
                                            PredictLT10$LT10_standard_error.30) # otherwise give it LT10 to the value of that averaged model
  PredictLT10
}


###############################################################################

# CH_LT50 to calculate the 50% lethal temperature

CH_LT50_lapins <- function(Calculated_CU_FU50=NULL){
  # load in the fitted Model50
  load("./Model50_Lapins1.RData")
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  PredictLT50 <- as.data.frame(AICcmodavg::predictSE.gls(Model50.m, newdata = Calculated_CU_FU50, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT50$LT50.CIUpper <- (PredictLT50$fit + PredictLT50$se.fit*1.96)
  PredictLT50$LT50.CILower <- (PredictLT50$fit - PredictLT50$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU50
  PredictLT50$YYYYMMDD50 <- Calculated_CU_FU50$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU50
  PredictLT50$Temp_min <- Calculated_CU_FU50$Temp_min
  # rename fit to LT50
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT50 <- PredictLT50 %>% 
    dplyr::rename(LT50_standard_error = "se.fit")
  PredictLT50$Colour = "black"
  PredictLT50$Colour[PredictLT50$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT50$FU_acc <- Calculated_CU_FU50$FU_acc
  
  PredictLT50$LT50.30 <- -5.19125
  PredictLT50$LT50.CIUpper.30 <- -4.544011
  PredictLT50$LT50.CILower.30 <- -5.838489
  PredictLT50$LT50_standard_error.30 <- 0.3302242
  
  
  PredictLT50$LT50 <- ifelse(PredictLT50$FU_acc < 25, # if FU_accl is less than 25
                             PredictLT50$LT50, # assign  it its original values
                             PredictLT50$LT50.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50.CIUpper <- ifelse(PredictLT50$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT50$LT50.CIUpper, # assign  it its original values
                                     PredictLT50$LT50.CIUpper.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50.CILower <- ifelse(PredictLT50$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT50$LT50.CILower,# assign  it its original values
                                     PredictLT50$LT50.CILower.30) # otherwise give it LT50 to the value of that averaged model
  
  PredictLT50$LT50_standard_error <- ifelse(PredictLT50$FU_acc < 25, # if FU_accl is less than 25
                                            PredictLT50$LT50_standard_error, # assign  it its original values
                                            PredictLT50$LT50_standard_error.30) # otherwise give it LT50 to the value of that averaged model
  PredictLT50
}


###############################################################################

# CH_LT90 to calculate the 90% lethal temperature

CH_LT90_lapins <- function(Calculated_CU_FU90=NULL){
  # load in the fitted Model90
  load("./Model90_Lapins1.RData")
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  PredictLT90 <- as.data.frame(AICcmodavg::predictSE.gls(Model90.m, newdata = Calculated_CU_FU90, se= TRUE))
  # Calculate confidence intervals and create data frames out of them
  PredictLT90$LT90.CIUpper <- (PredictLT90$fit + PredictLT90$se.fit*1.96)
  PredictLT90$LT90.CILower <- (PredictLT90$fit - PredictLT90$se.fit*1.96)
  # add the YYYYMMDD column from Calculated_CU_FU90
  PredictLT90$YYYYMMDD90 <- Calculated_CU_FU90$YYYYMMDD
  # add the Temp_min column from Calculated_CU_FU90
  PredictLT90$Temp_min <- Calculated_CU_FU90$Temp_min
  # rename fit to LT50
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90 = "fit")
  # rename se.fit to LT50_standard_error
  PredictLT90 <- PredictLT90 %>% 
    dplyr::rename(LT90_standard_error = "se.fit")
  PredictLT90$Colour = "black"
  PredictLT90$Colour[PredictLT90$YYYYMMDD >= as.Date(Sys.Date())] = "red"
  PredictLT90$FU_acc <- Calculated_CU_FU90$FU_acc
  
  PredictLT90$LT90.30 <- -6.3
  PredictLT90$LT90.CIUpper.30 <- -5.390393
  PredictLT90$LT90.CILower.30 <- -7.209607
  PredictLT90$LT90_standard_error.30 <- 0.4640851

  
  PredictLT90$LT90 <- ifelse(PredictLT90$FU_acc < 25, # if FU_accl is less than 25
                             PredictLT90$LT90, # assign  it its original values
                             PredictLT90$LT90.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90.CIUpper <- ifelse(PredictLT90$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT90$LT90.CIUpper, # assign  it its original values
                                     PredictLT90$LT90.CIUpper.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90.CILower <- ifelse(PredictLT90$FU_acc < 25, # if FU_accl is less than 25
                                     PredictLT90$LT90.CILower,# assign  it its original values
                                     PredictLT90$LT90.CILower.30) # otherwise give it LT90 to the value of that averaged model
  
  PredictLT90$LT90_standard_error <- ifelse(PredictLT90$FU_acc < 25, # if FU_accl is less than 25
                                            PredictLT90$LT90_standard_error, # assign  it its original values
                                            PredictLT90$LT90_standard_error.30) # otherwise give it LT90 to the value of that averaged model
  
  
  
  PredictLT90
}


###############################################################################
# This function is used to generate a URL to download the Gov. of Canada's Historic Climate data base
# Modified code originally written by Gavin Simpson that can be found at: https://gist.github.com/gavinsimpson/8c13e3c5f905fd67cf85

genURLS <- function(id, start, end, timeframe = c("hourly", "daily", "monthly")) {
  years <- seq(start, end, by = 1)
  nyears <- length(years)
  timeframe <- match.arg(timeframe)
  if (isTRUE(all.equal(timeframe, "hourly"))) {
    years <-  rep(years, each = 12)
    months <- rep(1:12, times = nyears)
    ids <- rep(id, nyears * 12)
  } else if (isTRUE(all.equal(timeframe, "daily"))) {
    months <- 1                      # this is essentially arbitrary & ignored if daily
    ids <- rep(id, nyears)
  } else {
    years <- start                   # again arbitrary, for monthly it just gives you all data
    months <- 1                      # and this is also ignored
    ids <- id
  }
  timeframe <- match(timeframe, c("hourly", "daily", "monthly"))
  URLS <- paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=", id,
                 "&Year=", years,
                 "&Month=", months,
                 "&Day=14",
                 "&format=csv",
                 "&timeframe=", timeframe,
                 "&submit=%20Download+Data"## need this stoopid thing as of 11-May-2016
  )
  list(urls = URLS, ids = ids, years = years, months = rep(months, length.out = length(URLS)))
}

###############################################################################
# This function is used to get the temperature data from the generated URL to access the Gov. of Canada's Historic Climate data base
# Modified code originally written by Gavin Simpson that can be found at: https://gist.github.com/gavinsimpson/8c13e3c5f905fd67cf85


getData <- function(stations, folder, timeframe = c("hourly", "daily", "monthly"), verbose = TRUE, delete = TRUE) {
  timeframe <- match.arg(timeframe)
  ## form URLS
  urls <- lapply(seq_len(NROW(stations)),
                 function(i, stations, timeframe) {
                   genURLS(stations$StationID[i],
                           stations$start[i],
                           stations$end[i], timeframe = timeframe)
                 }, stations = stations, timeframe = timeframe)
  
  ## check the folder exists and try to create it if not
  if (!file.exists(folder)) {
    warning(paste("Directory:", folder,
                  "doesn't exist. Will create it"))
    fc <- try(dir.create(folder))
    if (inherits(fc, "try-error")) {
      stop("Failed to create directory '", folder,
           "'. Check path and permissions.", sep = "")
    }
  }
  
  ## Extract the data from the URLs generation
  URLS <- unlist(lapply(urls, '[[', "urls"))
  sites <- unlist(lapply(urls, '[[', "ids"))
  years <- unlist(lapply(urls, '[[', "years"))
  months <- unlist(lapply(urls, '[[', "months"))
  
  ## filenames to use to save the data
  fnames <- paste(sites, years, months, "data.csv", sep = "-")
  fnames <- file.path(folder, fnames)
  
  nfiles <- length(fnames)
  
  ## set up a progress bar if being verbose
  if (isTRUE(verbose)) {
    pb <- txtProgressBar(min = 0, max = nfiles, style = 3)
    on.exit(close(pb))
  }
  
  out <- vector(mode = "list", length = nfiles)
  # changed hourly names to omit spaces
  hourlyNames <- c("Longitude", "Latitude", "Station.Name", "ClimateID", "Date.Time.LST", 
                   "Year", "Month", "Day", "Time.LST",
                   "Temp.degC", "Temp.Flag", "Dew.Point.Temp.degC",
                   "Dew.Point.Temp.Flag", "Rel.Hum", "Rel.Hum.Flag", "Precip.Amount.mm", "Precip.Amount.Flag", 
                   "Wind.Dir.10s.deg", "Wind.Dir.Flag", "Wind.Spd.km.h",
                   "Wind.Spd.Flag", "Visibility.km", "Visibility.Flag",
                   "Stn.Press.kPa", "Stn.Press.Flag", "Hmdx", "Hmdx.Flag",
                   "Wind.Chill", "Wind.Chill.Flag", "Weather")
  # MUST BE UPDATED
  dailyNames <- c("Date Time", "Year", "Month", "Day", "Data Quality", "Max Temp (degC)", "Max Temp Flag", 
                  "Min Temp (degC)", "Min Temp Flag", "Mean Temp (degC)", "Mean Temp Flag",
                  "Heat Deg Days (degC)", "Heat Deg Days Flag", "Cool Deg Days (degC)", "Cool Deg Days Flag",
                  "Total Rain (mm)", "Total Rain Flag", "Total Snow (cm)", "Total Snow Flag",
                  "Total Precip (mm)", "Total Precip Flag", "Snow on Grnd (cm)", "Snow on Grnd Flag",
                  "Dir of Max Gust (10s deg)", "Dir of Max Gust Flag", "Spd of Max Gust (10s deg)", "Spd of Max Gust Flag")
  # MUST BE UPDATED
  monthlyNames <- c("Date/Time", "Year", "Month", 
                    "Mean Max Temp (degC)", "Mean Max Temp Flag",
                    "Mean Min Temp (degC)", "Mean Min Temp Flag",
                    "Mean Temp (degC)", "Mean Temp Flag",
                    "Extr Max Temp (degC)", "Extr Max Temp Flag",
                    "Extr Min Temp (degC)", "Extr Min Temp Flag",
                    "Total Rain (mm)", "Total Rain Flag",
                    "Total Snow (cm)", "Total Snow Flag",
                    "Total Precip (mm)", "Total Precip Flag",
                    "Snow Grnd Last Day (cm)", "Snow Grnd Last Day Flag",
                    "Dir of Max Gust (10s deg)", "Dir of Max Gust Flag",
                    "Spd of Max Gust (10s deg)", "Spd of Max Gust Flag")
  
  cnames <- switch(timeframe, hourly = hourlyNames, daily = dailyNames, monthly = monthlyNames)
  TIMEFRAME <- match(timeframe, c("hourly", "daily", "monthly"))
  SKIP <- c(16, 25, 18)[TIMEFRAME]
  
  for (i in seq_len(nfiles)) {
    curfile <- fnames[i]
    
    ## Have we downloaded the file before?
    if (!file.exists(curfile)) {    # No: download it
      dload <- try(download.file(URLS[i], destfile = curfile, quiet = TRUE))
      if (inherits(dload, "try-error")) { # If problem, store failed URL...
        out[[i]] <- URLS[i]
        if (isTRUE(verbose)) {
          setTxtProgressBar(pb, value = i) # update progress bar...
        }
        next                             # bail out of current iteration
      }
    }
    
    ## Must have downloaded, try to read file
    ## skip first SKIP rows of header stuff
    ## encoding must be latin1 or will fail - may still be problems with character set
    cdata <- try(read.csv(curfile, skip = SKIP, encoding = "latin1", stringsAsFactors = FALSE), silent = TRUE)
    
    ## Did we have a problem reading the data?
    if (inherits(cdata, "try-error")) { # yes handle read problem
      ## try to fix the problem with dodgy characters
      cdata <- readLines(curfile) # read all lines in file
      cdata <- iconv(cdata, from = "latin1", to = "UTF-8")
      writeLines(cdata, curfile)          # write the data back to the file
      ## try to read the file again, if still an error, bail out
      cdata <- try(read.csv(curfile, skip = SKIP, encoding = "UTF-8", stringsAsFactors = FALSE), silent = TRUE)
      if (inherits(cdata, "try-error")) { # yes, still!, handle read problem
        if (delete) {
          file.remove(curfile) # remove file if a problem & deleting
        }
        out[[i]] <- URLS[i]    # record failed URL...
        if (isTRUE(verbose)) {
          setTxtProgressBar(pb, value = i) # update progress bar...
        }
        next                  # bail out of current iteration
      }
    }
    
    ## Must have (eventually) read file OK, add station data
    cdata <- cbind.data.frame(StationID = rep(sites[i], NROW(cdata)),
                              cdata)
    names(cdata)[-1] <- cnames
    out[[i]] <- cdata
    
    if (isTRUE(verbose)) { # Update the progress bar
      setTxtProgressBar(pb, value = i)
    }
  }
  
  out                                 # return
}




###############################################################################


# Define server logic required
server <- function(input, output, session) {
  # data input as a data.table called "data_input" (data.table is similar in function to data.frame, this is reactive
  # so you must call on it like it is a function from here on as "data_input()", will update with each new upload)
  data_input <- reactive({
    if(!is.null(input$csv_input) & input$location == "not_sel"){ # if a .csv file is uploaded (not null) and no other location is selected, that is your data input
      infile <- input$csv_input
      read.csv(infile$datapath)
      
      ###################################################
      # if summerland is selected from the drop down menu
    } else if (input$location == "summerland"){ 
      # if the current date is in the winter-spring (before June)
      if(current_month < 6){                    
        summerland_station <- data.frame(StationID = c(979),
                                         start = (last_year),
                                         end = (current_year))
        met <- getData(summerland_station, folder = "./summerland", verbose = FALSE)
        summerland_data <- dplyr::bind_rows(met)
        # select data from august the year prior to yesterdays date
        summerland_data <- summerland_data %>% dplyr::filter(summerland_data[,6] >= select_date1$FROM & 
                                                               summerland_data[,6] <= select_date1$YDAY)
        # remove the first column
        infile = summerland_data[,-1]
        
        # make predictions
        lat = 49.5626
        lon = -119.6487
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.6487"), times=nrow(forecast))
        Latitude <- rep(c("49.5626"), times=nrow(forecast))
        Station.Name <- rep(c("Summerland"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Summerland_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                          Year, Month, Day, Time.LST,
                                          Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                          Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                          Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                          Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                          Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                          Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Summerland_forecast)
        
      }else{
        
        # if the current date is in the spring-winter (after June)
        summerland_station <- data.frame(StationID = c(979),
                                         start = (current_year),
                                         end = (current_year))
        met <- getData(summerland_station, folder = "./summerland", verbose = FALSE)
        summerland_data <- dplyr::bind_rows(met)
        # select data from august this year to yesterday's date
        summerland_data <- summerland_data %>% dplyr::filter(summerland_data[,6] >= select_date2$FROM & 
                                                               summerland_data[,6] <= select_date2$YDAY)
        # remove the first column
        infile <- summerland_data[,-1]
        
        # make predictions
        lat = 49.5626
        lon = -119.6487
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.6487"), times=nrow(forecast))
        Latitude <- rep(c("49.5626"), times=nrow(forecast))
        Station.Name <- rep(c("Summerland"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Summerland_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                          Year, Month, Day, Time.LST,
                                          Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                          Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                          Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                          Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                          Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                          Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Summerland_forecast)
        
      }
      
      ###################################################
      # if penticton is selected from the drop down menu    
    }else if (input$location == "penticton"){ 
      # if the current date is in the winter-spring (before June)
      if(current_month < 6){                    
        penticton_station <- data.frame(StationID = c(50269),
                                        start = (last_year),
                                        end = (current_year))
        met <- getData(penticton_station, folder = "./penticton", verbose = FALSE)
        penticton_data <- dplyr::bind_rows(met)
        # select data from august the year prior to yesterday's date
        penticton_data <- penticton_data %>% dplyr::filter(penticton_data[,6] >= select_date1$FROM & 
                                                             penticton_data[,6] <= select_date1$YDAY)
        # remove the first column
        infile = penticton_data[,-1]
        
        # make predictions
        lat = 49.4625
        lon = -119.602
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.602"), times=nrow(forecast))
        Latitude <- rep(c("49.4625"), times=nrow(forecast))
        Station.Name <- rep(c("Penticton"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Penticton_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                         Year, Month, Day, Time.LST,
                                         Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                         Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                         Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                         Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                         Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                         Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Penticton_forecast)
        
      }else {
        
        # if the current date is in the spring-winter (after June)
        penticton_station <- data.frame(StationID = c(50269),
                                        start = (current_year),
                                        end = (current_year))
        met <- getData(penticton_station, folder = "./penticton", verbose = FALSE)
        penticton_data <- dplyr::bind_rows(met)
        # select data from august this year to yesterday's date
        penticton_data <- penticton_data %>% dplyr::filter(penticton_data[,6] >= select_date2$FROM & 
                                                             penticton_data[,6] <= select_date2$YDAY)
        # remove the first column
        infile <- penticton_data[,-1]
        
        # make predictions
        lat = 49.4625
        lon = -119.602
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.602"), times=nrow(forecast))
        Latitude <- rep(c("49.4625"), times=nrow(forecast))
        Station.Name <- rep(c("Penticton"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Penticton_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                         Year, Month, Day, Time.LST,
                                         Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                         Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                         Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                         Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                         Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                         Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Penticton_forecast)
        
      }
      
      ###################################################
      # if kelowna is selected from the drop down menu  
    }else if (input$location == "kelowna"){ 
      # if the current date is in the winter-spring (before June)
      if(current_month < 6){                    
        kelowna_station <- data.frame(StationID = c(51117),
                                      start = (last_year),
                                      end = (current_year))
        met <- getData(kelowna_station, folder = "./kelowna", verbose = FALSE)
        kelowna_data <- dplyr::bind_rows(met)
        # select data from august the year prior to yesterday's date
        kelowna_data <- kelowna_data %>% dplyr::filter(kelowna_data[,6] >= select_date1$FROM & 
                                                         kelowna_data[,6] <= select_date1$YDAY)
        # remove the first column
        infile = kelowna_data[,-1]
        
        # make predictions
        lat = 49.9408
        lon = -119.4002
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.4002"), times=nrow(forecast))
        Latitude <- rep(c("49.9408"), times=nrow(forecast))
        Station.Name <- rep(c("Kelowna"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Kelowna_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                       Year, Month, Day, Time.LST,
                                       Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                       Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                       Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                       Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                       Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                       Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Kelowna_forecast)
        
      }else{
        
        # if the current date is in the spring-winter (after June)
        kelowna_station <- data.frame(StationID = c(51117),
                                      start = (current_year),
                                      end = (current_year))
        met <- getData(kelowna_station, folder = "./kelowna", verbose = FALSE)
        kelowna_data <- dplyr::bind_rows(met)
        # select data from august this year to yesterday's date
        kelowna_data <- kelowna_data %>% dplyr::filter(kelowna_data[,6] >= select_date2$FROM & 
                                                         kelowna_data[,6] <= select_date2$YDAY)
        # remove the first column
        infile <- kelowna_data[,-1]
        
        # make predictions
        lat = 49.9408
        lon = -119.4002
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.4002"), times=nrow(forecast))
        Latitude <- rep(c("49.9408"), times=nrow(forecast))
        Station.Name <- rep(c("Kelowna"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Kelowna_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                       Year, Month, Day, Time.LST,
                                       Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                       Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                       Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                       Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                       Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                       Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Kelowna_forecast)
      }
      
      ###################################################
      # if vernon is selected from the drop down menu 
    }else if (input$location == "vernon"){ 
      # if the current date is in the winter-spring (before June)
      if(current_month < 6){                    
        vernon_station <- data.frame(StationID = c(46987),
                                     start = (last_year),
                                     end = (current_year))
        met <- getData(vernon_station, folder = "./vernon", verbose = FALSE)
        vernon_data <- dplyr::bind_rows(met)
        # select data from august the year prior to yesterday's date
        vernon_data <- vernon_data %>% dplyr::filter(vernon_data[,6] >= select_date1$FROM & 
                                                       vernon_data[,6] <= select_date1$YDAY)
        
        # remove the first column
        infile = vernon_data[,-1]
        
        # make predictions
        
        lat = 50.22
        lon = -119.19
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.19"), times=nrow(forecast))
        Latitude <- rep(c("50.22"), times=nrow(forecast))
        Station.Name <- rep(c("Vernon"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Vernon_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                      Year, Month, Day, Time.LST,
                                      Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                      Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                      Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                      Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                      Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                      Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Vernon_forecast)
        
      }else{
        
        # if the current date is in the spring-winter (after June)
        vernon_station <- data.frame(StationID = c(46987),
                                     start = (current_year),
                                     end = (current_year))
        met <- getData(vernon_station, folder = "./vernon", verbose = FALSE)
        vernon_data <- dplyr::bind_rows(met)
        # select data from august this year to yesterday's date
        vernon_data <- vernon_data %>% dplyr::filter(vernon_data[,6] >= select_date2$FROM & 
                                                       vernon_data[,6] <= select_date2$YDAY)
        # remove the first column
        infile <- vernon_data[,-1]
        
        # make predictions
        
        lat = 50.22
        lon = -119.19
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"
        
        #to get todays hourly forcast
        url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly <- ow$hourly
        hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'UTC')
        hourly <- hourly %>% tidyr::unnest(weather)
        hourly <- hourly %>% 
          dplyr::select(dt, temp)
        
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
                       , "&lon=", lon
                       , "&dt=", my_dt
                       , "&appid=", API_key
                       , "&units=", units)
        ow2 <- fromJSON(url2)
        hourly2 <- ow2$hourly
        hourly2$dt <- as.POSIXct(hourly2$dt, origin = "1970-01-01", tz = 'UTC')
        hourly2 <- hourly2 %>% tidyr::unnest(weather)
        hourly2 <- hourly2 %>% 
          dplyr::select(dt, temp)
        
        #get every 3hr forecast data
        hourly3 <- get_forecast(lat = lat, lon = lon, units = units) %>%
          owmr_as_tibble()
        #select dt_txt and temp
        hourly3 <- hourly3 %>% 
          dplyr::select(dt_txt, temp)
        
        #convert date time
        hourly3$dt_txt <- as.POSIXct(hourly3$dt_txt, format="%Y-%m-%d %H:%M:%S")
        
        # make it into a list
        hourly3 <- data.frame(hourly3)
        
        # make a data frame of hourly dates ranging from the first to last date/time in three hour data, 
        # interpolate the hourly temperatures
        dt <- seq.POSIXt(hourly3$dt_txt[1], tail(hourly3$dt_txt, n=1), by='1 hour')
        hourly3 <- data.frame(dt_txt=dt, VIStot=approx(hourly3$dt_txt, hourly3$temp, dt)$y)
        
        # rename to match other data frames
        hourly3 <- hourly3 %>% 
          dplyr::rename(dt = dt_txt)
        
        hourly3 <- hourly3 %>% 
          dplyr::rename(temp = VIStot)
        
        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2, hourly3)
        
        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)
        
        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-119.19"), times=nrow(forecast))
        Latitude <- rep(c("50.22"), times=nrow(forecast))
        Station.Name <- rep(c("Vernon"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Temp.degC <- forecast$temp
        Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.degC <- rep(c("NA"), times=nrow(forecast))
        Dew.Point.Temp.Flag <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum <- rep(c("NA"), times=nrow(forecast))
        Rel.Hum.Flag <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.mm <- rep(c("NA"), times=nrow(forecast))
        Precip.Amount.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.10s.deg <- rep(c("NA"), times=nrow(forecast))
        Wind.Dir.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.km.h <- rep(c("NA"), times=nrow(forecast))
        Wind.Spd.Flag <- rep(c("NA"), times=nrow(forecast))
        Visibility.km <- rep(c("NA"), times=nrow(forecast))
        Visibility.Flag <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.kPa <- rep(c("NA"), times=nrow(forecast))
        Stn.Press.Flag <- rep(c("NA"), times=nrow(forecast))
        Hmdx <- rep(c("NA"), times=nrow(forecast))
        Hmdx.Flag <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill <- rep(c("NA"), times=nrow(forecast))
        Wind.Chill.Flag <- rep(c("NA"), times=nrow(forecast))
        Weather <- rep(c("NA"), times=nrow(forecast))
        
        #create data frame
        Vernon_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST, 
                                      Year, Month, Day, Time.LST,
                                      Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                      Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag, 
                                      Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                      Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                      Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                      Wind.Chill, Wind.Chill.Flag, Weather)
        
        # add predictions to data frame
        infile <- rbind(infile, Vernon_forecast)
        
      }
      
      
      # if nothing is selected or uploaded (so site doesn't immediately crash)
    } else if (is.null(infile)) {
      return(NULL)
    }
    
    
  })
  
  #render image
  # output$orchard <- renderImage({
  #   filename <- normalizePath(file.path('./www/orchard.jpeg'))
  # Return a list containing the filename
  #  list(src = filename, width = "50%", style="display: block; margin-left: auto; margin-right: auto;")
  # }, deleteFile = FALSE)
  
  # render map
  # will manually make maps data frame since it doesnt seem to render
  # map_data <- read.csv(file = 'map.csv') # change to './Map.csv' if in a different folder?
  # map_data$lat <- as.numeric(map_data$lat)
  # map_data$long <- as.numeric(map_data$long)
  
  lat <- c(49.5626, 49.4625, 49.9408, 50.22)
  lat <- as.data.frame(lat)
  map_data <- lat
  map_data$long <- c(-119.6487, -119.602, -119.4002, -119.19)
  map_data$station <- c('Summerland', "Penticton", "Kelowna", "Vernon")
    
    map_data$lat <- as.numeric(map_data$lat)
    map_data$long <- as.numeric(map_data$long)
  output$map <- renderLeaflet({
    leaflet(map_data) %>% addTiles() %>% 
      addMarkers(~long, ~lat, popup = ~as.character(station))
  })
  
  
  # Create a data frame out of data_input() with calculated CU and FU using CU_FU function
  # You need 3 separate Calculated_CU_FU variables so that the same variable is not being called on in each time 
  # (was causing issues with downloading the app somewhere else without these unique variables)
  
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
  
  # create a data frame out of data_input() with calculated LT using LT10,LT50,LT90 functions THIS IS WHERE YOU NEED YOUR VARIETY DROP DOWN MENU SELECTION TO COME INTO PLAY (CH_LT10 vs CH_LT10_lapins
  Calculated_LT10 <- reactive({
    if (input$variety == "sweetheart"){
      LT10calc <- CH_LT10(Calculated_CU_FU10())
      LT10calc$Variety <- '_Sweetheart' ### added this line to change downloadable file name
    }
    if (input$variety == "lapins"){
      LT10calc <- CH_LT10_lapins(Calculated_CU_FU10())
      LT10calc$Variety <- '_Lapins' ### added this line to change downloadable file name
    }
    LT10calc
  })
  
  Calculated_LT50 <- reactive({
    if (input$variety == "sweetheart"){
      LT50calc <- CH_LT50(Calculated_CU_FU50())
    }
    if (input$variety == "lapins"){
      LT50calc <- CH_LT50_lapins(Calculated_CU_FU50())
    }
    LT50calc
  })
  
  Calculated_LT90 <- reactive({
    if (input$variety == "sweetheart"){
      LT90calc <- CH_LT90(Calculated_CU_FU90())
    } 
    if (input$variety == "lapins"){
      LT90calc <- CH_LT90_lapins(Calculated_CU_FU90())
    }
    LT90calc
  })
  
  #import test data to make sure this is working (must be done in console)

  # TestData <- read.csv("/Users/Elizabeth/Desktop/TestData/TestData.csv")
  # Calculated_CU_FU <- CU_FU(TestData)
  # Merge Calculated_LT10, Calculated_LT50, and Calculated_LT90, select only important columns (for data download)
  
  All_LTs <- reactive({
    LTs <- merge(Calculated_LT10(), Calculated_LT50(), by="Temp_min")
    LTs <- merge(LTs, Calculated_LT90(), by="Temp_min")
    # LTs <- cbind(Calculated_LT10(), Calculated_LT50(), Calculated_LT90())
    LTs <- LTs %>% dplyr::select(YYYYMMDD, LT10, LT50, LT90, Temp_min) # keep minimum air temps in data frame
    # LTs <- LTs  %>% 
    #   dplyr::rename(Temp_min = Temp_min.x)
    # split by year, month, day so you can select for Sept to April only
    LTs$YYYYMMDD <- as.Date(LTs$YYYYMMDD)
    LTs <- LTs %>%
      tidyr::separate(YYYYMMDD, into = c("YYYY", "MM", "DD"), "-")
    LTs <- subset(LTs, MM >= "09" | MM <= "04")
    LTs$YYYYMMDD <- as.Date(with(LTs, paste(YYYY, MM, DD, sep="-")),"%Y-%m-%d")
    LTs <- LTs %>% dplyr::select(YYYYMMDD, LT10, LT50, LT90, Temp_min)
    # highlight if the min air temp fell below the LT10, LT50, or LT90
    LTs$LT10 <- as.numeric(LTs$LT10)
    LTs$LT50 <- as.numeric(LTs$LT50)
    LTs$LT90 <- as.numeric(LTs$LT90)
    LTs$Temp_min <- as.numeric(LTs$Temp_min)
    
    LTs$Predicted.damage <- ifelse(LTs$LT10 >= LTs$Temp_min, "10%", "")
    LTs$Predicted.damage <- ifelse(LTs$LT50 >= LTs$Temp_min, "50%", LTs$Predicted.damage)
    LTs$Predicted.damage <- ifelse(LTs$LT90 >= LTs$Temp_min, "90%", LTs$Predicted.damage)
    
    #wip
    # round to 1 decimal places
    LTs$LT10 <- format(round(LTs$LT10, 1), nsmall = 1) 
    LTs$LT50 <- format(round(LTs$LT50, 1), nsmall = 1)
    LTs$LT90 <- format(round(LTs$LT90, 1), nsmall = 1)
    LTs$Temp_min <- format(round(LTs$Temp_min, 1), nsmall = 1)
    LTs$Weather.data = "historic"
    LTs$Weather.data[LTs$YYYYMMDD >= as.Date(Sys.Date())] = "predictions"
    #sort by date
    LTs <- LTs[order(as.Date(LTs$YYYYMMDD, format="%Y-%m-%d")),]
    LTs
  })
  
  # To download the .csv file of lethal temperature data to your computer
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Lethal_Temp_Estimates_", Calculated_LT10()$Station.Name, Calculated_LT10()$Variety, ".csv", sep = "") ### added 'Calculated_LT10()$Variety,'
    },
    content = function(file) {
      write.csv(All_LTs(), file, row.names = FALSE)
    }
  )
  
  # When the run button is hit, draw plots and render text
  # LT10
  plot_LT10 <- eventReactive(input$run_button,{
    draw_plot_LT10(Calculated_LT10(), Calculated_LT90())
  })
  
  description1 <- eventReactive(input$run_button,{
    "This plot shows the estimated temperature that will cause 10% of flower buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shaded area. This cold damage is predicted to occur when the daily minimum temperature drops below the estimated lethal temperature."
  })
  
  output$plot_LT10 <- renderPlot(plot_LT10(), height=600)
  
  output$description1 <- renderText({
    description1()
  }) 
  
  
  # LT50
  plot_LT50 <- eventReactive(input$run_button,{
    draw_plot_LT50(Calculated_LT50(), Calculated_LT90())
  })
  
  description2 <- eventReactive(input$run_button,{
    "This plot shows the estimated temperature that will cause 50% of flower buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shaded area. This cold damage is predicted to occur when the daily minimum temperature drops below the estimated lethal temperature."
  })
  
  output$plot_LT50 <- renderPlot(plot_LT50(), height=600)
  
  output$description2 <- renderText({
    description2()
  }) 
  
  # plot LT90
  plot_LT90 <- eventReactive(input$run_button,{
    draw_plot_LT90(Calculated_LT90())
  })
  
  description3 <- eventReactive(input$run_button,{
    "This plot shows the estimated temperature that will cause 90% of flower buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shaded area. This cold damage is predicted to occur when the daily minimum temperature drops below the estimated lethal temperature."
  })
  
  output$plot_LT90 <- renderPlot(plot_LT90(), height=600)
  
  output$description3 <- renderText({
    description3()
  }) 
  
  
  # To download the .csv file of the data template required
  output$downloadTemplate <- downloadHandler(
    filename <- function() {
      "DataTemplate.csv"
    },
    content <- function(file) {
      file.copy("./DataTemplate.csv", file) # needs to be file.copy("./data/DataTemplate.csv", file) when a function
    }
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)

# Currently not wrapped in a function for deployment to shinyapp.io
#}
