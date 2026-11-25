#' Shiny app for cold hardiness estimations
#' 
#' Web document to calculate cold hardiness estimations
#' 
#' @details Calculates the 10, 50, and 90% lethal temperatures for 'Sweetheart' and 'Lapins' sweet cherries in the Okanagan Valley, BC, Canada based on daily air temperatures.
#' 
#' @return web application, data frame of lethal temperatures
#' 
#' @import shiny shinydashboard shinythemes utils owmr jsonlite zoo dplyr tidyr leaflet ChillModels plotly randomForest chillR sodium
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
library(plotly)
library(randomForest)
library(chillR)
# library(bslib)
library(sodium)

# Updated Jan 2025
# remove it from a function when deploying it on shinyapp.io
# cherrycoldhardiness <- function() {
# the user interface

# Oct 2026 update
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
                                        10%, 50%, and 90% damage (LT10, LT50, LT90) to the flower buds. It is important to note that estimations made using these models from the months 
                                      of May to mid- to late-September generally are not meaniningful as sweet cherry flower buds often have not yet acclimated in this region. 
                                          Additional information on these models and their development is available in the following publication:")), style = "font-size:17px;color:black;"),
                                    
                                    p("Houghton, E., Noonan, M.J., Hannam, K., Nelson, L.M., and Neilsen, D. 2023. 
                                        Models for estimating the cold hardiness of sweet cherry",
                                      HTML("(<i>Prunus avium</i> cv. ‘Sweetheart’ and ‘Lapins’)"), 
                                      "in cold climate regions. HortSci. 58: 963-973.", style = "font-size:17px;color:black;",
                                      a(href="https://doi.org/10.21273/HORTSCI17128-23", "https://doi.org/10.21273/HORTSCI17128-23", style = "font-size:17px;color:teal;"), #active weblink to GitHub
                                      ".", style = "font-size:17px;color:black;"),
                                    
                                    br(),
                                    p(
                                      
                                      img(src ="seasons1.jpg", height = 255.5, width = 947.5, style="display: block; margin-left: auto; margin-right: auto;"),),
                                    br(),
                                    hr(),
                                    p(h4("Help and feedback", style = "color:black;")),
                                    p("For additional help, to submit feedback, or to report a bug, please contact: Elizabeth Houghton (elizabeth.anne.houghton@gmail.com). The website source content can be viewed on", style = "font-size:17px;color:black;",
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
                           
                           #######################################################

    # Custom CSS layout to create a sticky side panel and a scrollable main area
    tabPanel(
      strong("About the Models"),
      tags$head(
      tags$style(HTML("
        /* Split layout container */
        .toc-container {
          display: flex;
          align-items: flex-start;
          gap: 30px;
          margin-top: 15px;
        }
        
        /* Sticky Sidebar for Table of Contents */
        .toc-sidebar {
          width: 250px; # alter depending on text width
          position: -webkit-sticky;
          position: sticky;
          top: 70px; /* Keeps it locked under your Flatly top navbar */
          background-color: #f8f9fa;
          padding: 20px; 
          border-radius: 2px;
          border-left: 4px solid #18bc9c; /* Flatly's signature teal color accent */
        }
        
        /* Smooth scrolling profile for the browser viewport */
        html {
          scroll-behavior: smooth;
        }
        
        /* Formatting for the TOC links */
        .toc-sidebar ul {
          list-style-type: none;
          padding-left: 0;
          margin-bottom: 0;
        }
        .toc-sidebar li {
          margin-bottom: 12px;
        }
        .toc-sidebar a {
          color: #2c3e50; /* Flatly's dark slate brand color */
          text-decoration: none;
          font-weight: 500;
        }
        .toc-sidebar a:hover {
          color: #18bc9c; /* Turns teal on hover */
          text-decoration: underline;
        }
        
        /* Main Body Area */
        .toc-body {
          flex: 1;
        }
        
        .toc-sub-list {
  list-style: none;
  padding-left: 25px;      /* Indents the sub-sections to the right */
  margin-top: 5px;
  margin-bottom: 5px;
}

.toc-sub-list li {
  margin-bottom: 6px;      /* Tighter spacing for minor links */
}

.toc-sub-list a {
  color: #7f8c8d !important; /* Flatly's muted grey color for minor items */
  font-weight: normal !important; /* Thinner text than the main bold headers */
  font-size: 14px !important;    /* Slightly smaller font footprint */
}

.toc-sub-list a:hover {
  color: #18bc9c !important; /* Turns teal on hover just like main items */
}
      "))
    ),
    
    # THE INNER LAYOUT FRAMEWORK
    div(
      class = "toc-container",
      
      # SIDEBAR: The Table of Contents
      div(
        class = "toc-sidebar",
        tags$h4("Contents", style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
        tags$ul(
          tags$li(tags$a(href = "#section1", "Regression models")),
          tags$ul(
            class = "toc-sub-list", # Sub-list class for indentation
            tags$li(tags$a(href = "#section2", "Model evaluation")),
            tags$li(tags$a(href = "#section3", "'Sweetheart' model validation")),
            tags$li(tags$a(href = "#section4", "'Lapins' model validation")),
            tags$li(tags$a(href = "#section5", "Application to other varieties"))
          ),
          tags$li(tags$a(href = "#section6", "New Machine Learning Models")),
          tags$ul(
            class = "toc-sub-list", # Sub-list class for indentation
            tags$li(tags$a(href = "#section7", "Model evaluation and validation"))
          )
        )
      ),
      
      # MAIN PANEL: Scrollable document body
      div(
        class = "toc-body",
        
        div(
          id = "section1", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("Regression models", style = "color:black;font-size:27px;"),
            p("Six seasons of 'Sweetheart' and three seasons of 'Lapins' sweet cherry flower bud cold hardiness measurements from orchards in Summerland, British Columbia, Canada were used to develop these models. Additionally,
            hourly air temperatures were aquired from nearby weather stations. To improve models fit in the spring, when flower buds rapidly lose cold hardiness, models were split into two time periods:
            t1 (fall to later winter) and t2 (spring). The equations of the final models used to predict flower bud lethal temperatures during t1 (fall to late winter) are as follows:", style = "font-size:17px;color:black;"),
            br(),
          p(strong(HTML(paste0("LT", tags$sub("Sweetheart.t1"), " = &beta;", tags$sub("S0"),
                               " + &beta;", tags$sub("S1"), "T", tags$sub("mean.lag1"),
                               " + &beta;", tags$sub("S2"), "T", tags$sub("mean.lag3"),
                               " + &beta;", tags$sub("S3"),
                               "CU + &beta;", tags$sub("S4"),
                               "FU + &epsilon;", tags$sub("S")))), style = "text-align:center;font-size:17px;color:black;"),
            br(),
          p(strong(HTML(paste0("LT", tags$sub("Lapins.t1"), " = &beta;", tags$sub("L0"),
                               " + &beta;", tags$sub("L1"), "T", tags$sub("mean.lag1"),
                               "+ &beta;", tags$sub("L2"),
                               "CU + &beta;", tags$sub("L3"),
                               "FU + &epsilon;", tags$sub("L")))), style = "text-align:center;font-size:17px;color:black;"),
            br(),
          p(HTML(paste0("Where LT", tags$sub("Sweetheart.t1"),"  and LT", tags$sub("Lapins.t1")," represent the lethal temperature that causes 10%, 50%, and 90% damage (LT10, LT50, LT90) to 'Sweetheart' and 'Lapins' flower buds, respectively.
                          Unique models using the same parameters were developed for LT10, LT50, and LT90. &beta;", tags$sub("S0"), " and &beta;", tags$sub("L0"), " are the intercepts, &beta;", tags$sub("S1")," and &beta;", tags$sub("L1"),"
                          are the coefficients of the mean daily temperature from one day prior (˚C, T", tags$sub("mean.lag1"), ") , &beta;", tags$sub("S2"),
                        "  is the coefficient of the mean daily temperature from three days prior (˚C, T", tags$sub("mean.lag3"), ") , &beta;", tags$sub("S3"), " and &beta;", tags$sub("L2")," are the coefficients of the accumulated chilling units (CU), &beta;", tags$sub("S4"),
                        " and &beta;", tags$sub("L3"), " are the coefficients of the log transformed accumulated forcing units (FU), and &epsilon;", tags$sub("S"), " and &epsilon;", tags$sub("L")," are the error terms. Estimates of accumulated daily chilling and forcing were 
                        calculated from hourly air temperatures using models developed by ",
                        a(href ="https://www.researchgate.net/publication/282060426_Development_of_chilling_and_forcing_relationships_for_modeling_spring_phenology_of_apple_and_sweet_cherry", "Neilsen et al. (2015)", style = "font-size:17px;color:teal;"),
                        ". The final best fit models used to predict LT during t2 (spring) for both 'Sweetheart' and 'Lapins' included the intercept only meaning these estimates are equivalent to the mean lethal temperatures measured during this time period. <b>Caution must be taken when using this model to predict spring lethal temperatures as limited data were available for model development during this period.</b>"
          )), style = "font-size:17px;color:black;"),
          
          br()
          ),
        
        div(
          id = "section2", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("Model evaluation", style = "color:black;font-size:22px;"),
            p("The cold hardiness models showed strong agreement between predicted 
            and observed lethal temperatures for both ‘Sweetheart’ and ‘Lapins’ when evaluated against data not used in model development (Fig. 1).", style = "font-size:17px;color:black;"),
            # img(src ="Figure1.jpg", height = 484.25, width = 749.75, style="display: block; margin-left: auto; margin-right: auto;"),
            # p(em("Figure 1. One-to-one regression of the predicted and observed lethal temperature values for 'Sweetheart' and 'Lapins' evaluated against data used in model development"), style = "text-align:center;font-size:14px;"),
            img(src ="Figure2.jpg", height = 425.37, width = 674.78, style="display: block; margin-left: auto; margin-right: auto;"),
            p(em("Figure 1. One-to-one regression of the predicted and observed lethal temperature values for 'Sweetheart' and 'Lapins' evaluated against data not used in model development"), style = "text-align:center;font-size:14px;")
        ),
        
        div(
          id = "section3", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("'Sweetheart' model validation", style = "color:black;font-size:22px;"),
            p(HTML("'Sweetheart' model performance remained strong when evaluated against observations from several locations at varying elevations within Summerland, BC that were excluded from model development (Fig. 2), supporting their use for estimating 
            the changing cold hardiness of sweet cherry flower buds. However, <b>caution must be taken when using these models to predict spring lethal temperatures as limited data were available for model development during this period</b>."), style = "font-size:17px;color:black;"),
            img(src ="Figure3.jpg", height = 207.87, width = 749.75,  style="display: block; margin-left: auto; margin-right: auto;"),
            p(em("Figure 2. Examples of model estimates that were compared to years of lethal temperature data that were not included in model development collected from orchards varying in elevation in Summerland, BC.
                 These plots show the estimated temperature that will cause 10%, 50%, and 90% (LT10, LT50, LT90) of the buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shade area.
                 Models were compared to data collected at a high elevation orchard in 2020-2021, a mid-elevation orchard in 2020-2021, and a low-elevation orchard in 2021-2022."), style = "text-align:center;font-size:14px;"),
        ),
        
        div(
          id = "section4", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("'Lapins' model validation", style = "color:black;font-size:22px;"),
            p(HTML("Much like the 'Sweetheart' models, 'Lapins' model performance remained strong when evaluated against one year of data that was not used in model development (Fig. 3).
            Again, <b>caution must be taken when using these model to predict spring lethal temperatures as limited data were available during this period.</b>"), style = "font-size:17px;color:black;"),
            img(src ="Figure4.jpg", height = 202.75, width = 288.65,  style="display: block; margin-left: auto; margin-right: auto;"),
            p(em("Figure 3. Examples of model estimates that were compared to one year of lethal temperature data that was not included in model development collected from an AAFC research orchard in Summerland, BC in the 2014-2015 season.
                 These plots show the estimated temperature that will cause 10%, 50%, and 90% (LT10, LT50, LT90) of the buds to experience cold damage with the 95% confidence intervals on these estimations shown by the shade area."), 
              style = "text-align:center;font-size:14px;")
        ),
        
        div(
          id = "section5", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("Application to other varieties", style = "color:black;font-size:22px;"),
            p(HTML("Lethal temperature estimates from the 'Sweetheart' and 'Lapins' models were also compared to lethal temperature measurements for the varieties 'Skeena', 'Sonata', and 'Staccato' (Fig. 4). It must be noted that although model fit looks good during the fall and winter season,
              <b>caution must be taken when applying the 'Sweetheart' models to other varieties, especially in the spring time if bloom times vary greatly between varieties.</b>"), style = "font-size:17px;color:black;"),
            img(src ="Figure5.jpg", height = 368.89, width = 749.75,  style="display: block; margin-left: auto; margin-right: auto;"),
            p(em("Figure 4. Predicted ‘Sweetheart’ and ‘Lapins’ LT50 values in the t1 period with 95% confidence intervals plotted against observed ‘Staccato’, ‘Sonata’, and ‘Skeena’ cultivar LT50 values."), style = "text-align:center;font-size:14px;")
        ),
        
        div(
          id = "section6", 
          style = "margin-bottom: 1px;", # this sets how far down the next heading is from the body paragraph
          h2("New Machine Learning Models", style = "color:black;font-size:27px;"),
          p("An additional three seasons of 'Sweetheart' and six seasons of 'Skeena' sweet cherry flower bud cold hardiness measurements from orchards in Summerland, British Columbia, Canada were used to develop a random forest model after the completion of the initial regression models. 
             This model was developed to estimate the lethal temperatures that would result in 50% and 90% damage (LT50, LT90) to the flower buds. Predictor variables for the lethal temperature
             include: the maximum daily air temperature from one to four days prior, the mean daily air temeprature from one to three days prior,
            the daylength, the accumulated chill units, and log transformed accumulated forcing units.", style = "font-size:17px;color:black;")
        ),
        
        div(
          id = "section7", 
          style = "margin-bottom: 50px;", # this sets how far down the next heading is from the body paragraph
          h2("Model evaluation and validation", style = "color:black;font-size:22px;"),
          p("The random forest cold hardiness model showed overall good agreement between predicted 
            and observed lethal temperatures for ‘Sweetheart', ‘Lapins’, and 'Skeena' when evaluated against data not used in model development (Fig. 5).", style = "font-size:17px;color:black;"),
          img(src ="Figure6.jpg", height = 493.69, width = 674.78, style="display: block; margin-left: auto; margin-right: auto;"),
          p(em("Figure 5. One-to-one regression of the predicted and observed lethal temperature values for 'Sweetheart','Lapins', and 'Skeena' evaluated against data not used in model development"), style = "text-align:center;font-size:14px;"),
          p(HTML("The random forest cold hardiness model showed strong agreement between predicted 
            and observed lethal temperatures for ‘Sweetheart’,‘Lapins’, and 'Skeena' when evaluated against data not used in model development (Fig. 6).
           Like the previously described models, <b>caution must be taken when using these model to predict spring lethal temperatures as limited data were available during this period.</b>"), style = "font-size:17px;color:black;"),
          img(src ="Figure7.jpg", height = 478.33, width = 749.75, style="display: block; margin-left: auto; margin-right: auto;"),
          p(em("Figure 6. Model estimates that were compared to years of lethal temperature data that were not included in model development.
                 These plots show the estimated temperature that will cause 50% and 90% (LT50, LT90) of the buds to experience cold damage with the 95% confidence intervals on 500 trees for the random forest estimations shown by the shade area."), 
            style = "text-align:center;font-size:14px;")
          
        ),
        )
    )
    ),

                           
                           # third tab panel
                           tabPanel(strong("How to Use"),
                                    h3("Data requirements", style = "color:black;font-size:27px;"),
                                    br(),
                                    p("This model requires hourly air temperature data starting in September of the season you would like to estimate
                                        lethal temperatures. If you are interested in using your own hourly temperature data, it must be uploaded as a CSV file in the same format as
                                        the following example. A correct Climate ID is not required for this model to run, however, this template is in the same format that climate data is provided from the", style = "font-size:17px;color:black;",  
                                      a(href="https://climate.weather.gc.ca/historical_data/search_historic_data_e.html", "Government of Canada's historical climate database", style = "font-size:17px;color:teal;"),
                                      ". A downloadable CSV file template with example data inputs has also been provided for you to use.", style = "font-size:17px;color:black;", 
                                      HTML(" If you would like to use your own weather data, the <b>'Use uploaded data' option must be selected</b> under the 'Or choose closest weather station' tab.")
                                    ),
                                    br(),
                                    img(src ="Data_example.jpg", height = 159.8, width = 803.62, style="display: block; margin-left: auto; margin-right: auto;"),  
                                    br(),
                                    br(),
                                    downloadButton("downloadTemplate", 
                                                   "Download file template",  style="width:33%; display: block; margin-left: auto; margin-right: auto;"),
                                    br(),
                                    h3("Don't have your own weather data?", style = "color:black;font-size:27px;"),
                                    br(),
                                    p("Choose one of five locations currently available and hourly temperature data from the", style = "font-size:17px;color:black;",
                                      a(href="https://climate.weather.gc.ca/historical_data/search_historic_data_e.html", "Government of Canada's historical climate database", style = "font-size:17px;color:teal;"),
                                      "will be used in these models. You can find the exact weather stations used at each location here:", style = "font-size:17px;color:black;",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2007-11-30%7C2021-08-03&dlyRange=2005-01-01%7C2021-08-02&mlyRange=%7C&StationID=46987&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=2&searchMethod=contains&txtStationName=vernon&timeframe=1&time=LST&time=LST&Year=2025&Month=1&Day=1#", "Vernon", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-12-13%7C2021-08-03&dlyRange=2013-12-16%7C2021-08-02&mlyRange=%7C&StationID=51117&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=16&searchMethod=contains&txtStationName=kelowna&timeframe=1&time=LST&time=LST&Year=2025&Month=1&Day=1#", "Kelowna", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=1994-02-01%7C2021-08-03&dlyRange=1990-06-01%7C2021-08-02&mlyRange=1990-01-01%7C2006-11-01&StationID=979&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=3&searchMethod=contains&txtStationName=summerland&timeframe=1&time=LST&time=LST&Year=2025&Month=1&Day=1#", "Summerland", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2012-05-07%7C2021-08-03&dlyRange=2012-05-10%7C2021-08-02&mlyRange=%7C&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=1&searchMethod=contains&txtStationName=penticton&timeframe=1&time=LST&time=LST&Year=2025&Month=1&Day=1#", "Penticton", style = "font-size:17px;color:teal;"),
                                      ",",
                                      a(href="https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=1994-02-01%7C2025-01-06&dlyRange=1993-05-01%7C2025-01-06&mlyRange=1993-01-01%7C2006-10-01&StationID=6838&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2025&selRowPerPage=25&Line=1&searchMethod=contains&txtStationName=creston&timeframe=1&time=LST&time=LST&Year=2025&Month=1&Day=1#", "Creston", style = "font-size:17px;color:teal;"),
                                      ". If you select one of these weather stations, two days worth of weather forecast from", style = "font-size:17px;color:black;",
                                      a(href="https://openweathermap.org", "OpenWeather", style = "font-size:17px;color:teal;"),
                                      HTML(" will also be inputted to make predictions using these models. If the Government of Canada's historical climate database website is undergoing maintenance or has not been updated with the appropriate data, this option will not work and you will get the message <i>'An error has occurred. Check your logs or contact the app author for clarification.'</i> You can still upload data to the website to be inputted into the models if this occurs.
                                           <b>This option may take several minutes to load</b>.")
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
                                 selectInput("location", "Or choose closest weather station (Available Sept-May only)", choices = c("Use uploaded data"= "not_sel",
                                                                                                                                    "Penticton"="penticton",
                                                                                                                                    "Summerland"="summerland",
                                                                                                                                    "Kelowna"="kelowna",
                                                                                                                                    "Vernon"="vernon",
                                                                                                                                    "Creston"="creston"
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
                                 h5("Results may take several minutes to load", style = "font-size:14px;margin-top:7%"),
                                 
                                 # plot leaflet object (map)
                                 br(),
                                 h5(HTML("<b>Weather station locations</b>"), style = "font-size:15px;margin-top:1%"),
                                 leafletOutput("map")
                               ),
                               
                               # Oct 2026 update                              
                               mainPanel(
                                 plotlyOutput("plot_LT_combined", width = "100%", height = "650px"),
                                 
                                 # verbatimTextOutput does not allow you to show coloured HTML text, needed to adjust this so you could colour text instead of using a legend    
                                 # verbatimTextOutput("description_combined"),
                                 uiOutput("description_combined"),
                                 tags$style(
                                   type = "text/css",
                                   "#description_combined {
                                     white-space: pre-wrap;
                                     word-break: break-word;
                                     background-color: rgba(255,255,255,0.40);
                                     color: black;
                                     border-style: none;
                                     font-family: calibri;
                                     font-size: 16px;
                                     }"
                                 )
                               )
                               
                             )
                           ),
                           
                           # Oct 2026 update 
                           # fifth tab panel
                           tabPanel(
                             title = strong("*New Estimations*"),
                             
                             # Top panel with text
                             fluidRow(
                               column(
                                 width = 12,
                                 wellPanel(
                                   p("Since initial model development, new cold hardiness data have been collected at AAFC SuRDC. These data, along with the data used in 
                                     initial model development, were used to create additional cold hardiness models using a machine learning algorithm (random forest) 
                                     to expand this decision support tool.", em("Insufficient data for cold hardiness estimations in the spring season as rapid deacclimation 
                                     occurs has resulted in uncertainty in cold hardiness predictions during this period"), " .", style = "margin-bottom: 0px;")
                                   
                                 )
                               )
                             ),
                             
                             sidebarLayout(
                               sidebarPanel(
                                 title = "Inputs",
                                 
                                 p("Upload hourly climate data in Estimations tab or select a weather station below."),
                                 selectInput(
                                   "rf_location",
                                   "Climate data source",
                                   choices = c(
                                     "Use uploaded data" = "not_sel",
                                     "Penticton" = "penticton",
                                     "Summerland" = "summerland",
                                     "Kelowna" = "kelowna",
                                     "Vernon" = "vernon",
                                     "Creston" = "creston"
                                   ),
                                   selected = "not_sel"
                                 ),
                                 selectInput(
                                   "rf_variety",
                                   "Select sweet cherry variety",
                                   choices = c(
                                     "Sweetheart" = "sweetheart",
                                     "Lapins" = "lapins",
                                     "Skeena" = "skeena"
                                   ),
                                   selected = "sweetheart"
                                 ),
                                 actionButton(
                                   "rf_run_button",
                                   "Run Analysis",
                                   style = "margin-top: 8%"
                                 ),
                                 downloadButton(
                                   "rf_downloadData",
                                   "Save results",
                                   style = "margin-top: 8%"
                                 ),
                                 h5("Results may take several minutes to load", style = "font-size:14px;margin-top:7%"),
                                 br(),
                                 h5(HTML("<b>Weather station locations</b>"), style = "font-size:15px;margin-top:1%"),
                                 leafletOutput("map_1")
                               ),
                               
                               
                               # additional rf model code
                               
                               mainPanel(
                                 plotlyOutput("rf_plot_LT_combined", width = "100%", height = "650px"),
                                 
                                 # verbatimTextOutput does not allow you to show coloured HTML text, needed to adjust this so you could colour text instead of using a legend    
                                 # verbatimTextOutput("rf_description_combined"),
                                 uiOutput("rf_description_combined"),
                                 tags$style(
                                   type = "text/css",
                                   "#rf_description_combined {
                                     white-space: pre-wrap;
                                     word-break: break-word;
                                     background-color: rgba(255,255,255,0.40);
                                     color: black;
                                     border-style: none;
                                     font-family: calibri;
                                     font-size: 16px;
                                     }"
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

# Oct 2026 update

# FOR ORIGINAL MODEL
draw_plot_LT_combined <- function(
    Calculated_LT10,
    Calculated_LT50,
    Calculated_LT90
) {
  
  # Prepare LT10 data
  lt10 <- Calculated_LT10 %>%
    transmute(
      Date = as.Date(YYYYMMDD),
      LT = "LT10",
      Estimate = LT10,
      Lower = LT10.CILower,
      Upper = LT10.CIUpper,
      Temp_min = Temp_min
    )
  
  # Prepare LT50 data
  lt50 <- Calculated_LT50 %>%
    transmute(
      Date = as.Date(YYYYMMDD50),
      LT = "LT50",
      Estimate = LT50,
      Lower = LT50.CILower,
      Upper = LT50.CIUpper,
      Temp_min = Temp_min
    )
  
  # Prepare LT90 data
  lt90 <- Calculated_LT90 %>%
    transmute(
      Date = as.Date(YYYYMMDD90),
      LT = "LT90",
      Estimate = LT90,
      Lower = LT90.CILower,
      Upper = LT90.CIUpper,
      Temp_min = Temp_min
    )
  
  # Combine datasets
  lt_data <- bind_rows(lt10, lt50, lt90) %>%
    mutate(
      Period = if_else(
        Date > Sys.Date(),
        "Weather Predictions",
        "Recorded Weather"
      ),
      LT = factor(
        LT,
        levels = c("LT10", "LT50", "LT90")
      )
    )
  
  # --- FIX: Duplicate the boundary point to close the gap ---
  boundary_points <- lt_data %>%
    filter(Period == "Recorded Weather") %>%
    group_by(LT) %>%
    slice_max(Date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Period = "Weather Predictions")

  # Combine again and arrange correctly
  lt_data <- bind_rows(lt_data, boundary_points) %>%
    arrange(LT, Date, Period)
  # ----------------------------------------------------------
  
  # Daily minimum temperature
  temp_data <- Calculated_LT90 %>%
    transmute(
      Date = as.Date(YYYYMMDD90),
      Temp_min = Temp_min
    ) %>%
    distinct(Date, .keep_all = TRUE)
  
  
  # Colours for each lethal temperature
  lt_colours <- c(
    "LT10" = "#A9C9E2",
    "LT50" = "#4B8BBE",
    "LT90" = "#1D4E89"
  )
  
  # Tooltip information
  lt_data <- lt_data %>%
    mutate(
      Hover = paste0(
        "<b>", LT, "</b>",
        "<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Estimate: ", round(Estimate, 1), " °C",
        "<br>95% CI: ",
        round(Lower, 1), " to ",
        round(Upper, 1), " °C",
        "<br>", Period
      )
    )
  
  temp_data <- temp_data %>%
    mutate(
      Hover = paste0(
        "<b>Daily Minimum Temperature</b>",
        "<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Temperature: ", round(Temp_min, 1), " °C"
      )
    )
  
  # Draw graph
  p <- ggplot() +
    
    # 95% confidence intervals
    geom_ribbon(
      data = lt_data,
      aes(
        x = Date,
        ymin = Lower,
        ymax = Upper,
        fill = LT,
        group = interaction(LT, Period)
      ),
      alpha = 0.18,
      na.rm = TRUE
    ) +
    
    # Estimated lethal temperatures
    geom_line(
      data = lt_data,
      aes(
        x = Date,
        y = Estimate,
        colour = LT,
        linetype = Period,
        group = interaction(LT, Period),
        text = Hover
      ),
      linewidth = 1,
      na.rm = TRUE
    ) +
    
    
    # Add small points to make hovering easier
    geom_point(
      data = lt_data,
      aes(
        x = Date,
        y = Estimate,
        colour = LT,
        text = Hover
      ),
      size = 0.8,
      na.rm = TRUE
    ) +
    
    # Daily minimum air temperature
    geom_line(
      data = temp_data,
      aes(
        x = Date,
        y = Temp_min,
        text = Hover,
        group = 1
      ),
      colour = "#B22222",
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    
    geom_point(
      data = temp_data,
      aes(
        x = Date,
        y = Temp_min,
        text = Hover  # <-- Plotly will capture this perfectly now
      ),
      colour = "#B22222",
      size = 0.5, # Keep them small or set alpha = 0 to make them invisible
      alpha = 0,
      na.rm = TRUE
    ) +
    
    scale_colour_manual(
      name = "Lethal temperature",
      values = lt_colours
    ) +
    
    
    scale_fill_manual(
      values = lt_colours,
      guide = "none"
    ) +
    
    scale_linetype_manual(
      name = "Weather data",
      values = c(
        "Recorded Weather" = "solid",
        "Weather Predictions" = "dotted"
      )
    ) +
    
    labs(
      # title = "Sweet Cherry Cold Hardiness",
      x = "",
      y = "Lethal temperature (°C)",
      # y = expression("Lethal Temperature (" * degree * "C)")
    ) +
    
    coord_cartesian(
      xlim = c(
        min(lt_data$Date, na.rm = TRUE) + 30,
        max(lt_data$Date, na.rm = TRUE) - 1
      ),
      ylim = c(
        min(
          c(lt_data$Lower, temp_data$Temp_min),
          na.rm = TRUE
        ) - 1,
        0
      )

    ) +

    
    theme_bw(base_size = 13) +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.box = "vertical"
    )
  
  # Make the graph interactive
  ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      # legend = list(orientation = "h")
      showlegend = FALSE
    )

}



# FOR RF MODEL
rf_draw_plot_LT_combined <- function(
    Calculated_LT50_rf,
    Calculated_LT90_rf
) {
  
  # Prepare LT50 data
  lt50 <- Calculated_LT50_rf %>%
    transmute(
      Date = as.Date(YYYYMMDD50),
      LT = "LT50",
      Estimate = LT_temp,
      Lower = LT50.CILower,
      Upper = LT50.CIUpper,
      Temp_min = Temp_min
    )
  
  # Prepare LT90 data
  lt90 <- Calculated_LT90_rf %>%
    transmute(
      Date = as.Date(YYYYMMDD90),
      LT = "LT90",
      Estimate = LT_temp,
      Lower = LT90.CILower,
      Upper = LT90.CIUpper,
      Temp_min = Temp_min
    )
  
  # Combine datasets
  lt_data <- bind_rows(lt50, lt90) %>%
    mutate(
      Period = if_else(
        Date > Sys.Date(),
        "Weather Predictions",
        "Recorded Weather"
      ),
      LT = factor(
        LT,
        levels = c("LT50", "LT90")
      )
    )
  
  # --- FIX: Duplicate the boundary point to close the gap ---
  boundary_points <- lt_data %>%
    filter(Period == "Recorded Weather") %>%
    group_by(LT) %>%
    slice_max(Date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Period = "Weather Predictions")

  # Combine again and arrange correctly
  lt_data <- bind_rows(lt_data, boundary_points) %>%
    arrange(LT, Date, Period)
  # ----------------------------------------------------------
  
  # Daily minimum temperature
  temp_data <- Calculated_LT90_rf %>%
    transmute(
      Date = as.Date(YYYYMMDD90),
      Temp_min = Temp_min
    ) %>%
    distinct(Date, .keep_all = TRUE)
  
  # Colours for each lethal temperature
  lt_colours <- c(
    "LT50" = "#4B8BBE",
    "LT90" = "#1D4E89"
  )
  
  # Tooltip information
  lt_data <- lt_data %>%
    mutate(
      Hover = paste0(
        "<b>", LT, "</b>",
        "<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Estimate: ", round(Estimate, 1), " °C",
        "<br>95% CI: ",
        round(Lower, 1), " to ",
        round(Upper, 1), " °C",
        "<br>", Period
      )
    )
  
  temp_data <- temp_data %>%
    mutate(
      Hover = paste0(
        "<b>Daily Minimum Temperature</b>",
        "<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Temperature: ", round(Temp_min, 1), " °C"
      )
    )
  
  # Draw graph
  p <- ggplot() +
    
    # 95% confidence intervals
    geom_ribbon(
      data = lt_data,
      aes(
        x = Date,
        ymin = Lower,
        ymax = Upper,
        fill = LT,
        group = interaction(LT, Period)
      ),
      alpha = 0.18,
      na.rm = TRUE
    ) +
    
    # Estimated lethal temperatures
    geom_line(
      data = lt_data,
      aes(
        x = Date,
        y = Estimate,
        colour = LT,
        linetype = Period,
        group = interaction(LT, Period),
        text = Hover
      ),
      linewidth = 1,
      na.rm = TRUE
    ) +
    
    
    # Add small points to make hovering easier
    geom_point(
      data = lt_data,
      aes(
        x = Date,
        y = Estimate,
        colour = LT,
        text = Hover
      ),
      size = 0.8,
      na.rm = TRUE
    ) +
    
    # Daily minimum air temperature
    geom_line(
      data = temp_data,
      aes(
        x = Date,
        y = Temp_min,
        text = Hover,
        group = 1
      ),
      colour = "#B22222",
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    
    geom_point(
      data = temp_data,
      aes(
        x = Date,
        y = Temp_min,
        text = Hover  # <-- Plotly will capture this perfectly now
      ),
      colour = "#B22222",
      size = 0.5, # Keep them small or set alpha = 0 to make them invisible
      alpha = 0,
      na.rm = TRUE
    ) +
    
    scale_colour_manual(
      name = "Lethal temperature",
      values = lt_colours
    ) +
    
    
    scale_fill_manual(
      values = lt_colours,
      guide = "none"
    ) +
    
    scale_linetype_manual(
      name = "Weather data",
      values = c(
        "Recorded Weather" = "solid",
        "Weather Predictions" = "dotted"
      )
    ) +
    
    labs(
      # title = "Sweet Cherry Cold Hardiness",
      x = "",
      y = "Lethal temperature (°C)",
      # y = expression("Lethal Temperature (" * degree * "C)")
    ) +
    
    coord_cartesian(
      xlim = c(
        min(lt_data$Date, na.rm = TRUE) + 30,
        max(lt_data$Date, na.rm = TRUE) - 1
      ),
      ylim = c(
        min(
          c(lt_data$Lower, temp_data$Temp_min),
          na.rm = TRUE
        ) - 1,
        0
      )
    ) +

  
    theme_bw(base_size = 13) +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.box = "vertical"
    )
  
  # Make the graph interactive
  ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      # legend = list(orientation = "h")
      showlegend = FALSE
    )
  
  
  # print(summary(temp_data$Temp_min))
  # print(sum(is.finite(temp_data$Temp_min)))
  # print(range(temp_data$Date, na.rm = TRUE))
}

###############################################################################
# Currently placing externally accessed functions in the app.R file to simply deploy to shinyapp.io

# CU_FU function to calculate chill and forcing units
CU_FU <- function(data_input=NULL) {
  #uses the data_input from the .csv upload
  # select columns of interest
  data_input <- subset(data_input, select = -c(Flag)) # April 2025, remove new 'Flag' column
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
  
  # OLD APPROACH FOR CU CALCULATIONS USING THE ChillModels 'cherry_model' function
  # # Calculate CU
  # 
  # data_input$CU <- ChillModels::cherry_model(data_input$Temp, total=FALSE)
  # 
  # # Make a new column (CU_acc) that is the accumulation of all the calculated CU from each day prior and the current day
  # 
  # data_input <- data.frame(data_input, CU_acc=cumsum(data_input$CU))
  # 
  
  # NEW CU CALCULATION APPROACH USING EQUATION FROM NEILSEN ET AL. 2015
  # Calculate CU
  
  data_input <- data_input %>% mutate(CU = case_when(Temp < -2 ~ 0, 
                                                     Temp >= 18 ~ -1,
                                                     Temp >= 16 & Temp < 18 ~ -0.5,
                                                     Temp < 16 & Temp >= -2 ~
                                                       1-((46.8012*exp(-exp(-(Temp-9.8275)/2.1401)))/42.80757)))
  
  # Make a new column (Sweetheart_cherry_model) that is the accumulation of all the calculated chill units
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
# CU_FU function to calculate chill and forcing units WITH temp_max_lags!
CU_FU_rf <- function(rf_data_input=NULL) {
  #uses the rf_data_input from the .csv upload
  # select columns of interest
  rf_data_input <- subset(rf_data_input, select = -c(Flag)) # April 2025, remove new 'Flag' column
  rf_data_input <- rf_data_input[,c(3,5:10)]
  # rename columns
  colnames(rf_data_input) <- c("Station.Name", "Date", "Year", "Month", "Day", "Time", "Temp")
  # make sure they are numeric
  rf_data_input$Month <- as.numeric(rf_data_input$Month)
  rf_data_input$Day <- as.numeric(rf_data_input$Day)
  rf_data_input$Temp <- as.numeric(rf_data_input$Temp)
  rf_data_input$Time <- as.POSIXct(rf_data_input$Time,format="%H:%M") # this converts the time out of being a factor but adds a day that doesn't make sense
  # sort by year, month, day, time
  rf_data_input <- rf_data_input[
    order( rf_data_input[,3], rf_data_input[,4], rf_data_input[,5], rf_data_input[,6]),
  ]
  # replace any NA values with the average of the above and below temp values (if needed)
  rf_data_input$Temp <- (zoo::na.approx(rf_data_input$Temp, rule = 2))
  
  rf_data_input_mean <- rf_data_input #save to calculate daily max and lag temps

  
  # NEW CU CALCULATION APPROACH USING EQUATION FROM NEILSEN ET AL. 2015
  # Calculate CU
  
  rf_data_input <- rf_data_input %>% mutate(CU = case_when(Temp < -2 ~ 0, 
                                                     Temp >= 18 ~ -1,
                                                     Temp >= 16 & Temp < 18 ~ -0.5,
                                                     Temp < 16 & Temp >= -2 ~
                                                       1-((46.8012*exp(-exp(-(Temp-9.8275)/2.1401)))/42.80757)))
  
  # Make a new column (Sweetheart_cherry_model) that is the accumulation of all the calculated chill units
  rf_data_input <- data.frame(rf_data_input, CU_acc=cumsum(rf_data_input$CU))
  
  #remove the first NA value with na.omit
  rf_data_input <- stats::na.omit(rf_data_input)
  
  # Anything after the minimum CU_acc, assign the accumulative CU, everything before assign 0
  # which.min outputs the row that the minimum value exists in
  
  # if the length of the data frame when selecting for values after the minimum CU_acc value is 
  # greater than 0, assign CU_acc_final to be the cumulative sum of CU, if it is shorter than 1,
  # assign CU_acc_final a value of zero
  
  if (nrow(rf_data_input[(which.min(rf_data_input$CU_acc)):nrow(rf_data_input), ]) > 1){
    rf_data_input_2 <- rf_data_input[(which.min(rf_data_input$CU_acc) + 1):nrow(rf_data_input), ]
    rf_data_input_2 <-  data.frame(rf_data_input_2, CU_acc_final=cumsum(rf_data_input_2$CU))
    # remove Station.Name from rf_data_input so there isnt duplicates in data upload
    station <- as.character(last(rf_data_input$Station.Name)) # built in incase Station.Name problems
    rf_data_input <- subset(rf_data_input, select = -c(Station.Name))
    rf_data_input <- merge(rf_data_input, rf_data_input_2, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc"), all = TRUE)
    rf_data_input$Station.Name <- station
  } else {
    rf_data_input <- rf_data_input %>% dplyr::mutate(CU_acc_final = 0)
  }
  
  
  # Replace NA values with 0
  rf_data_input[is.na(rf_data_input)] <- 0
  
  # If CU_acc_final below 1119, assign acquiring, if CU_acc_final above 1119, assign complete
  rf_data_input <- rf_data_input %>% dplyr::mutate(CU_state = case_when(CU_acc_final < 1119 ~ 'acquiring', 
                                                                  CU_acc_final >= 1119 ~ 'complete'))
  
  # For the model, have a column that is 0-1119 (Call it CU_1119)
  rf_data_input <- rf_data_input %>% dplyr::mutate(CU_1119 = case_when(CU_acc_final < 1119 ~ CU_acc_final, 
                                                                 CU_acc_final >= 1119 ~ 1119))
  
  # FU Calculations
  # FU will begin accumulation after 1119 CU have been accumulated
  # Subset data that has complete the chilling requirements
  
  rf_data_input_FU <- rf_data_input %>% dplyr::filter(CU_state=="complete")
  
  # calculate the FU for these times
  # From Neilsen et al. 2015
  
  rf_data_input_FU <- rf_data_input_FU %>% dplyr::mutate(FU = case_when(Temp < 5 ~ 0, 
                                                                  Temp >= 5 ~ 1.5/(1+exp(-(Temp-22.8)/5.7))))
  
  # Make a new column (FU_acc) that is the accumulation of all the calculated FU from each day prior and the current day
  
  rf_data_input_FU <- data.frame(rf_data_input_FU, FU_acc=cumsum(rf_data_input_FU$FU))
  
  # add FU to CU data frame
  
  rf_data_input_final <- merge(rf_data_input, rf_data_input_FU, by = c("Date", "Year", "Month", "Day", "Time","Temp", "CU", "CU_acc", "CU_acc_final", "CU_state", "CU_1119", "Station.Name"), all = TRUE)
  
  # Replace NA values with 0
  rf_data_input_final[is.na(rf_data_input_final)] <- 0
  
  # Optimal forcing reached at 68 FU, assign FU_state 
  rf_data_input_final <- rf_data_input_final %>% dplyr::mutate(FU_state = case_when(CU_acc_final < 1119 ~ 'not_applicable',
                                                                              FU_acc < 68 ~ 'acquiring', 
                                                                              FU_acc >= 68 ~ 'complete'))
  
  # assign forcing a value of below 2000 chill accumulation to 0 to give early forcing accumulated during warm snaps in the winter less weight
  rf_data_input_final$FU_acc_0 <- 0
  
  rf_data_input_final$FU_acc <- ifelse(rf_data_input_final$CU_acc_final <= 2000, # if cherry model is less than 2000
                                    rf_data_input_final$FU_acc_0, # assign Sweetheart_cherry_forcingBB_final to 0
                                    rf_data_input_final$FU_acc) # otherwise give it its original values
  
  # Now, to bring to the resolution of daily maximum air temperature, keep only the daily data from the end (10:00) at each day (most growers will not be expecting an hourly resolution anyways)
  
  rf_data_input_daily <- rf_data_input_final %>% 
    dplyr::mutate(Hour = lubridate::hour(Time),
                  Minute = lubridate::minute(Time),
                  Second = lubridate::second(Time)) %>%  
    dplyr::filter(Hour == 10 & Minute == 0 & Second == 0)
  
  # add log transformed forcing units
  rf_data_input_daily$FU_acc_log <- log(rf_data_input_daily$FU_acc+10)
  
  
  ## Calculate Daily Air Temperatures from Hourly Data
  # Aggregate by Year, Month, Day
  # mean temp
  rf_data_input_dailymean <- stats::aggregate(Temp ~ Year + Month + Day, rf_data_input_mean, mean)
  # Min temp
  rf_data_input_dailymin <- stats::aggregate(Temp ~ Year + Month + Day, rf_data_input_mean, min)
  
  # max temp
  rf_data_input_dailymax <- stats::aggregate(Temp ~ Year + Month + Day, rf_data_input_mean, max)
  
  # rename Temp to Temp_mean and Temp to Temp_min
  rf_data_input_dailymean <- rf_data_input_dailymean %>% 
    dplyr::rename(Temp_mean = Temp)
  
  rf_data_input_dailymin <- rf_data_input_dailymin %>% 
    dplyr::rename(Temp_min = Temp)
  
  rf_data_input_dailymax <- rf_data_input_dailymax %>% 
    dplyr::rename(Temp_max = Temp)
  
  # merge mean and min by Year, Month, Day
  rf_data_input_dailymean <- merge(rf_data_input_dailymean, rf_data_input_dailymin, by = c("Year", "Month", "Day"), all = TRUE)
  rf_data_input_dailymean <- merge(rf_data_input_dailymean, rf_data_input_dailymax, by = c("Year", "Month", "Day"), all = TRUE)
  
  # sort by year, month, day
  rf_data_input_dailymean <- rf_data_input_dailymean[
    order(rf_data_input_dailymean[,1], rf_data_input_dailymean[,2], rf_data_input_dailymean[,3]),
  ]
  
  
  # Assign 1st order temperature lags for mean temp
  # For mean temps
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_mean.lag1 = dplyr::lag(rf_data_input_dailymean$Temp_mean))
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_mean.lag2 = dplyr::lag(rf_data_input_dailymean$Temp_mean, 2))
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_mean.lag3 = dplyr::lag(rf_data_input_dailymean$Temp_mean, 3))
  
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_max.lag1 = dplyr::lag(rf_data_input_dailymean$Temp_max))
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_max.lag2 = dplyr::lag(rf_data_input_dailymean$Temp_max, 2))
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_max.lag3 = dplyr::lag(rf_data_input_dailymean$Temp_max, 3))
  rf_data_input_dailymean <- data.frame(rf_data_input_dailymean, Temp_max.lag4 = dplyr::lag(rf_data_input_dailymean$Temp_max, 4))
  
  # Merge CU_FU and daily mean temps into one
  CU_FU1 <- merge(rf_data_input_dailymean, rf_data_input_daily, by = c("Year", "Month", "Day"), all = TRUE)
  
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
                                                           "Temp_max.lag1", 
                                                           "Temp_max.lag2",
                                                           "Temp_max.lag3",
                                                           "Temp_max.lag4",
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
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_max.lag1, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_max.lag2, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_max.lag3, .direction= c("up"))
  CUFUcalculations <- CUFUcalculations %>% tidyr::fill(Temp_max.lag4, .direction= c("up"))
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
  
  # OCT 2026 UPDATE
  # Remove last day in case there is only a partial day of data
  CUFUcalculations <- head(CUFUcalculations, -1)
  
  # assign all Station.Name values to the value of the last row (then it will be e.g. 'Kelowna' not 'KELOWNA UBCO')
  CUFUcalculations$Station.Name <- as.character(last(CUFUcalculations$Station.Name))
  
  # calculate daylength
  JDay <- as.numeric(format(as.Date(CUFUcalculations$YYYYMMDD), "%j"))
  daylength <- chillR::daylength(latitude = 49.6535, JDay = JDay) # this is the average latitude currently between 5 included stations (summerland, penticton, vernon, kelowna, creston)
  CUFUcalculations <- cbind(CUFUcalculations, daylength$Daylength) 
  CUFUcalculations <- CUFUcalculations %>% 
    rename(daylength = "daylength$Daylength")
  # calculate daylength_10hrs
  CUFUcalculations$month <- format(CUFUcalculations$YYYYMMDD, "%m")
  CUFUcalculations <- CUFUcalculations %>%
    mutate(    daylength = as.numeric(unlist(daylength)),
               month = as.numeric(unlist(month)),
               daylength_10hrs = if_else(daylength >= 10 & month %in% c(2, 3, 4, 5), 1, 0)) %>%
    select(-month)
  
  CUFUcalculations <- CUFUcalculations %>%
    mutate(FU_BB_log = Sweetheart_cherry_forcingBB_final_log) 
  
  CUFUcalculations
}

###############################################################################

# ORIGINAL MODELS

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
  
  PredictLT10$LT10.30 <- -4.3
  PredictLT10$LT10.CIUpper.30 <- -3.8
  PredictLT10$LT10.CILower.30 <- -4.8
  PredictLT10$LT10_standard_error.30 <- 0.2443594
  
  PredictLT10$LT10 <- ifelse(PredictLT10$FU_acc < 30, # if FU_acc is less than 30
                             PredictLT10$LT10, # assign  it its original values
                             PredictLT10$LT10.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CIUpper <- ifelse(PredictLT10$FU_acc < 30, # if FU_acc is less than 30
                                     PredictLT10$LT10.CIUpper, # assign  it its original values
                                     PredictLT10$LT10.CIUpper.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10.CILower <- ifelse(PredictLT10$FU_acc < 30, # if FU_acc is less than 30
                                     PredictLT10$LT10.CILower,# assign  it its original values
                                     PredictLT10$LT10.CILower.30) # otherwise give it LT10 to the value of that averaged model
  
  PredictLT10$LT10_standard_error <- ifelse(PredictLT10$FU_acc < 30, # if FU_acc is less than 30
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
  
  PredictLT50$LT50.30 <- -5.8
  PredictLT50$LT50.CIUpper.30 <- -5.3
  PredictLT50$LT50.CILower.30 <- -6.3
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
  
  PredictLT90$LT90.30 <- -7.4
  PredictLT90$LT90.CIUpper.30 <- -6.6
  PredictLT90$LT90.CILower.30 <- -8.1
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
  
  PredictLT10$LT10.30 <- -4.1
  PredictLT10$LT10.CIUpper.30 <- -3.5
  PredictLT10$LT10.CILower.30 <- -4.7
  PredictLT10$LT10_standard_error.30 <- 0.3
  
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
  
  PredictLT50$LT50.30 <- -5.2
  PredictLT50$LT50.CIUpper.30 <- -4.5
  PredictLT50$LT50.CILower.30 <- -5.8
  PredictLT50$LT50_standard_error.30 <- 0.3
  
  
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
  PredictLT90$LT90.CIUpper.30 <- -5.4
  PredictLT90$LT90.CILower.30 <- -7.2
  PredictLT90$LT90_standard_error.30 <- 0.5
  
  
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

# RF MODELS

# CH_LT50 to calculate the 50% lethal temperature / sweetheart
CH_LT50_rf_sweetheart <- function(Calculated_CU_FU50_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU50_rf_cut <- Calculated_CU_FU50_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU50_rf_cut$LT <- factor("LT50", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU50_rf_cut$variety <- factor("sweetheart", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU50_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU50_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU50_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT50 <- data.frame(
    LT_temp = unlist(pred_means),
    LT50.CIUpper  = unlist(upper_bound),
    LT50.CILower  = unlist(lower_bound),
    YYYYMMDD50 = Calculated_CU_FU50_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU50_rf$Temp_min,
    LT50_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT50$LT <- "LT50"
  PredictLT50$variety <- "sweetheart"
  PredictLT50$Station.Name <- Calculated_CU_FU50_rf$Station.Name
  PredictLT50
}

###############################################################################

# CH_LT50 to calculate the 50% lethal temperature / lapins
CH_LT50_rf_lapins <- function(Calculated_CU_FU50_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU50_rf_cut <- Calculated_CU_FU50_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU50_rf_cut$LT <- factor("LT50", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU50_rf_cut$variety <- factor("lapins", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU50_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU50_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU50_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT50 <- data.frame(
    LT_temp = unlist(pred_means),
    LT50.CIUpper  = unlist(upper_bound),
    LT50.CILower  = unlist(lower_bound),
    YYYYMMDD50 = Calculated_CU_FU50_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU50_rf$Temp_min,
    LT50_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT50$LT <- "LT50"
  PredictLT50$variety <- "lapins"
  PredictLT50$Station.Name <- Calculated_CU_FU50_rf$Station.Name
  PredictLT50
}

###############################################################################

# CH_LT50 to calculate the 50% lethal temperature / skeena
CH_LT50_rf_skeena <- function(Calculated_CU_FU50_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU50_rf_cut <- Calculated_CU_FU50_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU50_rf_cut$LT <- factor("LT50", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU50_rf_cut$variety <- factor("skeena", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU50_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT50 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU50_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU50_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT50 <- data.frame(
    LT_temp = unlist(pred_means),
    LT50.CIUpper  = unlist(upper_bound),
    LT50.CILower  = unlist(lower_bound),
    YYYYMMDD50 = Calculated_CU_FU50_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU50_rf$Temp_min,
    LT50_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT50$LT <- "LT50"
  PredictLT50$variety <- "skeena"
  PredictLT50$Station.Name <- Calculated_CU_FU50_rf$Station.Name
  PredictLT50
}

###############################################################################

# CH_LT90 to calculate the 90% lethal temperature / sweetheart
CH_LT90_rf_sweetheart <- function(Calculated_CU_FU90_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU90_rf_cut <- Calculated_CU_FU90_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU90_rf_cut$LT <- factor("LT90", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU90_rf_cut$variety <- factor("sweetheart", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU90_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU90_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU90_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT90 <- data.frame(
    LT_temp = unlist(pred_means),
    LT90.CIUpper  = unlist(upper_bound),
    LT90.CILower  = unlist(lower_bound),
    YYYYMMDD90 = Calculated_CU_FU90_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU90_rf$Temp_min,
    LT90_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT90$LT <- "LT90"
  PredictLT90$variety <- "sweetheart"
  PredictLT90$Station.Name <- Calculated_CU_FU90_rf$Station.Name
  PredictLT90
}

###############################################################################

# CH_LT90 to calculate the 90% lethal temperature / lapins
CH_LT90_rf_lapins <- function(Calculated_CU_FU90_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU90_rf_cut <- Calculated_CU_FU90_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU90_rf_cut$LT <- factor("LT90", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU90_rf_cut$variety <- factor("lapins", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU90_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU90_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU90_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT90 <- data.frame(
    LT_temp = unlist(pred_means),
    LT90.CIUpper  = unlist(upper_bound),
    LT90.CILower  = unlist(lower_bound),
    YYYYMMDD90 = Calculated_CU_FU90_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU90_rf$Temp_min,
    LT90_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT90$LT <- "LT90"
  PredictLT90$variety <- "lapins"
  PredictLT90$Station.Name <- Calculated_CU_FU90_rf$Station.Name
  PredictLT90
}

###############################################################################

# CH_LT90 to calculate the 90% lethal temperature / skeena
CH_LT90_rf_skeena <- function(Calculated_CU_FU90_rf=NULL){
  # load in the rf model
  rf_model <- readRDS("./rf_model.rds")
  # load in the training data 
  train_data_50_90 <- read.csv("./train_data_50_90.csv")
  train_data_50_90$LT <- as.factor(train_data_50_90$LT)
  train_data_50_90$variety <- as.factor(train_data_50_90$variety)
  # Select only matching columns
  Calculated_CU_FU90_rf_cut <- Calculated_CU_FU90_rf %>% 
    select(Temp_max.lag1, Temp_max.lag2, Temp_max.lag3, Temp_max.lag4, 
           Temp_mean.lag1, Temp_mean.lag2, Temp_mean.lag3, daylength, daylength_10hrs, FU_BB_log, cherry_model_final)
  
  Calculated_CU_FU90_rf_cut$LT <- factor("LT90", levels = levels(train_data_50_90$LT))
  Calculated_CU_FU90_rf_cut$variety <- factor("skeena", levels = levels(train_data_50_90$variety))
  
  # make sure same factor levels between data the rf model was trained on and input data
  for(attr in colnames(train_data_50_90)) {
    if (is.factor(train_data_50_90[[attr]])) {
      levels(Calculated_CU_FU90_rf_cut[[attr]]) <- levels(train_data_50_90[[attr]])
    }
  }
  # predict the LT90 values based on file upload (inputID labeled 'csv_input')
  rf_pred <- predict(rf_model, newdata = Calculated_CU_FU90_rf_cut, predict.all = TRUE)
  
  # Calculate the mean and standard error for each observation
  pred_means <- rf_pred$aggregate
  pred_sd    <- apply(rf_pred$individual, 1, sd)
  pred_se    <- pred_sd / sqrt(rf_model$ntree)
  
  # Calculate a 95% Confidence Interval
  margin_error <- 1.96 * pred_se
  lower_bound  <- pred_means - margin_error
  upper_bound  <- pred_means + margin_error
  
  #FU_acc <- Calculated_CU_FU90_rf$FU_acc
  
  # create the dataframe of correct variables
  PredictLT90 <- data.frame(
    LT_temp = unlist(pred_means),
    LT90.CIUpper  = unlist(upper_bound),
    LT90.CILower  = unlist(lower_bound),
    YYYYMMDD90 = Calculated_CU_FU90_rf$YYYYMMDD,
    Temp_min = Calculated_CU_FU90_rf$Temp_min,
    LT90_standard_error = unlist(pred_se)
    #FU_acc = unlist(FU_acc)
  )
  PredictLT90$LT <- "LT90"
  PredictLT90$variety <- "skeena"
  PredictLT90$Station.Name <- Calculated_CU_FU90_rf$Station.Name
  PredictLT90
}

###############################################################################


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
                   "Year", "Month", "Day", "Time.LST", "Flag", # April 2025, noticed additional 'Flag' column added to datasets so had to add that here
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

# ESTIMATIONS
# # Define server logic required

# OCT 2026 UPDATE 
      # ESTIMATIONS #
      # Define server logic required
      server <- function(input, output, session) {
        # data input as a data.table called "data_input" (data.table is similar in function to data.frame, this is reactive
        # so you must call on it like it is a function from here on as "data_input()", will update with each new upload)
        get_weather_data <- function(location, uploaded_file) {
          infile <- NULL
          if(!is.null(uploaded_file) & location == "not_sel"){ # if a .csv file is uploaded (not null) and no other location is selected, that is your data input
            infile <- uploaded_file
            read.csv(infile$datapath)
            
           
      ###################################################
      # if summerland is selected from the drop down menu
    } else if (location == "summerland"){ 
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
        
        summerland_data$Date.Time.LST <- as.POSIXct(summerland_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        summerland_data$Station.Name <- rep(c("Summerland"), times=nrow(summerland_data))
        
        # remove the first column
        infile = summerland_data[,-1]
        
        # ADD BACK

        # make predictions
        lat = 49.5626
        lon = -119.6487
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

        #average out daily predictions in case there are repeats
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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                          Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
        
        summerland_data$Date.Time.LST <- as.POSIXct(summerland_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        summerland_data$Station.Name <- rep(c("Summerland"), times=nrow(summerland_data))
        
        # remove the first column
        infile <- summerland_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.5626
        lon = -119.6487
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                          Year, Month, Day, Time.LST, Flag,  #April 2025 added 'Flag'
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
    }else if (location == "penticton"){ 
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
        
        penticton_data$Date.Time.LST <- as.POSIXct(penticton_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        penticton_data$Station.Name <- rep(c("Summerland"), times=nrow(penticton_data))
        
        # remove the first column
        infile = penticton_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.4625
        lon = -119.602
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                         Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
        
        penticton_data$Date.Time.LST <- as.POSIXct(penticton_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        # OCT 2026 UPDATE
        penticton_data$Station.Name <- rep(c("Penticton"), times=nrow(penticton_data))
          
        # remove the first column
        infile <- penticton_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.4625
        lon = -119.602
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                         Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
    }else if (location == "kelowna"){ 
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
        
        kelowna_data$Date.Time.LST <- as.POSIXct(kelowna_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        kelowna_data$Station.Name <- rep(c("Kelowna"), times=nrow(kelowna_data))
        
        # remove the first column
        infile = kelowna_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.9408
        lon = -119.4002
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                       Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
        
        kelowna_data$Date.Time.LST <- as.POSIXct(kelowna_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        kelowna_data$Station.Name <- rep(c("Kelowna"), times=nrow(kelowna_data))
        
        # remove the first column
        infile <- kelowna_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.9408
        lon = -119.4002
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                       Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
    }else if (location == "vernon"){ 
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
        
        vernon_data$Date.Time.LST <- as.POSIXct(vernon_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        vernon_data$Station.Name <- rep(c("Vernon"), times=nrow(vernon_data))
        
        # remove the first column
        infile = vernon_data[,-1]
        
        # ADD BACK
        # make predictions

        lat = 50.22
        lon = -119.19
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

        #average out daily predictions in case there are repeats
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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                      Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
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
        
        vernon_data$Date.Time.LST <- as.POSIXct(vernon_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        vernon_data$Station.Name <- rep(c("Vernon"), times=nrow(vernon_data))
        
        # remove the first column
        infile <- vernon_data[,-1]
        
        # ADD BACK
        # make predictions

        lat = 50.22
        lon = -119.19
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

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
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
                                      Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
                                      Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                      Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag,
                                      Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                      Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                      Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                      Wind.Chill, Wind.Chill.Flag, Weather)

        # add predictions to data frame
        infile <- rbind(infile, Vernon_forecast)
        
      }
      
      ############################################################################################################################################################################################################     
      
      ###################################################
      # if creston is selected from the drop down menu
    } else if (location == "creston"){ 
      # if the current date is in the winter-spring (before June)
      if(current_month < 6){                    
        creston_station <- data.frame(StationID = c(6838),
                                      start = (last_year),
                                      end = (current_year))
        met <- getData(creston_station, folder = "./creston", verbose = FALSE)
        creston_data <- dplyr::bind_rows(met)
        # select data from august the year prior to yesterdays date
        creston_data <- creston_data %>% dplyr::filter(creston_data[,6] >= select_date1$FROM & 
                                                         creston_data[,6] <= select_date1$YDAY)
        
        creston_data$Date.Time.LST <- as.POSIXct(creston_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        creston_data$Station.Name <- rep(c("Creston"), times=nrow(creston_data))
        
        # remove the first column
        infile = creston_data[,-1]
        
        # ADD BACK
        # make predictions
        lat = 49.0817
        lon = -116.5007
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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


        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

        #average out daily predictions in case there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)

        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-116.5007"), times=nrow(forecast))
        Latitude <- rep(c("49.0817"), times=nrow(forecast))
        Station.Name <- rep(c("Creston"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
        Creston_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST,
                                       Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
                                       Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                       Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag,
                                       Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                       Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                       Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                       Wind.Chill, Wind.Chill.Flag, Weather)

        # add predictions to data frame
        infile <- rbind(infile, Creston_forecast)
        
      }else{
        
        # if the current date is in the spring-winter (after June)
        creston_station <- data.frame(StationID = c(6838),
                                      start = (current_year),
                                      end = (current_year))
        met <- getData(creston_station, folder = "./creston", verbose = FALSE)
        creston_data <- dplyr::bind_rows(met)
        # select data from august this year to yesterday's date
        creston_data <- creston_data %>% dplyr::filter(creston_data[,6] >= select_date2$FROM & 
                                                         creston_data[,6] <= select_date2$YDAY)
        
        creston_data$Date.Time.LST <- as.POSIXct(creston_data$Date.Time.LST, format="%Y-%m-%d %H:%M") #NEED TO ADD THIS DATE-TIME CONVERSION IN
        
        creston_data$Station.Name <- rep(c("Creston"), times=nrow(creston_data))
        
        # remove the first column
        infile <- creston_data[,-1]
        
        #ADD BACK
        # make predictions
        lat = 49.0817
        lon = -116.5007
        # this must be as.numeric from datetime
        my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
        API_key = "d99f77bed68ff58e6dec11cdc2bbb127"
        units = "metric"

        # make a dataframe with today's date
        my_dt_series <- as.data.frame(Sys.Date())
        # replicate these entries for number of hours
        my_dt_series <- my_dt_series[rep(row.names(my_dt_series), times = 24), ]
        my_dt_series <- as.data.frame(my_dt_series)
        #rename column
        colnames(my_dt_series)[1] <- c("date")
        # break into year month date
        my_dt_series <- my_dt_series %>%
          mutate(
            year = year(date),
            month = month(date),
            day = day(date),
            hour = c(0:23),
            minute = c(0)
          )
        # make datetime column (dt)
        my_dt_series <- my_dt_series %>%
          select(year, month, day, hour, minute) %>%
          mutate(dt = make_datetime(year, month, day, hour, minute))

        # new as.numeric datetime for the current day's hourly datetime
        # to get today's hourly forecast you must now request each hour of the day for today's date
        my_dt_t0 <- as.numeric(as.POSIXct(strftime(my_dt_series[1,6])))
        my_dt_t1 <- as.numeric(as.POSIXct(strftime(my_dt_series[2,6])))
        my_dt_t2 <- as.numeric(as.POSIXct(strftime(my_dt_series[3,6])))
        my_dt_t3 <- as.numeric(as.POSIXct(strftime(my_dt_series[4,6])))
        my_dt_t4 <- as.numeric(as.POSIXct(strftime(my_dt_series[5,6])))
        my_dt_t5 <- as.numeric(as.POSIXct(strftime(my_dt_series[6,6])))
        my_dt_t6 <- as.numeric(as.POSIXct(strftime(my_dt_series[7,6])))
        my_dt_t7 <- as.numeric(as.POSIXct(strftime(my_dt_series[8,6])))
        my_dt_t8 <- as.numeric(as.POSIXct(strftime(my_dt_series[9,6])))
        my_dt_t9 <- as.numeric(as.POSIXct(strftime(my_dt_series[10,6])))
        my_dt_t10 <- as.numeric(as.POSIXct(strftime(my_dt_series[11,6])))
        my_dt_t11 <- as.numeric(as.POSIXct(strftime(my_dt_series[12,6])))
        my_dt_t12 <- as.numeric(as.POSIXct(strftime(my_dt_series[13,6])))
        my_dt_t13 <- as.numeric(as.POSIXct(strftime(my_dt_series[14,6])))
        my_dt_t14 <- as.numeric(as.POSIXct(strftime(my_dt_series[15,6])))
        my_dt_t15 <- as.numeric(as.POSIXct(strftime(my_dt_series[16,6])))
        my_dt_t16 <- as.numeric(as.POSIXct(strftime(my_dt_series[17,6])))
        my_dt_t17 <- as.numeric(as.POSIXct(strftime(my_dt_series[18,6])))
        my_dt_t18 <- as.numeric(as.POSIXct(strftime(my_dt_series[19,6])))
        my_dt_t19 <- as.numeric(as.POSIXct(strftime(my_dt_series[20,6])))
        my_dt_t20 <- as.numeric(as.POSIXct(strftime(my_dt_series[21,6])))
        my_dt_t21 <- as.numeric(as.POSIXct(strftime(my_dt_series[22,6])))
        my_dt_t22 <- as.numeric(as.POSIXct(strftime(my_dt_series[23,6])))
        my_dt_t23 <- as.numeric(as.POSIXct(strftime(my_dt_series[24,6])))

        # 0:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t0
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t0 <- ow$data
        hourly_t0 <- hourly_t0 %>%
          dplyr::select(dt, temp)
        hourly_t0$dt <- as.POSIXct(hourly_t0$dt, origin = "1970-01-01", tz = 'UTC')

        # 1:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t1
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t1 <- ow$data
        hourly_t1 <- hourly_t1 %>%
          dplyr::select(dt, temp)
        hourly_t1$dt <- as.POSIXct(hourly_t1$dt, origin = "1970-01-01", tz = 'UTC')

        # 2:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t2
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t2 <- ow$data
        hourly_t2 <- hourly_t2 %>%
          dplyr::select(dt, temp)
        hourly_t2$dt <- as.POSIXct(hourly_t2$dt, origin = "1970-01-01", tz = 'UTC')

        # 3:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t3
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t3 <- ow$data
        hourly_t3 <- hourly_t3 %>%
          dplyr::select(dt, temp)
        hourly_t3$dt <- as.POSIXct(hourly_t3$dt, origin = "1970-01-01", tz = 'UTC')

        # 4:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t4
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t4 <- ow$data
        hourly_t4 <- hourly_t4 %>%
          dplyr::select(dt, temp)
        hourly_t4$dt <- as.POSIXct(hourly_t4$dt, origin = "1970-01-01", tz = 'UTC')

        # 5:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t5
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t5 <- ow$data
        hourly_t5 <- hourly_t5 %>%
          dplyr::select(dt, temp)
        hourly_t5$dt <- as.POSIXct(hourly_t5$dt, origin = "1970-01-01", tz = 'UTC')

        # 6:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t6
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t6 <- ow$data
        hourly_t6 <- hourly_t6 %>%
          dplyr::select(dt, temp)
        hourly_t6$dt <- as.POSIXct(hourly_t6$dt, origin = "1970-01-01", tz = 'UTC')

        # 7:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t7
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t7 <- ow$data
        hourly_t7 <- hourly_t7 %>%
          dplyr::select(dt, temp)
        hourly_t7$dt <- as.POSIXct(hourly_t7$dt, origin = "1970-01-01", tz = 'UTC')

        # 8:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t8
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t8 <- ow$data
        hourly_t8 <- hourly_t8 %>%
          dplyr::select(dt, temp)
        hourly_t8$dt <- as.POSIXct(hourly_t8$dt, origin = "1970-01-01", tz = 'UTC')

        # 9:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t9
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t9 <- ow$data
        hourly_t9 <- hourly_t9 %>%
          dplyr::select(dt, temp)
        hourly_t9$dt <- as.POSIXct(hourly_t9$dt, origin = "1970-01-01", tz = 'UTC')

        # 10:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t10
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t10 <- ow$data
        hourly_t10 <- hourly_t10 %>%
          dplyr::select(dt, temp)
        hourly_t10$dt <- as.POSIXct(hourly_t10$dt, origin = "1970-01-01", tz = 'UTC')

        # 11:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t11
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t11 <- ow$data
        hourly_t11 <- hourly_t11 %>%
          dplyr::select(dt, temp)
        hourly_t11$dt <- as.POSIXct(hourly_t11$dt, origin = "1970-01-01", tz = 'UTC')

        # 12:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t12
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t12 <- ow$data
        hourly_t12 <- hourly_t12 %>%
          dplyr::select(dt, temp)
        hourly_t12$dt <- as.POSIXct(hourly_t12$dt, origin = "1970-01-01", tz = 'UTC')

        # 13:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t13
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t13 <- ow$data
        hourly_t13 <- hourly_t13 %>%
          dplyr::select(dt, temp)
        hourly_t13$dt <- as.POSIXct(hourly_t13$dt, origin = "1970-01-01", tz = 'UTC')

        # 14:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t14
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t14 <- ow$data
        hourly_t14 <- hourly_t14 %>%
          dplyr::select(dt, temp)
        hourly_t14$dt <- as.POSIXct(hourly_t14$dt, origin = "1970-01-01", tz = 'UTC')

        # 15:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t15
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t15 <- ow$data
        hourly_t15 <- hourly_t15 %>%
          dplyr::select(dt, temp)
        hourly_t15$dt <- as.POSIXct(hourly_t15$dt, origin = "1970-01-01", tz = 'UTC')

        # 16:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t16
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t16 <- ow$data
        hourly_t16 <- hourly_t16 %>%
          dplyr::select(dt, temp)
        hourly_t16$dt <- as.POSIXct(hourly_t16$dt, origin = "1970-01-01", tz = 'UTC')

        # 17:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t17
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t17 <- ow$data
        hourly_t17 <- hourly_t17 %>%
          dplyr::select(dt, temp)
        hourly_t17$dt <- as.POSIXct(hourly_t17$dt, origin = "1970-01-01", tz = 'UTC')

        # 18:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t18
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t18 <- ow$data
        hourly_t18 <- hourly_t18 %>%
          dplyr::select(dt, temp)
        hourly_t18$dt <- as.POSIXct(hourly_t18$dt, origin = "1970-01-01", tz = 'UTC')

        # 19:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t19
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t19 <- ow$data
        hourly_t19 <- hourly_t19 %>%
          dplyr::select(dt, temp)
        hourly_t19$dt <- as.POSIXct(hourly_t19$dt, origin = "1970-01-01", tz = 'UTC')

        # 20:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t20
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t20 <- ow$data
        hourly_t20 <- hourly_t20 %>%
          dplyr::select(dt, temp)
        hourly_t20$dt <- as.POSIXct(hourly_t20$dt, origin = "1970-01-01", tz = 'UTC')

        # 21:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t21
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t21 <- ow$data
        hourly_t21 <- hourly_t21 %>%
          dplyr::select(dt, temp)
        hourly_t21$dt <- as.POSIXct(hourly_t21$dt, origin = "1970-01-01", tz = 'UTC')

        # 22:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t22
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t22 <- ow$data
        hourly_t22 <- hourly_t22 %>%
          dplyr::select(dt, temp)
        hourly_t22$dt <- as.POSIXct(hourly_t22$dt, origin = "1970-01-01", tz = 'UTC')

        # 23:00
        url <- paste0("http://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",lat
                      , "&lon=", lon
                      , "&dt=", my_dt_t23
                      , "&appid=", API_key
                      , "&units=", units)
        ow <- fromJSON(url)
        hourly_t23 <- ow$data
        hourly_t23 <- hourly_t23 %>%
          dplyr::select(dt, temp)
        hourly_t23$dt <- as.POSIXct(hourly_t23$dt, origin = "1970-01-01", tz = 'UTC')

        # combine all hourly temperatures into one dataframe called hourly
        hourly <- rbind(hourly_t0,
                        hourly_t1,
                        hourly_t2,
                        hourly_t3,
                        hourly_t4,
                        hourly_t5,
                        hourly_t6,
                        hourly_t7,
                        hourly_t8,
                        hourly_t9,
                        hourly_t10,
                        hourly_t11,
                        hourly_t12,
                        hourly_t13,
                        hourly_t14,
                        hourly_t15,
                        hourly_t16,
                        hourly_t17,
                        hourly_t18,
                        hourly_t19,
                        hourly_t20,
                        hourly_t21,
                        hourly_t22,
                        hourly_t23)
        #to get 2 days of hourly forecasts
        url2 <- paste0("http://api.openweathermap.org/data/3.0/onecall?lat=",lat
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

        # now have to omit three day weather forecast due to changes in OpenWeather API 3.0

        # merge all predictions based on dt, average values if they differ
        forecast <- rbind(hourly, hourly2)

        #average out daily predictions incase there are repeats
        forecast <- stats::aggregate(temp ~ dt, forecast, mean)

        # create a data frame that matches gov. of canada
        Longitude <- rep(c("-116.5007"), times=nrow(forecast))
        Latitude <- rep(c("49.0817"), times=nrow(forecast))
        Station.Name <- rep(c("Creston"), times=nrow(forecast))
        ClimateID <- rep(c("NA"), times=nrow(forecast))
        Date.Time.LST <- forecast$dt
        Year <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%Y"))
        Month <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%m"))
        Day <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%d"))
        Time.LST <- c(format(as.POSIXct(forecast$dt,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S"))
        Flag <- rep(c("NA"), times=nrow(forecast)) # April 2025, added because 'Flag' column added to Gov. of Canada historic climate datasets
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
        Creston_forecast <- data.frame(Longitude, Latitude, Station.Name, ClimateID, Date.Time.LST,
                                       Year, Month, Day, Time.LST, Flag, #April 2025 added 'Flag'
                                       Temp.degC, Temp.Flag, Dew.Point.Temp.degC,
                                       Dew.Point.Temp.Flag, Rel.Hum, Rel.Hum.Flag, Precip.Amount.mm, Precip.Amount.Flag,
                                       Wind.Dir.10s.deg, Wind.Dir.Flag, Wind.Spd.km.h,
                                       Wind.Spd.Flag, Visibility.km, Visibility.Flag,
                                       Stn.Press.kPa, Stn.Press.Flag, Hmdx, Hmdx.Flag,
                                       Wind.Chill, Wind.Chill.Flag, Weather)

        # add predictions to data frame
        infile <- rbind(infile, Creston_forecast)
        
      }
      
      ###############################################################################################################################################################################################################################################################
      
        # OCT 2026 UPDATE
      # if nothing is selected or uploaded (so site doesn't immediately crash)
    } else if (is.null(infile)) {
      return(NULL)
    }
          
          
        }
        
        
        # Keep the two tabs independent: each uses its own location dropdown.
        # The uploaded CSV remains shared between tabs, as described in the UI.
        data_input <- reactive({
          req(input$location)
          get_weather_data(input$location, input$csv_input)
        }) %>% bindEvent(input$run_button) # <-- ONLY runs when this button is clicked
        
        rf_data_input <- reactive({
          req(input$rf_location)
          get_weather_data(input$rf_location, input$csv_input)
        }) %>% bindEvent(input$rf_run_button) # <-- ONLY runs when this button is clicked
  
  #render image
  
  lat <- c(49.5626, 49.4625, 49.9408, 50.22, 49.08)
  lat <- as.data.frame(lat)
  map_data <- lat
  map_data$long <- c(-119.6487, -119.602, -119.4002, -119.19, -116.50)
  map_data$station <- c('Summerland', "Penticton", "Kelowna", "Vernon", "Creston")
  
  map_data$lat <- as.numeric(map_data$lat)
  map_data$long <- as.numeric(map_data$long)
  output$map <- renderLeaflet({
    leaflet(map_data) %>% addTiles() %>% 
      addMarkers(~long, ~lat, popup = ~as.character(station))
  })
  
  # Oct 2026 update (seems that each leaflet needs to be rendered independently if you want to use the same map twice, thats no good) 
  output$map_1 <- renderLeaflet({
    leaflet(map_data) %>% addTiles() %>% 
      addMarkers(~long, ~lat, popup = ~as.character(station))
  })
  
  
  # Create a data frame out of data_input() with calculated CU and FU using CU_FU function
  # You need 3 separate Calculated_CU_FU variables so that the same variable is not being called on in each time 
  # (was causing issues with downloading the app somewhere else without these unique variables)
  
  # FOR ORIGINAL MODEL
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
  }) %>% bindEvent(input$run_button) # <-- ONLY runs when this button is clicked
  
  Calculated_LT50 <- reactive({
    if (input$variety == "sweetheart"){
      LT50calc <- CH_LT50(Calculated_CU_FU50())
    }
    if (input$variety == "lapins"){
      LT50calc <- CH_LT50_lapins(Calculated_CU_FU50())
    }
    LT50calc
  }) %>% bindEvent(input$run_button) # <-- ONLY runs when this button is clicked
  
  Calculated_LT90 <- reactive({
    if (input$variety == "sweetheart"){
      LT90calc <- CH_LT90(Calculated_CU_FU90())
    } 
    if (input$variety == "lapins"){
      LT90calc <- CH_LT90_lapins(Calculated_CU_FU90())
    }
    LT90calc
  }) %>% bindEvent(input$run_button) # <-- ONLY runs when this button is clicked
  
  #import test data to make sure this is working (must be done in console)
  
  # TestData <- read.csv("/Users/Elizabeth/Desktop/TestData/TestData.csv")
  # Calculated_CU_FU <- CU_FU(TestData)
  # Merge Calculated_LT10, Calculated_LT50, and Calculated_LT90, select only important columns (for data download)
  
  All_LTs <- reactive({
    LTs <- merge(Calculated_LT10(), Calculated_LT50(), by.x="YYYYMMDD", by.y="YYYYMMDD50")
    LTs <- merge(LTs, Calculated_LT90(), by.x="YYYYMMDD", by.y="YYYYMMDD90")
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
    
    LTs$Predicted.damage <- ifelse(LTs$LT10 >= LTs$Temp_min, "10%", "-")
    LTs$Predicted.damage <- ifelse(LTs$LT50 >= LTs$Temp_min, "50%", LTs$Predicted.damage)
    LTs$Predicted.damage <- ifelse(LTs$LT90 >= LTs$Temp_min, "90%", LTs$Predicted.damage)
    
    # round to 1 decimal places
    LTs$LT10 <- format(round(LTs$LT10, 1), nsmall = 1) 
    LTs$LT50 <- format(round(LTs$LT50, 1), nsmall = 1)
    LTs$LT90 <- format(round(LTs$LT90, 1), nsmall = 1)
    LTs$Temp_min <- format(round(LTs$Temp_min, 1), nsmall = 1)
    LTs$Weather.data = "historic"
    LTs$Weather.data[LTs$YYYYMMDD >= as.Date(Sys.Date())] = "predictions"
    #sort by date
    LTs <- LTs[order(as.Date(LTs$YYYYMMDD, format="%Y-%m-%d")),]
    LTs <- LTs  %>% 
      dplyr::rename("YYYY-MM-DD" = "YYYYMMDD")
    LTs <- LTs  %>% 
      dplyr::rename("Temp.min_C" = "Temp_min")
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
  
  # FOR RF MODEL
  Calculated_CU_FU50_rf <- reactive({
    CUFU_rf <- CU_FU_rf(rf_data_input())
    CUFU_rf
  })
  Calculated_CU_FU90_rf <- reactive({
    CUFU_rf <- CU_FU_rf(rf_data_input())
    CUFU_rf
  })
  
  # create a data frame out of data_input() with calculated LT using LT10,LT50,LT90 functions THIS IS WHERE YOU NEED YOUR VARIETY DROP DOWN MENU SELECTION TO COME INTO PLAY (CH_LT10 vs CH_LT10_lapins
  Calculated_LT50_rf <- reactive({
    req(input$rf_variety)
    if (input$rf_variety == "sweetheart"){
      LT50calc_rf <- CH_LT50_rf_sweetheart(Calculated_CU_FU50_rf())
    }
    if (input$rf_variety == "skeena"){
      LT50calc_rf <- CH_LT50_rf_skeena(Calculated_CU_FU50_rf())
    }
    if (input$rf_variety == "lapins"){
      LT50calc_rf <- CH_LT50_rf_lapins(Calculated_CU_FU50_rf())
    }
    LT50calc_rf
  }) %>% bindEvent(input$rf_run_button) # <-- ONLY runs when this button is clicked
  
  Calculated_LT90_rf <- reactive({
    req(input$rf_variety)
    if (input$rf_variety == "sweetheart"){
      LT90calc_rf <- CH_LT90_rf_sweetheart(Calculated_CU_FU90_rf())
    }
    if (input$rf_variety == "skeena"){
      LT90calc_rf <- CH_LT90_rf_skeena(Calculated_CU_FU90_rf())
    }
    if (input$rf_variety == "lapins"){
      LT90calc_rf <- CH_LT90_rf_lapins(Calculated_CU_FU90_rf())
    }
    LT90calc_rf
  }) %>% bindEvent(input$rf_run_button) # <-- ONLY runs when this button is clicked
  
  #import test data to make sure this is working (must be done in console)
  
  # TestData <- read.csv("/Users/Elizabeth/Desktop/TestData/TestData.csv")
  # Calculated_CU_FU <- CU_FU(TestData)
  # Merge Calculated_LT10, Calculated_LT50, and Calculated_LT90, select only important columns (for data download)
  
  All_LTs_rf <- reactive({
    df_LT50 <- Calculated_LT50_rf()
    df_LT90 <- Calculated_LT90_rf()
    df_LT50 <- df_LT50 %>% 
      rename(YYYYMMDD = YYYYMMDD50,
             LT50 = LT_temp)
    df_LT90 <- df_LT90 %>% 
      rename(YYYYMMDD = YYYYMMDD90,
             LT90 = LT_temp)
    LTs <- merge(df_LT50, df_LT90, by = c("YYYYMMDD", "Temp_min", "Station.Name", "variety"))
    #LTs <- merge(Calculated_LT50_rf(), Calculated_LT90_rf(), by.x="YYYYMMDD50", by.y="YYYYMMDD90")
    LTs <- LTs %>% dplyr::select(YYYYMMDD, LT50, LT90, Station.Name, Temp_min, variety) # keep minimum air temps in data frame
    # LTs$YYYYMMDD <- as.Date(LTs$YYYYMMDD50)
    LTs <- LTs %>%
      tidyr::separate(YYYYMMDD, into = c("YYYY", "MM", "DD"), "-")
    LTs <- subset(LTs, MM >= "09" | MM <= "04")
    LTs$YYYYMMDD <- as.Date(with(LTs, paste(YYYY, MM, DD, sep="-")),"%Y-%m-%d")
    # LTs <- LTs %>% dplyr::select(YYYYMMDD, LT50, LT90, Temp_min)
    # highlight if the min air temp fell below the LT10, LT50, or LT90
    LTs$LT50 <- as.numeric(LTs$LT50)
    LTs$LT90 <- as.numeric(LTs$LT90)
    LTs$Temp_min <- as.numeric(LTs$Temp_min)
  
     LTs$Predicted.damage <- ifelse(LTs$LT50 >= LTs$Temp_min, "50%", "-")
     LTs$Predicted.damage <- ifelse(LTs$LT90 >= LTs$Temp_min, "90%", LTs$Predicted.damage)
    
    # round to 1 decimal places
    LTs$LT50 <- format(round(LTs$LT50, 1), nsmall = 1)
    LTs$LT90 <- format(round(LTs$LT90, 1), nsmall = 1)
    LTs$Temp_min <- format(round(LTs$Temp_min, 1), nsmall = 1)
    LTs$Weather.data = "historic"
    LTs$Weather.data[LTs$YYYYMMDD >= as.Date(Sys.Date())] = "predictions"
    #sort by date
    LTs <- LTs[order(as.Date(LTs$YYYYMMDD, format="%Y-%m-%d")),]
    LTs <- LTs  %>% 
      dplyr::rename("YYYY-MM-DD" = "YYYYMMDD")
    LTs <- LTs  %>% 
      dplyr::rename("Temp.min_C" = "Temp_min")
    
    
    LTs
    
  })
  
  # To download the .csv file of lethal temperature data to your computer
  output$rf_downloadData <- downloadHandler(
    filename = function() {
      paste("Lethal_Temp_Estimates_", Calculated_LT50_rf()$Station.Name, "_", Calculated_LT50_rf()$variety, ".csv", sep = "") ### added 'Calculated_LT10()$Variety,'
    },
    content = function(file) {
      write.csv(All_LTs_rf(), file, row.names = FALSE)
    }
  )
  
  # When the run button is hit, draw plots and render text
  
  # Oct 2026 update
  
  # Combined interactive LT10, LT50, and LT90 graph
  
  # FOR ORIGINAL MODEL
  output$plot_LT_combined <- renderPlotly({
    
    # Wait until the user has run the analysis
    req(input$run_button > 0)
    
    req(
      Calculated_LT10(),
      Calculated_LT50(),
      Calculated_LT90()
    )
    
    draw_plot_LT_combined(
      Calculated_LT10 = Calculated_LT10(),
      Calculated_LT50 = Calculated_LT50(),
      Calculated_LT90 = Calculated_LT90()
    )
  })
  
  output$description_combined <- renderUI({
    
    req(input$run_button > 0)
    
    HTML(paste(
      "<span style='font-family: \"Lato\", sans-serif;'>","This plot shows the estimated temperatures that will cause",
      "<span style='color: #A9C9E2;'>10%</span>, <span style='color: #4B8BBE;'>50% </span>, and <span style='color: #1D4E89;'>90%</span> of sweet cherry flower buds to experience",
      "cold damage (<span style='color: #A9C9E2;'>LT10</span>, <span style='color: #4B8BBE;'>LT50</span>, and <span style='color: #1D4E89;'>LT90</span>). Shaded areas represent",
      "the 95% confidence intervals. Solid lines show estimates",
      "based on recorded weather, while dotted lines show estimates",
      "based on weather predictions. The <span style='color: firebrick;'>red line</span> represents daily",
      "minimum air temperature. Hover over the estimates to view",
      "the predicted lethal temperature and its confidence interval."
    ))
  })
  
  
  # FOR RF MODEL
  output$rf_plot_LT_combined <- renderPlotly({
    
    # Wait until the user has run the analysis
    req(input$rf_run_button > 0)
    
    req(
      Calculated_LT50_rf(),
      Calculated_LT90_rf()
    )
    
    rf_draw_plot_LT_combined(
      Calculated_LT50_rf = Calculated_LT50_rf(),
      Calculated_LT90_rf = Calculated_LT90_rf()
    )
  })
  
  output$rf_description_combined <- renderUI({
    
    req(input$rf_run_button > 0)
    
    HTML(paste(
      "<span style='font-family: \"Lato\", sans-serif;'>","This plot shows the estimated temperatures that will cause",
      "<span style='color: #4B8BBE;'>50% </span> and <span style='color: #1D4E89;'>90%</span> of sweet cherry flower buds to experience",
      "cold damage (<span style='color: #4B8BBE;'>LT50</span> and <span style='color: #1D4E89;'>LT90</span>). Shaded areas represent",
      "the 95% confidence intervals. Solid lines show estimates",
      "based on recorded weather, while dotted lines show estimates",
      "based on weather predictions. The <span style='color: firebrick;'>red line</span> represents daily",
      "minimum air temperature. Hover over the estimates to view",
      "the predicted lethal temperature and its confidence interval."
    ))
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
