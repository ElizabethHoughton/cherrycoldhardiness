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
                                        p("This model can be used to estimate the the cold hardiness of 
                                          the sweet cherry cultivar Sweetheart grafted on Mazzard 
                                          rootstock within the Okanagan Valley of 
                                          British Columbia."),
                                        br(),
                                        p("Paragraph text"),
                                        tags$img(height = 300, width = 400, src = "winterorchard.png"),
                                        br(),
                                        a(href="https://github.com/ElizabethHoughton/cherry-cold-hardiness", "GitHub"))), #active weblink to GitHub)),
                             # second tab panel
                             tabPanel("How to Use",
                                      mainPanel(
                                        h3("Instructions"), # size and text for paragraph
                                        p("To use this model, hourly temperature data is required..."),
                                        br(), #line break
                                        h3("Don't have your own data?"),
                                        p("This will only be here with instuctions to Gov. Canada Historic climate 
                                        data if I can't figure out how to link the data to the shiny app.")
                                      )),
                             # third tab panel
                             tabPanel(
                               title = "Estimations",
                               sidebarLayout(
                                 sidebarPanel(
                                   title = "Inputs",
                                   fileInput("csv_input", "Select CSV File to Import", accept = ".csv", multiple = TRUE),
                                   actionButton("run_button", 
                                                "Run Analysis",
                                                style="margin-top: 12%"),
                                   #Add a button for saving any calculated corrections as a .csv
                                   downloadButton("downloadData", 
                                                  "Save results",
                                                  style= "margin-top: 12%") #not currently functional
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
        cex.lab=1, ylim=c(-30, 0))
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT10$YYYYMMDD, Calculated_LT10$LT10.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
    legend("bottomright", legend=c("Estimated Values", "95% Confidence Intervals"), col=c("black", "black"), lty=1:2, cex=0.8) # add legend
  }
  
  
  # plot LT50
  
  draw_plot_LT50 <- function(Calculated_LT50)
  {plot(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50, # CHANGE THIS IF YOU RENAMTE YOUR data_input 
        main= "Lethal Temperature for 50% Bud Damage",
        pch= NA,
        xlab="Date", 
        ylab="LT50 (C)",
        cex.lab=1, ylim=c(-30, 0))
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT50$YYYYMMDD50, Calculated_LT50$LT50.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
    legend("bottomright", legend=c("Estimated Values", "95% Confidence Intervals"), col=c("black", "black"), lty=1:2, cex=0.8) # add legend
  }
  
  # plot LT90
  
  draw_plot_LT90 <- function(Calculated_LT90)
  {plot(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90, # CHANGE THIS IF YOU RENAMTE YOUR data_input 
        main= "Lethal Temperature for 90% Bud Damage",
        pch=NA,
        xlab="Date", 
        ylab="LT90 (C)",
        cex.lab=1, ylim=c(-30, 0))
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
    lines(Calculated_LT90$YYYYMMDD90, Calculated_LT90$LT90.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
    legend("bottomright", legend=c("Estimated Values", "95% Confidence Intervals"), col=c("black", "black"), lty=1:2, cex=0.8) # add legend
  }
  
  # Define server logic required
  # I think adding 'session' into the function allows you to access the functions created in your other .R files?
  server <- function(input, output, session) {
    # data input as a data.table called "data_input" (data.table is similar in function to data.frame, this is reactive
    # so you must call on it like it is a function from here on as "data_input()", will update with each new upload)
     data_input <- reactive({
      infile <- input$csv_input
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile$datapath)
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