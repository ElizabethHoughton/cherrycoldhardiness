#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# download packages
library(shiny)
library(shinydashboard)
library("shinythemes")
library(markdown)
library(ggplot2)
library(grid)
library(png)
library(devtools)



# creates a description file
# use_description()

# wrap entire shiny app in a function, create a NAMESPACE file too

# CH <- function() {

# the user interface
ui <- fluidPage(theme = shinytheme("flatly"),
                # Navbar title
                navbarPage("Sweet Cherry Cold Hardiness Estimations",
                           # first tab panel
                           tabPanel("About",
                                    # inputs for first tab pane;
                                    mainPanel(
                                        h3("Title"), # size and text for paragraph
                                        p("Paragraph text"),
                                        br(),
                                        p("Paragraph text"),
                                        img(height = 300, width = 400, src = "winterorchard.png"),
                                        br(),
                                        a(href="https://github.com/ElizabethHoughton/cherry-cold-hardiness", "GitHub"))), #active weblink to GitHub)),
                           # second tab panel
                           tabPanel("How to Use",
                                    mainPanel(
                                        h3("Instructions"), # size and text for paragraph
                                        p("Instructions on what data in what format that they need to upload "),
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
                                 actionButton("run_button", "Run Analysis"),
                                 #Add a button for saving any calculated corrections as a .csv
                                 downloadButton("downloadData", "Save results")
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
# FIRST NEED TO SOMEHOW MANIPULATE THE data_input to calculate CU AND FU
# THEN WITH THIS MANIPULATED DATA WE NEED TO RUN IT THROUGH EACH MODEL
# THEN WITH THIS DATA WE NEED TO DRAW THE PLOTS

# Define server logic required
# I think adding 'session' into the function allows you to access the functions created in your LT.R file
server <- function(input, output, session) {
  # data input as a data.table called "data_input" (data.table is similar in function to data.frame)
  data_input <- reactive({
    req(input$csv_input)
    # gives error code if not a .csv file
    ext <- tools::file_ext(input$csv_input$name)
    validate(need(ext == "csv", "Invalid file. Please upload a .csv file"))
    fread(input$csv_input$datapath)
  })
  
  # When ever a new .csv file is uploaded calculate the CU and FU using CU_FU function
  # we want to calculate CU and FU from data_input()
  calculate_CU_FU <- reactive({
    CUFUcalculations <- CU_FU(data_input())
    CUFUcalculations
  })

  # we want to calculate LT10
  calculate_LT10 <- reactive({
    PredictLT10 <- LT10(calculate_CU_FU())
    PredictLT10
  })
  # we want to calculate LT50
  calculate_LT50 <- reactive({
    PredictLT50 <- LT50(calculate_CU_FU())
    PredictLT50
  })
  # we want to calculate LT10
  calculate_LT90 <- reactive({
    PredictLT90 <- LT90(calculate_CU_FU())
    PredictLT90
  })
  
#  })

  # plot LT10
  
  plot_LT10 <- eventReactive(input$run_button,
    {plot(LT10 ~ YYYMMDD, data = calculate_LT10(), # CHANGE THIS IF YOU RENAMTE YOUR data_input 
         main= "Lethal Temperature for 10% Bud Damage",
         pch= 20,
         xlab="Date", 
         ylab="LT10 (˚C)",
         cex.main=1.25, 
         xlim= c(300, 460),
         cex.lab=1, ylim=c(-30, 0))
    lines(PredictLT10$YYYMMDD, PredictLT10$Model10, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(PredictLT10$YYYMMDD, PredictLT101$LT10.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
    lines(PredictLT10$YYYMMDD, PredictLT10$LT10.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
  })
  
  output$plot_LT10 <- renderPlot(plot_LT10())
  
  # plot LT50
  
  plot_LT50 <- eventReactive(input$run_button,
     {plot(LT50 ~ YYYMMDD, data = calculate_LT50(), # CHANGE THIS IF YOU RENAMTE YOUR data_input 
           main= "Lethal Temperature for 50% Bud Damage",
           pch= 20,
           xlab="Date", 
           ylab="LT50 (˚C)",
           cex.main=1.25, 
           xlim= c(300, 460),
           cex.lab=1, ylim=c(-30, 0))
    lines(PredictLT50$YYYMMDD, PredictLT50$Model50, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
    lines(PredictLT50$YYYMMDD, PredictLT501$LT50.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
    lines(PredictLT50$YYYMMDD, PredictLT50$LT50.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
  })
  
  output$plot_LT50 <- renderPlot(plot_LT50())
  
  # plot LT90
  
  plot_LT90 <- eventReactive(input$run_button,
    {plot(LT90 ~ YYYMMDD, data = calculate_LT90(), # CHANGE THIS IF YOU RENAMTE YOUR data_input 
          main= "Lethal Temperature for 90% Bud Damage",
          pch= 20,
          xlab="Date", 
          ylab="LT90 (˚C)",
          cex.main=1.25, 
          xlim= c(300, 460),
          cex.lab=1, ylim=c(-30, 0))
      lines(PredictLT90$YYYMMDD, PredictLT90$Model90, lty = 1) #WHATEVER YOUR PREDICTIONS ARE LABELLED
      lines(PredictLT90$YYYMMDD, PredictLT901$LT90.CIUpper, lty = 2) #WHATEVER YOUR CIs ARE LABELLED
      lines(PredictLT90$YYYMMDD, PredictLT90$LT90.CILower, lty = 2)#WHATEVER YOUR CIs ARE LABELLED
})
  
  output$plot_LT90 <- renderPlot(plot_LT90())
  
  
  # Downloadable csv of corrected areas ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "Lethal_Temp_Estimates.csv",".csv", sep = "") # input$dataset might not make sense here
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

# }

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
#  rename(Temp = "Temp (°C)")

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
