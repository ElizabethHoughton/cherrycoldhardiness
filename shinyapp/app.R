#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shinythemes")
library(markdown)
library(ggplot2)
library(grid)
library(png)

# These are required for data manipulation and modelling
library(plyr)
library(tidyverse)
library(MuMIn)
library(nlme)
library(report)
library(hydroGOF)
library(Metrics)
library(knitr)
library(lubridate)
library(zoo)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
 
                navbarPage("Sweet Cherry Cold Hardiness Estimations", #title of Navbar
                           tabPanel("About",
                                    mainPanel(
                                        h3("Title"), # size and text for paragraph
                                        p("Paragraph text"),
                                        br(),
                                        p("Paragraph text"),
                                        img(height = 300, width = 400, src = "winterorchard.png"),
                                        br(),
                                        a(href="https://github.com/ElizabethHoughton/cherry-cold-hardiness", "GitHub"))), #active weblink to GitHub)),
                           tabPanel("How to Use",
                                    mainPanel(
                                      h3("Instructions"), # size and text for paragraph
                                      p("Instructions on what data in what format that they need to upload "),
                                      br(), #line break
                                      h3("Don't have your own data?"),
                                      p("This will only be here with instuctions to Gov. Canada Historic climate 
                                        data if I can't figure out how to link the data to the shiny app.")
                                    )),
                           tabPanel("Estimations",
                                    fileInput(inputId='files', 
                                              label="Upload Hourly Temperature Data (.csv)",
                                              multiple = TRUE,
                                              accept = '.csv',
                                              buttonLabel = "Browse...",
                                              placeholder = "No file selected"),
                                    plotOutput("LT10"),
                                    plotOutput("LT50"),
                                    plotOutput("LT90")))
)

  
# Define server logic required
server <- function(input, output) {
  data <- reactive({
    rbindlist(lapply(input$files$datapath, fread),
              use.names = TRUE, fill= TRUE)
    }) # merge all the data files that have been uploaded into one
  output$LT10 <- renderPlot({#plug data() into your code for LT10 model
  })
  output$LT50 <- renderPlot({#plug data() into your code for LT50 model
  })
  output$LT90 <- renderPlot({#plug data() into your code for LT90 model
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Notes
# Gov of Canada API https://fromthebottomoftheheap.net/2016/05/24/harvesting-more-canadian-climate-data/
