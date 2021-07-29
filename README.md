---
title: "README""
author: "Elizabeth Houghton"
output: html_document
---
## cherrycoldhardiness

A shiny app for estimating the 10, 50, and 90% lethal temperatures (LT10, LT50, LT90) of the sweet cherry variety 'Sweetheart' in the Okanagan Valley of British Columbia, Canada.

This app can be used online at: [link] (https://sweetcherry.shinyapps.io/cherrycoldhardiness/) *currently not working*

Or locally, via an R installation. To install and run the app run the following lines in R:

```{r eval=FALSE}

# Get list of the currently installed packages
avail <- installed.packages()[, 1]

# Make a list of the required packages
needed <- c("shiny",
    "shinydashboard",
    "shinythemes",
    "ggplot2",
    "dplyr",
    "png",
    "data.table",
    "zoo",
    "stats",
    "nlme",
    "car",
    "lubridate"
)
# This list will eventually be shortened

# Check which of these are missing
install <- needed[!needed %in% avail]

# And then install them if necessary
if (length(install) > 0) {
  install.packages(install)
}

# After installing the dependencies,
# install cherrycoldhardiness from the github repository
devtools::install_github(repo = "ElizabethHoughton/cherrycoldhardiness")


# Run the application
cherrycoldhardiness()
```