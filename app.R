#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#5th april - fixed bug Yersinia spp. > Yersinia enterolitica, 
#replace the link continue to risk ranking with the correct url
#push update to github and republish
#why is the link not showing? and the HPP categories needs to be updated
##with the correct inactivation number and that will be the same with the rr app

library(shiny)
library(rlang)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(writexl)
library(DT)
library(data.table)
library(markdown)
library(shinyjs)
library(bslib)
library(shinyFeedback)
library(glue)
library(shinybrowser)
library(bslib)
library(spsComps)
### 

# Run the application 
shinyApp(ui = UI, server = Server)