# Load data and libraries
library(shiny)
source("ui2.R")
source("server2.R")

# Create Shiny application
shinyApp(ui = ui2, server = server2)