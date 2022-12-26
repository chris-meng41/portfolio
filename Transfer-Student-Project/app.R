# Load data and libraries
library(shiny)
source("ui.R")
source("server.R")

# Create Shiny application
shinyApp(ui = ui, server = server)