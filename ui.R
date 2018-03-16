#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Symulacja Wyborow"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("par1", h5("Partia 1"), "30"),
      textInput("par2", h5("Partia 2"), "20"),
      textInput("par3", h5("Partia 3"), "10"),
      textInput("fr", h5("Nie ide na wybory"), "40"),
      br(),actionButton("button", "Run"),
      downloadButton('downloadData', 'Download')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table1")
      # verbatimTextOutput("trial")
    )
  )
)