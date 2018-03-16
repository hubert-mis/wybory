#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("wybory.R")

server <- function(input, output) {
  
  surv <- reactive({
    c(as.numeric(input$par1), as.numeric(input$par2), as.numeric(input$par3))
  })
  freq <- reactive({
    as.numeric(input$fr)
  })
  
  # output$trial <- reactive({
  #   surv()
  #   
  # })

  xdd <- eventReactive(input$button, {
    elect.sym(data_cons, surv(), freq()/100)
  })
  output$table1 <- renderTable({
    xdd()
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0("Symulacja", ".csv")
    },
    content = function(file) {
      write.csv(xdd(), file, row.names = FALSE, sep = ';', dec = ',')
    }
  )
}