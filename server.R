#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
source("wybory.R")

server <- function(input, output) {
  
  surv <- reactive({
    c(as.numeric(input$par1), as.numeric(input$par2), as.numeric(input$par3))
  })
  freq <- reactive({
    as.numeric(input$fr)
  })

  data <- eventReactive(input$button, {
    if(input$rad == 1)
      elect.sym(data_cons, surv(), freq()/100)
    else if (input$rad == 2)
      elect.sym(data_cons, surv(), freq()/100, method = saintlague)
  })
  
  a <- reactive({
    data()[nrow(data()), (ncol(data()) - 2) : ncol(data())]
  })
  b <- reactive({
    data.frame(mandaty = c(a()[1,1], a()[1,2], a()[1,3]),
               partie = c("Partia 1", "Partia 2", "Partia 3"))
  })
  output$plot1 <- renderPlot({
    ggplot(data = b(), aes(x=partie, y=mandaty)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=mandaty), vjust=1.6, color="white", size=4.5)+
      theme_minimal()
  })
  output$table1 <- renderTable({data()})

  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0("Symulacja", ".csv" )},
    
    content = function(file) {
      write.csv(data(), file, row.names = FALSE, sep = ';', dec = ',')
    })
}