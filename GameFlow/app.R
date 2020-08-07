#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # tab - game-level stats
  #   Sidebar panel with calendar selector, and then games on the left with score, time, and logos
  #   main game flow plot
  #   table below that, when you scroll over a time it shows the lineup(s) at that time
  # tab - Season-level stats
  #   
)

server <- function(input, output) {
   output$distPlot <- renderPlot({})
}

shinyApp(ui = ui, server = server)

