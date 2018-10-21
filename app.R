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
  htmlOutput('op')
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  size <- length(hd);
  rows <- vector("list",(size/3))   
  output$op <- renderUI({
    row <- vector("list",3)
    for(x in c(1:size)) {
      index <- x%%3
      if(index == 0){
        index <- 3;
      }
      row[[index]] <- list( column(4,h3(paste0('TOI : ',hd[x]))));
      if(x%%3 == 0){
        rows[[x%/%3]] <- list(fluidRow(row));
      }
    }
    return(rows);
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

