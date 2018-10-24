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
ui <- fluidPage(tags$head(
  tagList(
    suppressDependencies("bootstrap"),
    tags$link(
      rel="stylesheet",
      href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css",
      integrity="sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy",
      crossorigin="anonymous"
    ),
    tags$script(
      src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js",
      integrity="sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q",
      crossorigin="anonymous"
    ),
    tags$script(
      src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.min.js",
      integrity="sha384-a5N7Y/aK3qNeh15eJKGWxsqtnX/wWdSZSKp+81YjTmS15nvnvxKHuzaWwXHDli+4",
      crossorigin="anonymous"
    ),
    tags$meta(
      name="viewport",
      content="width=device-width, initial-scale=1.0"
    )
  )),
  #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  htmlOutput('op')
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read.csv(file = 'stories.csv',stringsAsFactors = FALSE)
  hd <- data$title;
  refurls <- data$url;
  content <- data$content;
  size <- length(hd);
  rows <- vector("list",(size/3))   
  output$op <- renderUI({
    row <- vector("list",3)
    for(x in c(1:size)) {
      
      text <- as.character(content[[x]]) %>% strsplit('\n')
      
      words <- data_frame(text = text[[1]]) %>% filter(text != '') %>% unnest_tokens(words,text)
      
      time <- floor( nrow(words)/ 200 )
      
      index <- x%%3
      
      if(index == 0){
        index <- 3;
      }
      
      card <- tags$div(class = 'card h-100',style = 'border-radius: 10px;', tags$div(class = 'card-body', tags$h5(class = 'card-title',hd[x]), tags$p(class = 'card-text',paste0(nchar(content[[x]])),' - ',time),tags$a(href = refurls[x],tags$img(src='toi.png',class = 'rounded')) ))
      row[[index]] <- tags$div(class = 'col-sm col-xs-12 container p-1',card)
      #list( column(4, card ,class = 'container p-1'));
      if(x%%3 == 0){
        rows[[x%/%3]] <- list(fluidRow(row));
      }
    }
    return(rows);
  })
}

# Run the application 
shinyApp(ui = ui, server = server)