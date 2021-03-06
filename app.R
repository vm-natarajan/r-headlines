#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);
library(ggplot2);
library(dplyr);
library(tidyr);
library(tidytext);
library(magick);
library(rdrop2);
library(shinyjs);
# Define UI for application that draws a histogram
ui <- fluidPage( useShinyjs(),tags$head(
  tagList(
    suppressDependencies("bootstrap"),
    tags$link(
      rel="stylesheet",
      href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css",
      integrity="sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy",
      crossorigin="anonymous"
    ),
    tags$link(
      rel="stylesheet",
      href="blog.css"
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
  )),htmlOutput("inc")
  ,htmlOutput("others")
  #includeHTML(path = 'index.html')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  drop_auth(rdstoken = 'auth/token.rds');
  data <- drop_read_csv("stories/td.csv",stringsAsFactors = FALSE);
   
  getPage<-function(dataset = data) {
    
    hd <- data$title;
    refurls <- data$url;
    content <- data$description;
    section <- data$section;
    pub_date <- format(as.Date(data$date,format = "%m/%d/%Y"),'%b %d');
    size <- length(hd);
    cardId <- vector("list",size)
    
    header <- includeHTML(path = 'html/header.html');
    menus <- tags$nav(class = 'nav d-flex justify-content-between',list(actionLink('world','World',class='p-2 text-muted'),actionLink('india','India',class='p-2 text-muted'),actionLink('technology','Technology',class='p-2 text-muted'),actionLink('top-news','Top News',class='p-2 text-muted'),actionLink('business','Business',class='p-2 text-muted'),actionLink('politics','Politics',class='p-2 text-muted'),actionLink('science','Science',class='p-2 text-muted'),actionLink('health','Health',class='p-2 text-muted'),actionLink('life-style','Life Style',class='p-2 text-muted'),actionLink('travel','Travel',class='p-2 text-muted')))
    jumbotron <- includeHTML(path = 'html/jumbotron.html');
    
    cards <- lapply(c(1:size), function(X){
      
      details = list( tags$div(class = 'container d-flex justify-content-between p-0' ,tags$a(href = section[X], tags$strong(class = 'd-inline-block mb-2 text-success',section[X])),tags$div(class = 'mb-1 text-muted',pub_date[X]) ) , tags$h4(class='mb-1',hd[X]),tags$p(class = 'card-text mb-auto', style = 'font-size: 14px;',content[X]),tags$a(href=refurls[X],'Continue reading'));
      cardId[[X]] <- tags$div(class = 'col-md-6',tags$div(class = 'card flex-md-row mb-4 shadow-sm h-md-250',tags$div(class = 'card-body d-flex flex-column align-items-start p-3',details)))
      return(cardId[[X]]);
      
    })
    
    footer <- includeHTML(path = 'html/footer.html');
    cardholder <- tags$div(cards,class = 'row mb-2');
    container <- tags$div(header,menus,jumbotron,cardholder,footer,class = 'container')
    return(container);
    
  }
  
  observeEvent(input$world, {
    data <- data[data$section == 'world',];
    hide("inc");
    output$others<- output$inc<-renderUI({getPage(dataset = data)})
  })
  
  observeEvent(input$india, {
    data <- data[data$section == 'india',];
    hide("inc");
    output$others<- output$inc<-renderUI({getPage(dataset = data)})
  })
  
  output$inc<-renderUI({getPage(data)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)