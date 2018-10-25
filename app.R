#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(magick)
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
  htmlOutput('op'),
  plotOutput('plot')
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
      
      text <- unlist(as.character(content[[x]]) %>% strsplit('\n'))
      data <- data_frame(text = text) %>% filter(text!='') %>% mutate(line = c(1:length(text))) %>% unnest_tokens(word,text) %>% inner_join(afinn,by = 'word') %>% group_by(line) %>% summarise(sum = sum(score)) %>% mutate( index = 1,color = ifelse(sum > 0,'green','red')) 
      words <- data_frame(text = text) %>% filter(text != '') %>% unnest_tokens(words,text)
      time <- floor( nrow(words)/ 200 )
      index <- x%%3
      
      ggplot(data = data,aes(x = line,y = index , fill=color)) + geom_bar(stat="identity") + scale_fill_manual( values = c("green" = "mediumseagreen", "red" = "indianred")) + theme_void()  + theme(legend.position = "none")
      fileName <- paste0('www/','grp','_',x,'.png');
      ggsave(file = fileName);
      image <- image_trim(image_read(path = fileName));
      image <- image_crop(image = image, "3071x150");
      image_write(path = fileName,image = image);
      if(index == 0){
        index <- 3;
      }
      
      card <- tags$div(class = 'card h-100',style = 'border-radius: 10px;', tags$div(class = 'card-body', tags$h5(class = 'card-title',hd[x]), tags$img(src = paste0('grp','_',x,'.png'),class="img-fluid"),tags$div(tags$a(href = refurls[x],tags$img(src='toi.png',class = 'rounded')),img(src='green.png',class = 'rounded' )   )))
      row[[index]] <- tags$div(class = 'col-sm col-xs-12 container p-1',card)
      if(x%%3 == 0){
        rows[[x%/%3]] <- list(fluidRow(row));
      }
    }
    return(rows);
  })
}

# Run the application 
shinyApp(ui = ui, server = server)