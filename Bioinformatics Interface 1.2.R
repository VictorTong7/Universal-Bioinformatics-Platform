library(shiny)
# need a few more now...
# if you need to install these to run on your local machine uncomment the following line:
#install.packages(c('dplyr','tidyr','ggplot2','RColorBrewer','readr','stringr','shiny','shinythemes','shinyjs'))
library(shinythemes)
library(shinyjs)

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(stringr)

# 2017-09-04
# I have uploaded the CSV containing all series (GSE) using the human Affymetrix array (GPL570)
# Our search will look through here for the appropriate keywords
# 
# In this version, I have just replaced the matrix (i.e. mtcards for all_gpl570)
# Also made the checkboxes within the table (from 'http://stackoverflow.com/questions/26026740/rstudio-shiny-list-from-checking-rows-in-datatables')
# Search function is not yet implemented in the UI
#
#
all_gpl570<-read.csv('~/Dropbox/Retina_app/all_gpl570_april.csv')

# search strings –– next version we take these from the UI
search_string_motorNeurons = "\\bbrain\\b|\\bneurons?(al)?\\b&\\bmotor\\b"
search_string_diabetes = "\\bpancreas\\b|\\bislets?\\b|\\bbeta-cells\\b|\\bdiabetes\\b"
search_string_hepatocytes = "\\bliver\\b&\\bhepatocytes?\\b"
search_string_cardiomyocytes = "\\bheart\\b&\\bcardiomyocytes?\\b"

selected <- all_gpl570[,c(2,3,8)]
selected$id <- 1:nrows(selected)
selected$category <- rep("Not assigned", nrow(selected))

ui <- fluidPage(
  #creation of a navigation bar and mulitple pages
  navbarPage("Bioinformatics Software",
             tabPanel("First Page",  
                      #setting user inputed values and displaying values back at the user
                      numericInput("Key", "Enter number of rows", value = 16),
                      verbatimTextOutput("Key")
                      ),
             tabPanel("Second Page", uiOutput("page2"), 
                      textInput("cat1", "Define Category 1"),
                      verbatimTextOutput("cat1"),
                      textInput("cat2", "Define Category 2"),
                      verbatimTextOutput("cat2"),
                      textInput("cat3", "Define Category 3"),
                      verbatimTextOutput("cat3")
                      ),
             tabPanel("Third Page", uiOutput("page3"), uiOutput("page3.1")
                      ),
             tabPanel("Fourth Page", uiOutput("page4"), 
                      dataTableOutput("SelectedRows")
                      )
  )
)

server <- function(input, output) {
  output$Key <- renderText(input$Key)
  output$cat1 <- renderText(input$cat1)
  output$cat2 <- renderText(input$cat2)
  output$cat3 <- renderText(input$cat3)
  
  # checkbox function
  rowSelect <- reactive({paste(sort(unique(input[["rows"]])),sep=',')
                         })
  output$page3 <- renderUI(
    fluidRow(
      column(3,
               selectInput("selection", "Select a Category",
                           c("category1" <- {input$cat1},
                             "category2" <- {input$cat2},
                             "category3" <- {input$cat3},
                             "category4" <- "Not included"))
      )
    )
  )
  
  output$page3.1 <- renderUI(
    fluidRow(renderDataTable({
      addCheckboxButtons<- paste0('<input type="checkbox" name="row', selected$id, '" value="', selected$id, '">',"")
      cbind(Pick=addCheckboxButtons, selected[, , drop=FALSE])
    }#)
    #))}         
    , options = list(orderClasses = TRUE, lengthMenu = c(5, 25, 50), pageLength = 25, escape=FALSE)
    , callback = "function(table) {
    table.on('change.dt', 'tr td input:checkbox', function() {
      setTimeout(function () {
         Shiny.onInputChange('rows', $(this).add('tr td input:checkbox:checked').parent().siblings(':last-child').map(function() {
                 return $(this).text();
              }).get())
         }, 10); 
    });
}")
  
  )
  )
}

shinyApp(ui, server)
