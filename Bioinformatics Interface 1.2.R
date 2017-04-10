library(shiny)
# need a few more now...
# if you need to install these to run on your local machine uncomment the following line:
#install.packages(c('dplyr','tidyr','ggplot2','RColorBrewer','readr','stringr','shiny','shinythemes','shinyjs','DT'))
library(shinythemes)
library(shinyjs)
library(DT)

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
all_gpl570<-read.csv('all_gpl570.csv')

# search strings –– next version we take these from the UI
search_string_motorNeurons = "\\bbrain\\b|\\bneurons?(al)?\\b&\\bmotor\\b"
search_string_diabetes = "\\bpancreas\\b|\\bislets?\\b|\\bbeta-cells\\b|\\bdiabetes\\b"
search_string_hepatocytes = "\\bliver\\b&\\bhepatocytes?\\b"
search_string_cardiomyocytes = "\\bheart\\b&\\bcardiomyocytes?\\b"

# cut back the columns from the GSE info to make it more readable
selected <- all_gpl570[,c(2,3,8)]

# fill each row with 'not assigned' label, until they are placed in groups
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
            # changed the format of this slightly to accomodate the DT package (for row selection)
             tabPanel("Third Page", uiOutput("page3"), DT::dataTableOutput("page3table")
                      ),
             # currently, page 4 displays the selected rows from page 3
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
  
  # render the table with the specified number of rows (will be keywords) without the option to further search (although we may want this)
  output$page3table <- DT::renderDataTable(selected[1:input$Key,], options=list(searching=FALSE))
  
  # depending on the rows selected on page 3, make a new table
  output$SelectedRows <- renderDataTable({
    s = input$page3table_rows_selected
    selected[s,]})
}

shinyApp(ui, server)
