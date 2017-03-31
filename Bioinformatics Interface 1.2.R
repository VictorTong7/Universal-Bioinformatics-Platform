library(shiny)

Selected <- mtcars
Selected$category <- rep("Not assigned", nrow(Selected))

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
                      tableOutput("SelectedRows")
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
  output$page3.1 <- renderUI(
    fluidRow(
      column(9,
            renderTable(mtcars[seq(1, input$Key, 1),])
      ),
      column(3,
            checkboxGroupInput("checkgroup", "Checked options are included", choices = seq(1, input$Key, 1), seq(3,input$Key, 3))
      )
    )
  )
  output$SelectedRows <- renderTable(mtcars[as.numeric(input$checkgroup),])
}

shinyApp(ui, server)