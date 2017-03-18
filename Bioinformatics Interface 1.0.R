library(shiny)

ui <- fluidPage(
  #creation of a navigation bar and mulitple pages
  navbarPage("Bioinformatics Software",
             tabPanel("First Page",  
                      #setting user inputed values and displaying values back at the user
                      textInput("Key", "Enter Keywords"),
                      verbatimTextOutput("Key"),
                      textInput("cat1", "Enter Category 1"),
                      verbatimTextOutput("cat1"),
                      textInput("cat2", "Enter Category 2"),
                      verbatimTextOutput("cat2"),
                      textInput("cat3", "Enter Category 3"),
                      verbatimTextOutput("cat3")
             ),
             tabPanel("Second Page", uiOutput("page2"), 
                      dataTableOutput("Results")),
             tabPanel("Third Page", uiOutput("page3")),
             tabPanel("Fourth Page", uiOutput("page4"))
  )
)

server <- function(input, output) {
  output$Key <- renderText(input$Key)
  output$cat1 <- renderText(input$cat1)
  output$cat2 <- renderText(input$cat2)
  output$cat3 <- renderText(input$cat3)
  output$page2 <- renderUI(
    selectInput("selection", "Selection",
                c("category1" <- {input$cat1},
                  "category2" <- {input$cat2},
                  "category3" <- {input$cat3},
                  "category4" <- "Not included"))
  )
  output$Results <- renderDataTable({
    mtcars[, c("mpg", input$Key), drop = TRUE]
  })
}

shinyApp(ui = ui, server = server)