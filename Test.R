library(shiny)
ui <- fluidPage(
  
  textInput("keywords", "Enter keywords"),
  verbatimTextOutput("keywords"),
  
  textInput("category_1", "Set Category 1"),
  verbatimTextOutput("category1"),
  
  textInput("category_2", "Set Category 2"),
  verbatimTextOutput("category2"),
  
  textInput("category_3", "Set Category 3"),
  verbatimTextOutput("category3"),
  
  selectInput("variable","Variable:",
              c("category1" = "cyl",
                "category2" = "am",
                "category3" = "gear",
                "Not included" = "wt")),
  
  dataTableOutput("data"),
  tableOutput("results")
  
  
)
server <- function(input, output) {
  output$keywords <- renderText({input$keywords})
  output$category1 <- renderText({input$category_1})
  output$category2 <- renderText({input$category_2})
  output$category3 <- renderText({input$category_3})
  
  output$data <- renderDataTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  })
  
  output$results <- renderTable({
    mtcars[, c("mpg", input$keywords), drop = TRUE]
  }, rownames = TRUE)
}

shinyApp(ui = ui, server = server)
