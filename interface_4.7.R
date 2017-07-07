########################################
#$#$#$#$#$#$    Shiny App  $#$#$#$#$#$#$
########################################
library(stringr)

all_gpl570<-read.csv('all_gpl570.csv')
dia_gsm2 <- all_gpl570[,c(2,3,8)]

# fill each row with 'not assigned' label, until they are placed in groups
dia_gsm2$category <- rep("Not yet assigned", nrow(dia_gsm2))
dia_gsm2 -> gsm_selected


ui <- fluidPage(
  #creation of a navigation bar and mulitple pages
  navbarPage("Bioinformatics Software",
             tabPanel("Search for GEO data series (GSE)",  
                      #setting user inputed values and displaying values back at the user
                      textInput("Key", "Enter search terms, separated by commas", value = ""),
                      verbatimTextOutput("Key"),
                      actionButton("Search", "Search"), 
                      verbatimTextOutput("Commas"), 
                      verbatimTextOutput("Ncommas"), 
                      verbatimTextOutput("Searchterms2")
             ),
             tabPanel("Define categories for GEO samples (GSM)", uiOutput("page2"), 
                      textInput("cat1", "Define Category 1"),
                      #verbatimTextOutput("cat1"),
                      textInput("cat2", "Define Category 2"),
                      #verbatimTextOutput("cat2"),
                      textInput("cat3", "Define Category 3")
                      #verbatimTextOutput("cat3")
             ),
            
             # changed the format of this slightly to accomodate the DT package (for row selection)
             tabPanel("Assign samples to categories", uiOutput("page3"), 
                      actionButton("Lock", "Assign Categories"),
                      DT::dataTableOutput("gsm_table")                            
                      #DT::dataTableOutput("filteredgsm")      
 
             )
  )
)

server <- function(input, output) {
  
    Totalchar <- eventReactive(input$Search, {nchar(input$Key)})
    
    Commas <- eventReactive(input$Search, {which(strsplit(input$Key, "")[[1]]==",")})
    
    Ncommas <- eventReactive(input$Search, {length(Commas())})
    
    Commasstart <- eventReactive(input$Search, {Commas() + 1})
    
    Commasend <- eventReactive(input$Search, {Commas() - 1})
    
    Searchterms <- eventReactive(input$Search, {
      substring(input$Key, c(1, Commasstart()), c(Commasend(), Totalchar()))
    })
    
    # Searchterms2 <- eventReactive(input$Search, {
    #   paste0("//b", Searchterms(), "//b")
    # })
    # 
    # Searchterms3 <- eventReactive(input$Search, {
    #   gsub(",//b", "//b|", Searchterms2())
    # })
    
    #This currently doesn't actually do anything from what I see
    # Searchterms4 <- eventReactive(input$Search, {
    #   gsub(" ", "", Searchterms3())
    # })
    
    filteredgsm <- eventReactive(input$Search, {filter(gsm_selected, str_detect(gsm_selected$title, Searchterms()))})
    
    #import dataframe as reactive
    #for some reason this line is not needed
    rows <- reactiveValues() 
    observeEvent(input$Lock, {
    #s <- eventReactive(input$Lock, input$gsm_table_rows_selected)
        if (input$Lock == 1) {gsm_selected <- filteredgsm()
          gsm_selected[input$gsm_table_rows_selected,"category"] <- input$selection
          rows$df <- gsm_selected
          gsm_selected <<- rows$df # '<<-' is necessary to get this to the enclosing environment
        }
        else
        {
          gsm_selected[input$gsm_table_rows_selected,"category"] <- input$selection
          rows$df <- gsm_selected
          gsm_selected <<- rows$df
        }
    })
    
  output$Key <- renderText(unlist(strsplit(input$Key,",")))
  output$Commas <- renderText(Commas())
  output$Ncommas <- renderText(Ncommas())
  output$Searchterms2 <- renderText(Searchterms())
#  output$cat1 <- renderText(input$cat1)
#  output$cat2 <- renderText(input$cat2)
#  output$cat3 <- renderText(input$cat3)
#  output$Button <- renderText(input$Lock)
#  output$Rows <- renderText(s())
#  output$Currentcategory <- renderText(currentcategory)
  
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
    
  
  output$gsm_table <- DT::renderDataTable({
      if (input$Lock == 0)
          return(filteredgsm())
      else
          return (rows$df)}, options=list(searching=FALSE))

}

shinyApp(ui, server)
