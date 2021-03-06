# 2017-06-19

########################################
#$#$#$#$#$#$    HEADER     $#$#$#$#$#$#$
########################################
# Bioinformatics packages installed via biocLite:
#source("https://bioconductor.org/biocLite.R")
#biocLite(c('limma','annotate','genefilter','ComplexHeatmap','pheatmap','cowplot','GEOmetadb','mouse4302.db','hgu133plus2.db'))

library(limma)
library(annotate)
library(genefilter)
library(ComplexHeatmap)
library(pheatmap)
library(cowplot)
library(GEOmetadb)

#biocLite(c('MergeMaid','GEOquery','inSilicoMerging','affy','sva','Rtsne','metaArray','testthat'))
library(MergeMaid)
library(GEOquery)
library(testthat)
library(metaArray)
#library(inSilicoMerging) not available for this version of R
library(Rtsne)
library(sva)
library(affy)

# Microarray platform annotations:
# Equivalent human platform is GPL570 with 127 514 samples
# HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array

#library(mouse4302.db) 
#library(hgu133plus2.db)  Both aren't available

# Other packages:
#install.packages(c('dplyr','dbplyr','tidyr','ggplot2','RColorBrewer','readr','stringr','shiny','shinythemes','shinyjs','DT'))

library(dplyr)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(stringr)
library(DT)
library(shiny)
library(shinythemes)
library(shinyjs)

########################################
#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
########################################

# Trying out getting and analyzing the data from GEO

if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()

db = src_sqlite("GEOmetadb.sqlite")
src_tbls(db)
gse = tbl(db, 'gse')
gse_gpl = tbl(db, 'gse_gpl')
gpl = tbl(db, 'gpl') 
gsm = tbl(db, 'gsm')
gse_gsm = tbl(db, 'gse_gsm')

# Import GPL file (570 or 1260)
# read CSV (all samples using the GPL570 platform)
all_gpl570<-read.csv('all_gpl570.csv')

#dia_gsm <- as.data.frame(diabetes_gsm)

#dia_gsm2 <- dia_gsm[,c(-1,-14:-26,-28:-32)]
dia_gsm2 <- all_gpl570

#get_files = TRUE
get_files = FALSE
if (get_files) {
  # get raw CEL files
  rawFilePaths = lapply(gsm_to_fetch, function(x) {
      getGEOSuppFiles(x) # confirm this is the right function
  })
  
  tar_dirs = list.files(pattern = "GSM")
  tar_files = lapply(tar_dirs, list.files, pattern = ".tar", full.names = TRUE)
  
  # these are downloaded as tar archives so need to extract
  # for the next step they should all be in the same directory, 'all_geo'
  lapply(tar_files, function(x) {
    untar(x, exdir = 'all_geo') 
  })

# New approach: Normalize together ------------------------------------------------------------
all_geo_files = list.files("all_geo", full.names = TRUE)
all_data = ReadAffy(filenames = all_geo_files)

all_eset = rma(all_data)

all_pData = pData(all_eset)

gsm = str_match(rownames(all_pData), "(GSM[[:alnum:]]+)")[,2]

pheno_df = pheno %>% add_rownames("gsm") 

all_pData_df = all_pData %>% mutate(gsm = gsm) %>% 
  left_join(pheno, by = c("gsm" = "geo_accession")) %>% 
  dplyr::select(gsm, Study, tissue)

rownames(all_pData_df) = rownames(all_pData)
pData(all_eset) = all_pData_df

all_eset_final = all_eset[,!is.na(pData(all_eset)$Study)]

pData(all_eset_final) %>% View

identical(colnames(exprs(all_eset_final)), rownames(pData(all_eset_final)))

run_tsne(t(exprs(all_eset_final)), pData(all_eset_final))


# ok that looks good let's save this now

save(all_eset_final, file = "final_processed_data_2017-06-25.rda")

} else {
  load("esets_2017-06-25.rda")
}


### DE

ID = featureNames(all_eset_final)
Symbol = getSYMBOL(ID, "hgu133plus2.db")
fData(all_eset_final) = data.frame(Symbol = Symbol)

eset = all_eset_final

tissue = pData(eset)$tissue
design = model.matrix(~0 + tissue)
colnames(design) = c(UIa,UIb,UIc)


########################################
#$#$#$#$#$#$    Shiny App  $#$#$#$#$#$#$
########################################

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
                      DT::dataTableOutput("gsm_table"), 
                      actionButton("Remove", "Finalize selections and remove not included")
                      #DT::dataTableOutput("filteredgsm")
             ),
             tabPanel("Selection details", uiOutput("page4"), 
                      DT::dataTableOutput("finishedtable")
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
  
  filteredgsm <- eventReactive(input$Search, {dplyr::filter(gsm_selected, str_detect(gsm_selected$title, Searchterms()))})
  
  # List of the GSM associated with the selected GSE
  gsegsm2 <- eventReactive(input$Search, {filter(gse_gsm,gse %in% filteredgsm()$gse)}) # list of series and associated samples
  gsm2 <- eventReactive(input$Search, {filter(gsm,series_id %in% filteredgsm()$gse)}) # detailed sample information
  
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
  
  finishedtable <- eventReactive(input$Remove, {
    dplyr::filter(rows$df, category %in% c(input$cat1, input$cat2, input$cat3))
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
      return (filteredgsm()[,c(2,3,8,21)])
    else ## breaks when there is no search results
      return (rows$df[,c(2,3,8,21)])}, options=list(searching=FALSE))
  
  #remove the highlightability of this table; make it not a datatable and just a table
  output$finishedtable <- DT::renderDataTable({
    if (input$Remove == 0)
      return (filteredgsm()[,c(2,3,8,21)])
    else
      return (finishedtable()[,c(2,3,8,21)])}, options=list(searching=FALSE))
  
}

shinyApp(ui, server)

