#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RPostgreSQL)
source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Homology-based, Representative PDB Sampling Tool Version 1.0"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons("options", "Sampling Options", 
                      choices = c("Specify Pfam accession ids",
                                  "Enter number of Pfam accession ids") ,
                      selected = ""),
         
         uiOutput("choice"),
         #numericInput("pfam_num", "Enter Number of Desired Pfam Families", value = 10, min = 1, max = 17929),
         
         numericInput("struc_num", "Enter Number of Desired Protein Structures", value = 10, min = 1, max = 25000),
         
         checkboxGroupInput("all", "Download all structurs for these Pfam accession numbers?", choices = c("Yes")),
         
         uiOutput("button")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #tags$h3("waiting to be built")
        tabsetPanel(
          tabPanel(
            "data output",
            tags$hr(),
            uiOutput("downloadData"),
            tags$hr(),
            div(style='height:400px; width:600px; overflow-y: scroll; overflow: scroll',
                tableOutput("mainTable"))
            
          ),
          tabPanel(
            "meta data",
            tags$hr(),
            uiOutput("downloadMData"),
            tags$hr(),
            div(style='height:400px; width:600px; overflow-y: scroll; overflow: scroll',
                tableOutput("metaTable"))
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$choice <- renderUI({
    req(input$options)
    if(input$options == "Specify Pfam accession ids"){
      textInput("pfam_input", "Enter Desired Pfam accession ids separated by whitespace", value = "")
    }
    else{
      numericInput("pfam_num", "Enter Number of Desired Pfam Families", value = 10, min = 1, max = 17929)
    }
  })
  
  output$button <- renderUI({
    req(input$options)
    actionButton("go", "Generate Data")
  })
  
  # # give the main download button a function
  # output$data <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.csv', sep='')
  #     },
  #   content = function(con) {
  #     write.csv(mtcars, con)
  #     }
  #   )
  
  observeEvent(input$go, {
    drv <- dbDriver("PostgreSQL")
    
    conn <- dbConnect(drv,
                      dbname="can",
                      host="pyrva.cul6zd3pcwrp.us-east-2.rds.amazonaws.com",
                      user="python",
                      password="pythonuser")
    
    if(input$options == "Specify Pfam accession ids"){
      if(input$pfam_input != ""){
        query <- return_spec_pfam(input$pfam_input)
        pdb_query <- return_pdb_ids(input$pfam_input)
        rs <- dbSendQuery(conn, query)
        data <- fetch(rs,n=-1)
        print(head(data))
        
        split <- strsplit(input$pfam_input,"\\s+")
        full_split <- unlist(split)
        num_structs_per_pfam <- input$struc_num / length(full_split)
      }
    }
    else{
      req(input$pfam_num)
      rs <- dbSendQuery(conn, 
                        paste0("WITH unique_ids as (SELECT DISTINCT pfam_id FROM all_opt), ",
                               "ran_ids as (SELECT pfam_id FROM unique_ids ORDER BY RANDOM() LIMIT ",
                               as.character(input$pfam_num),
                               ") ",
                               "select all_opt.pfam_id, all_opt.uniprot_id, all_opt.pdb_id, ",
                               "pfam_ratio.pdb_exists, pfam_ratio.total_seqs, pfam_ratio.ratio ",
                               "from all_opt ",
                               "inner join ran_ids on ran_ids.pfam_id = all_opt.pfam_id ",
                               "inner join pfam_ratio on pfam_ratio.pfam_id = all_opt.pfam_id"
                               )
                        )
      data <- fetch(rs,n=-1)
  
      # calculate the right number of structures per pfam
      num_structs_per_pfam <- input$struc_num / input$pfam_num
    }
    # submit a statement
    # rs <- dbSendQuery(conn, "SELECT * FROM pfam_ratio")
    
    # create output dataframe
    outputData <- data.frame(pfam_id=character(),
                         pdb_exists=integer(),
                         total_seqs=integer(),
                         ratio=integer(),
                         uniprot_id=character(),
                         pdb_id=character())
    
    pfams <- unique(data$pfam_id)
    if(length(input$all) == 0){
      for(value in pfams){
        filtered_data <- data[data$pfam_id == value,]
        
        if(nrow(filtered_data) > num_structs_per_pfam){
          add_in <- filtered_data[sample(nrow(filtered_data),num_structs_per_pfam), ]
        }
        else{
          add_in <- filtered_data
        }
        outputData <- rbind(outputData,add_in)
      }
    }
    else{
      for(value in pfams){
        filtered_data <- data[data$pfam_id == value,]
        
        outputData <- rbind(outputData,filtered_data)
      }
    }
    
    metaData <- outputData
    
    outputData[2] <- NULL
    outputData[3] <- NULL
    outputData[3] <- NULL
    outputData[3] <- NULL
    
    metaData[2] <- NULL
    metaData[2] <- NULL
    
    metaData <- unique(metaData)
    
    tableOutput <- outputData
    
    metaData$pdb_exists <- as.integer(metaData$pdb_exists)
    metaData$total_seqs <- as.integer(metaData$total_seqs)
    
    #print out metadata
    output$metaTable <- renderTable(metaData, digits=7)
    output$downloadMData <- renderUI({
      downloadButton("downloadMain2", "Download meta data")
    })
    output$downloadMain2 <- downloadHandler(
      filename = "metadata.csv",
      content = function(file) {
        write.table(metaData, file, row.names = FALSE, sep=",")
      }
    )
    
    #print(head(data))
    output$mainTable <- renderTable({
      tableOutput
    })
    output$downloadData <- renderUI({
      downloadButton("downloadMain", "Download PDB ids")
    })
    
    outputData[1] <- NULL
    
    output$downloadMain <- downloadHandler(
      filename = "pdb_ids.csv",
      content = function(file) {
        write.table(outputData, file, row.names = FALSE, col.names = FALSE, sep=",")
      }
    )
    
    dbDisconnect(conn)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

