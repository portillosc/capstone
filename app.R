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
   titlePanel("Homology-based, Representative PDB Sampling Tool"),
   
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
         
         uiOutput("button")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #tags$h3("waiting to be built")
        tabsetPanel(
          tabPanel(
            "data output",
            tableOutput("mainTable"),
            uiOutput("downloadData")
          ),
          tabPanel(
            "meta data",
            tableOutput("metaTable"),
            uiOutput("downloadMData")
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
        rs <- dbSendQuery(conn,
                          pdb_query)
        data <- fetch(rs,n=-1)
        #drop columns here
        split <- strsplit(input$pfam_input,"\\s+")
        full_split <- unlist(split)
        num_structs_per_pfam <- input$struc_num / length(full_split)
      }
    }
    else{
      req(input$pfam_num)
      rs <- dbSendQuery(conn, 
                        paste0("WITH temp_data as (SELECT DISTINCT pfam_id FROM pfam_ratio WHERE pdb_exists != 0), ",
                               "ids AS (select pfam_id from temp_data ORDER BY RANDOM() LIMIT ",
                               as.character(input$pfam_num),
                               "), ",
                               "pfam_uni_trunc AS (select * from ids ",
                               "INNER JOIN pfam_ratio on pfam_ratio.pfam_id = ids.pfam_id ",
                               "INNER JOIN pfam_uni on pfam_ratio.pfam_id = pfam_uni.pfam_id ) ",
                               "select * from pfam_uni_trunc ",
                               "INNER JOIN pdb_uni on pfam_uni_trunc.uniprot_id = pdb_uni.uniprot_id"
                               )
                        )
      data <- fetch(rs,n=-1)
      data[1] <- NULL
      data[5] <- NULL
      data[5] <- NULL
      data[6] <- NULL
      data[7] <- NULL
      
      
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
    
    metaData <- outputData
    
    outputData[2] <- NULL
    outputData[2] <- NULL
    outputData[2] <- NULL
    outputData[2] <- NULL
    
    metaData[5] <- NULL
    metaData[5] <- NULL
    
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
      filename = "pdb_ids.csv",
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

