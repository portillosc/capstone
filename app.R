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
         tags$h3("waiting to be built")
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
  
  observeEvent(input$go, {
    drv <- dbDriver("PostgreSQL")
    
    conn <- dbConnect(drv,
                      dbname="can",
                      host="pyrva.cul6zd3pcwrp.us-east-2.rds.amazonaws.com",
                      user="python",
                      password="pythonuser")
    
    if(input$options == "Specify Pfam accession ids"){
      query <- return_spec_pfam(input$pfam_input)
      pdb_query <- return_pdb_ids(input$pfam_input)
      print(pdb_query)
      rs <- dbSendQuery(conn,
                        pdb_query)
      data <- fetch(rs,n=-1)
    }
    else{
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
    }
    # submit a statement
    # rs <- dbSendQuery(conn, "SELECT * FROM pfam_ratio")
    print(head(data))
    
    #print(head(data))
    
    dbDisconnect(conn)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

