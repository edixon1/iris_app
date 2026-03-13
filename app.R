library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Iris Sepal Length"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  showModal(ui = modalDialog(
    fileInput("conn_encrypted", "Connection File", accept = ".rds")
  ))
  
  # Connection
  con <- reactiveVal(NULL)
  
  
  iris_data <- reactiveVal(NULL)
  observeEvent(con(), {
    res <- DBI::dbSendQuery(con(), "SELECT * FROM iris;")
    
    iris_data(DBI::dbFetch(res))
    
  })
  
  
  # Observe upload of connection file
  observeEvent(input$conn_encrypted, {
    
    raw <- readRDS(input$conn_encrypted$datapath)
    key <- Sys.getenv("key") |>
      charToRaw() |> 
      sodium::hash()
    
    print(key)
    
    con_df <- sodium::data_decrypt(raw, key) |>
      unserialize()
    
    print(con_df)
    
   # tryCatch({
      con(DBI::dbConnect(drv = RPostgres::Postgres(),
                    dbname = con_df$dbname,
                    host = con_df$host,
                    port = con_df$port,
                    password = con_df$password,
                    user = con_df$user))
      
      
      removeModal()
   # },
   # error = function(e){
      
  #    showNotification("Unable to resolve host, ensure connection file is valid.  Otherwise, contact Eliot.")
  #    shinyjs::reset("pw_upload_fi")
 #   })
    
    
    
  })
  
  
  
  
  
  output$distPlot <- renderPlot({
    
    validate(need(!is.null(iris_data()), ""))
    # generate bins based on input$bins from ui.R
    x    <- iris_data()$sepal_length
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Sepal Length',
         main = 'Histogram of Sepal Lengths')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
