library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("déterminer la production de chaque générateur"),
  
  fluidRow(
    
    
    column(3, 
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox group"), 
                              choices = list("CC1 (210-420)" = 1, 
                                             "CC2 (210-420)" = 2, 
                                             "CC3 (210-420)" = 3,
                                             "TV1 (70-140)" = 4,
                                             "TV2 (70-140)" = 5,
                                             "TG1 (50-120)" = 6,
                                             "TG2 (50-120)" = 7),
                              selected = 1)),
    
    
    column(3, 
           numericInput("num", h3("entrer la demande de puissance en MW: "), 
                        value = 1000)),
    
    column(3,
           h3("lancer le calcul"),
           actionButton("action", "Action"),
           br(),
           br())
  ),
    
    fluidRow(
      
      column(3,
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
    
    
    
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)