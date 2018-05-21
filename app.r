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
           actionButton("runopt", "Action"),
           br(),
           br())
  ),
  
  fluidRow(
    
    column(3,
           verbatimTextOutput("res"),
           
           # Output: HTML table with requested number of observations ----
           tableOutput("view")
           
    )
    
    
    
  )
  
)

# Define server logic ----
server <- function(input, output) {
  output$table <- renderTable({
    Marche <- input$checkGroup
    demande <-input$num
    
  })
  observeEvent(input$runopt, {
  
  library ('nloptr')
  vectA <- c(0.0012, 0.0114, 0.044, 0.0162, 0.0021, 0.0419, 0.0417)
  vectB <- c(1.4423,0.607,0.7037,1.7106,1.7013,2.2095,1.7013)
  vectC <- c(15.86, 16.098, 3.0391, 6.0502, 5.3922, 8.3339, 8.2559)
  
  
  
  eval_f <- function(x){
    objf <- 0.0
    for (i in 1: length(vectA))
    {objf  <-vectA[i]*x[i]*x[i]*Marche[i]+  vectB[i]*x[i]+vectC[i]*Marche[i]}
    
    vector <- c(2*vectA[1]*x[1]+vectB[1])
    for (i in 2: length(vectA))
    {vector<-c(vector,2*vectA[i]*x[i]+vectB[i])}
    
    
    return( list( "objective"=objf,
                  "gradient" = vector))
  }
  eval_g_ineq <- function( x ) {
    constr <- c( 0 - sum(x) )
    grad <- c(1)
    for (i in 2: length(vectA))
    {grad<-c(grad,1)}
    return( list( "constraints"=constr, "jacobian"=grad ) )
  }
  # equalities
  eval_g_eq <- function( x ) {
    constr <- c( sum(x)-demande)
    grad <- c(1)
    for (i in 2: length(vectA))
    {grad<-c(grad,1)}
    
    return( list( "constraints"=constr, "jacobian"=grad ) )
  }
  # initial values
  x0 <- c( 210, 210, 210, 70, 70, 50, 50 )*Marche
  
  # lower and upper bounds of control
  
  lb <- c( 210, 210, 210, 70, 70, 50, 50 )*Marche
  ub <- c( 420, 420, 420, 140, 140, 120, 120 )*Marche
  
  
  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel" = 1.0e-7 )
  opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel" =1.0e-7 ,
                "maxeval" =1570,
                "local_opts" = local_opts )
  
  res <- nloptr( x0=x0,
                 eval_f=eval_f,lb=Marche * lb,
                 ub=Marche * ub,
                 eval_g_ineq=eval_g_ineq,
                 eval_g_eq=eval_g_eq,
                 opts=opts)
  }) 
}

# Run the app ----
shinyApp(ui = ui, server = server)