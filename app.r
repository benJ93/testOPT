library(shiny)
if (interactive()) {
  
  
  # demoing optgroup support in the `choices` arg
  shinyApp (
    ui = fluidPage(
      column(3,
             selectInput("CC1", "groupe CC1:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("CC2", "groupe CC2:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("CC3", "groupe CC3:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TV1", "groupe TV1:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TV2", "groupe TV2:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TG1", "groupe TG1:",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TG2", "groupe TG2:",
                         list(
                           `Midwest` = c(0, 1))
             )

      ),
      column(3, 
             numericInput("num", h3("entrer la demande de puissance en MW: "), 
                          value = 1000)

             
      ),
      column(3,
             titlePanel("la solution:"),
             tableOutput("S")
             

    )
    ),
    
    server = function(input, output, session) {
      output$S <- renderTable({
        CC1 <- paste(input$CC1, collapse = ", ")
        
        CC2 <- paste(input$CC2, collapse = ", ")
        
        CC3 <- paste(input$CC3, collapse = ", ")
        
        TV1 <- paste(input$TV1, collapse = ", ")
        
        TV2 <- paste(input$TV2, collapse = ", ")
        
        TG1 <- paste(input$TG1, collapse = ", ")
        
        TG2 <- paste(input$TG2, collapse = ", ")
        
        num <- paste(input$num, collapse = ", ")
        
        demande <- as.numeric(num)
        
        RESU <-c(CC1, CC2, CC3, TV1, TV2, TG1, TG2)
        DD <- as.numeric(RESU)
        

        
        library ('nloptr')
        vectA <- c(0.0012, 0.0114, 0.044, 0.0162, 0.0021, 0.0419, 0.0417)
        vectB <- c(1.4423,0.607,0.7037,1.7106,1.7013,2.2095,1.7013)
        vectC <- c(15.86, 16.098, 3.0391, 6.0502, 5.3922, 8.3339, 8.2559)
        
        
        eval_f <- function(x){
          objf <- 0.0
          for (i in 1: length(vectA))
          {objf  <-vectA[i]*x[i]*x[i]*DD[i]+  vectB[i]*x[i]+vectC[i]*DD[i]}
          
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
        x0 <- c( 210, 210, 210, 70, 70, 50, 50 )*DD
        
        # lower and upper bounds of control
        
        lb <- c( 210, 210, 210, 70, 70, 50, 50 )*DD
        ub <- c( 420, 420, 420, 140, 140, 120, 120 )*DD
        
        
        local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                            "xtol_rel" = 1.0e-7 )
        opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                      "xtol_rel" =1.0e-7 ,
                      "maxeval" =1570,
                      "local_opts" = local_opts )
        
        res <- nloptr( x0=x0,
                       eval_f=eval_f,lb=DD * lb,
                       ub=DD * ub,
                       eval_g_ineq=eval_g_ineq,
                       eval_g_eq=eval_g_eq,
                       opts=opts)
        print( res )

        total <- sum (res[["solution"]])
        print(total )
        S <- (res[["solution"]])
      })
    }
  )
}