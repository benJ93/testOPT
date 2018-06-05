library(shiny)
if (interactive()) {
  
  
  # demoing optgroup support in the `choices` arg
  shinyApp (
    ui = fluidPage(
      column(3,
             selectInput("CC1", "groupe CC1(260-360):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("CC2", "groupe CC2(220-420):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("CC3", "groupe CC3(220-420):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("CC4", "groupe CC4(220-420):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TV1", "groupe TV1(70-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TV2", "groupe TV2(70-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TG1", "groupe TG1(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG2", "groupe TG2(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG3", "groupe TG3(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG4", "groupe TG4(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG5", "groupe TG5(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG6", "groupe TG6(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG7", "groupe TG7(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),
             
             selectInput("TG8", "groupe TG8(40-120):",
                         list(
                           `Midwest` = c(0, 1))
             ),

             
             selectInput("TG9", "groupe TG9(40-120):",
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
        
        CC4 <- paste(input$CC4, collapse = ", ")
        
        TV1 <- paste(input$TV1, collapse = ", ")
        
        TV2 <- paste(input$TV2, collapse = ", ")
        
        TG1 <- paste(input$TG1, collapse = ", ")
        
        TG2 <- paste(input$TG2, collapse = ", ")
        
        TG3 <- paste(input$TG3, collapse = ", ")
        
        TG4 <- paste(input$TG4, collapse = ", ")
        
        TG5 <- paste(input$TG5, collapse = ", ")
        
        TG6 <- paste(input$TG6, collapse = ", ")
        
        TG7 <- paste(input$TG7, collapse = ", ")
        
        TG8 <- paste(input$TG8, collapse = ", ")
        
        TG9 <- paste(input$TG9, collapse = ", ")
        
        num <- paste(input$num, collapse = ", ")
        
        demande <- as.numeric(num)
        
        RESU <-c(CC1, CC2, CC3, CC4, TV1, TV2, TG1, TG2, TG3, TG4, TG5, TG6, TG7, TG8, TG9)
        DD <- as.numeric(RESU)
        

        
        library ('nloptr')
        vectA <- c(0.0015, 0.0051, 0.0243, 0.0047, 0.0018, 0.0376, 0.049, 0.414, 0.015, 0.0147, 0.0167, 0.1192, 0.016, 0.0456, 0.0492)
        vectB <- c(1.4272, 1.2308, 0.0673,	1.1093, 2.283, 1.514, 1.6207, 1.6847, 1.9057,	1.9218,	2.0345,	0.5623,	2.126,	1.5586,	1.5167)
        vectC <- c(16.07, 9.2567, 24.633, 13.785, 3.3269, 6.8341, 7.8635,	8.2481,	8.9157,	9.009,	7.39567,	12.083,	6.588,	8.2122,	9.0123)
        
        
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
        x0 <- c( 260, 220, 220, 220, 70, 70, 40, 40, 40, 40, 40, 40, 40, 40, 40 )*DD
        
        # lower and upper bounds of control
        
        lb <- c( 260, 220, 220, 220, 70, 70, 40, 40, 40, 40, 40, 40, 40, 40, 40 )*DD
        ub <- c( 360, 420, 420, 420, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120 )*DD
        
        
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