library ('nloptr')
vectA <- c(0.0012, 0.0114, 0.044, 0.0162, 0.0021, 0.0419, 0.0417)
vectB <- c(1.4423,0.607,0.7037,1.7106,1.7013,2.2095,1.7013)
vectC <- c(15.86, 16.098, 3.0391, 6.0502, 5.3922, 8.3339, 8.2559)
Marche <- c(0, 1, 1, 1, 1, 0, 1)
demande<- 1500

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
print( res )
total <- sum (res[["solution"]])
print(total )
total == demande