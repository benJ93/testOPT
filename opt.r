library ('nloptr')
library('readxl')
vectA <- my_data[1,]
vectB <- my_data[2,]
vectC <- my_data[3,]
Marche <- scan(nmax=length(vectA))
demande<- scan(nmax=1)

if(demande > sum(ub)){
  print("La demande est supérieur a la capacité de production on deficitaire de: " )
  print(demande-sum(ub))
  }
  
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
x0 <- my_data[4,]*Marche

# lower and upper bounds of control

lb <- my_data[4,]*Marche
ub <- my_data[5,]*Marche


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