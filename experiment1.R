# experiment 1
f <- function(a){
  return(a*a)
}

trapezoidal <- function(f,a,b,n){
  if(is.function(f)==FALSE){
    stop('f is a function with only one parameter')
  }
  h <- (b-a)/n
  j<- 1:(n-1)
  xj = a + j*h
  ans <- h/2 * (f(a) + 2*sum(f(xj)) + f(b))
  return(ans)
}

trapezoidal(f,0,3,6)

