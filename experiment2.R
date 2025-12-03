# experiment 2

## SIMPSON'S 1/3rd for even
f <- function(a){
  return(a*sin(a))
}
simpson <- function(f,a,b,n){
  if(is.function(f)== FALSE){
    stop("f only allows one parameter")
  }
  h <- (b-a)/n
  even <- 0
  odd <- 0
  for(i in 1:(n-1)){
    xi <- a + i*h
    if(i %% 2 == 0){
      even <- even + f(xi)
    }
    else{
      odd <- odd + f(xi)
    }
  }
  ans <- h/3 *(f(a) + f(b) + 4*(odd) + 2*(even))
  return(ans)
}
simpson(f, 1 ,2 ,10)

## SIMPSON'S 3/8th for multiples of 3
f <- function(a){
  return(a*sin(a))
}
simpson <- function(f,a,b,n){
  if(is.function(f)== FALSE){
    stop("f only allows one parameter")
  }
  h <- (b-a)/n
  s <- f(a) + f(b)
  for(i in 1:(n-1)){
    xi <- a + i*h
    if(i %% 3 == 0){
      s <- s + 2*f(xi)
    }
    else{
      s <- s + 3*f(xi)
    }
  }
  ans <- (3*h/8) * s
  return(ans)
}
simpson(f, 1 ,2 ,10)

