# experiment 4 - runge katta method

# 4th ORDER RUNGE KATTA METHOD
f <- function(x,y){
  return((x*y)/(1 + x^2))
}

rungekatta <- function(f, xn, yn, h, n){
  df<- NULL
  k<- f(xn,yn)
  for(i in 0:(n-1)){
    fn <- f(xn, yn)
    df <- rbind(df, data.frame(N=i, Xn = xn, Yn = yn , Fn = fn, K=k)) 
    k1 <- h * f(xn, yn)
    k2 <- h * f(xn + h/2 , yn + (k1/2))
    k3 <- h * f(xn + h/2, yn + (k2/2))
    k4 <- h * f(xn + h, yn + k3)
    k <- 1/6 *(k1 + k4 + 2*(k2 + k3))
    xn <- xn + h
    yn <- yn + k
  }
  return(df)
}

rungekatta(f, 1,2,0.1,2)

# 2nd ORDER RUNGE KATTA METHOD
f1 <- function(x1,y1){
  return(sin(x1) + y1^2)
}
rungekatta2 <- function(f1, xn, yn, h, n){
  df <- NULL
  k <- f1(xn, yn)
  for(i in 0:(n-1)){
    fn <- f1(xn,yn)
    df <- rbind(df, data.frame(N=i, Xn = xn, Yn = yn, Fn = fn, K = k ))
    k1 <- h * f1(xn, yn)
    k2 <- h * f1( xn + h, yn+ k1)
    k <- (k1 + k2)/2
    xn <- xn + h
    yn <- yn + k
  }
  return(df)
}
rungekatta2(f1, 0, 1,0.1, 3)
