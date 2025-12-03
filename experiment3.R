# experiment 3 - one step euler's method
f <- function(x,y){
  return(y - (x^2))
}

euler <- function( f,x0, y0 , h, n){
  df <- NULL
  xn <- x0
  yn <- y0
  
  for(i in 0:(n-1)){
    fn <- f(xn, yn)
    
    df <- rbind(df, data.frame(n=i, Xn = xn, Yn = yn, Fn = fn, hFn = h*fn))
    xn = xn + h
    yn = yn + h*fn
  }
  return(df)
}
print("final approximation table is : ")
euler(f, 0, 0.5, 0.05, 3)
