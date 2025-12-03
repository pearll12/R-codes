# experiment 5- random no

#1
rnorm(20, mean=50, sd=10)

#2
sample(1:4, 10, replace= T, prob =c(0.1, 0.2, 0.3, 0.4))

#3
col1 <- sample(1:10, 10, replace=T)
col2 <- rnorm(10)
col3 <- sample(0:1,10, replace=T)
df <- data.frame(INTEGERS = col1, NORMAL = col2 , BINARY = col3)
print(df)

lcg <- function(x0, m, a, c,n){
  xn <- NULL
  xn[1] <- x0
  for(i in 1:(n-1)){
    xn[i+1] <- (a * xn[i] + c)%%m
  }
  u = xn/m
  
  return(list(X=xn , U=u))
}
lcg(5, 23, 6,7, 20)
