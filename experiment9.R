#difference eq

f <- function(n,initial){
  x<-NULL
  x[1]<- initial[1]
  x[2]<-initial[2]
  for(i in 3:n){
    x[i]=5*x[i-1] - 6*x[i-2]
  }
  return(x)
}

res<-f(10, c(1,2))
print(res)
plot(0:9, res, type="o", col="blue", pch=19, lwd=2, xlab="x", ylab="y")

#question 
f<-function(a,b,p,q,n,initial){
  x<-NULL
  y<-NULL
  x[1]<-initial[1]
  y[1]<-initial[2]
  for(i in 1:(n-1)){
    x[i+1] <- a*x[i] + b*y[i]
    y[i+1] <- p*x[i] + q*y[i]
  }
  return(list(X=x, Y=y))
}
n<-15
res<- f(2,1,1,3,15,c(1,2))
print(res)
plot(0:(n-1), res$X, type="s", lty=1, col="red", xlab="n", ylab="value", pch=19 )
lines(0:(n-1), res$Y, col="blue", pch=19)
