# experiment 7 - growth decay model

f <- function(xo, a, b, n){
  x <- xo
  for(i in 1:n){
    x[i+1] <- x[i]*a + b
  }
  return(x)
}
res = f(150000,1.02, 3000, 10)
plot(0:10, res, pch=19, col="blue", type="b", xlab="yrs", ylab="pop")
print(res)


#8
#predator prey model
f <- function(alpha, beta, delta, gamma, n, x0, y0){
  x<- x0
  y<- y0
  for(i in 1:n){
    x[i+1]<- (1+alpha)*x[i] - beta*y[i]
    y[i+1]<- (1-gamma)*y[i] + delta*x[i]
  }
  return(list(O=x, C=y))
}
t<-50
c0 = c(200,500,800)
res<- f(0.5, 0.02, 0.1,0.1, 50,1000, c0[1])
plot(0:t, res$O, col="red", type='o', pch=2,xlab="time", ylab="pop", main="caterpillar-oak trees")
lines(0:t, res$C, col="blue", type='l', pch=2)

res2<-f(0.5, 0.02, 0.1,0.1, 50,1000, c0[2])
lines(0:t, res2$O, col="green", type='o', pch=2,xlab="time", ylab="pop", main="caterpillar-oak trees")
lines(0:t, res2$C, col="yellow", type='l', pch=2)

res3<-f(0.5, 0.02, 0.1,0.1, 50,1000, c0[3])
lines(0:t, res3$O, col="pink", type='o', pch=2,xlab="time", ylab="pop", main="caterpillar-oak trees")
lines(0:t, res3$C, col="cyan", type='l', pch=2)

legend("topright", legend=c("Oak (C0=200)", "Caterpillar (C0=200)",
                            "Oak (C0=500)", "Caterpillar (C0=500)",
                            "Oak (C0=800)", "Caterpillar (C0=800)"),
       col = c("red","blue","green","yellow","pink","cyan"),
       lty = c(1,1,1,1,1,1),     # all lines are line type 1 (solid)
       pch = c(12, 12, 12, 12, 12, 12),
       lwd = 2)




