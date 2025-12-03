# experiment 6

f <- function(x){
  x^2
}
curve(f, from=-3, to=3 , col="red")
no <- 10000
x<- runif(no, min=-3, max=3)
res<- 6 * 1/no * sum(f(x))
print(res)
estimate <- NULL
for(i in 1:no){
  y<- runif(i, min=-3, max=3)
  estimate[i] <- 6 * (1/i) *(b-a)*sum(f(y))
}
plot(estimate)
abline(h=18, lwd=3, col="yellow")

# BY INTEGRATION
f2 <- function(x){
  x^3 * sin(x)
}
rand <- 10000
xrand <- runif(rand, min=0, max=1)
approx <- 1/rand * sum(f2(xrand))
print(approx)
est <- NULL
for(i in 1:rand){
  z <- runif(i, min=0, max=1)
  est[i] <- 1/i* sum(f2(z))
}
plot(est)
abline(h=approx, lwd=3, col="yellow")

#BY ESTIMATION
dots<-0
fk <- function(x){
  x^3 * sin(x)
}
rand <- 1000
x2<-runif(rand, min=0, max=1)
y2<-runif(rand, min=0, max=0.5)

for(i in 1:rand){
  plot(x2[1:i], y2[1:i], xlim=c(0,1), ylim = c(0,0.5) )
  if(y2[i]<=fk(x2[i])){
    points(x2[i], y2[i], col="blue", pch=19)
    dots= dots+1
  }
  else{
    points(x2[i], y2[i], col="red", pch=19)
  }
  Sys.sleep(0.05)
}
