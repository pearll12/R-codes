#epidemic model
install.packages("desolve")
library(deSolve)
beta<- 0.002
gamma <- 1/7
Io <-1
So<- 499
t <- seq(0,20,0.1)

params <- c(beta= beta, gamma = gamma)
state <- c(s=So, i = Io, R = 0)

sir<-function(t, state, params){
  with(as.list(c(params, state)), {
    ds = -beta*s*i
    di = beta*s*i - gamma*i
    dr = gamma*i
    list(c(ds,di,dr))
  })
  
}
output<- ode(y=state, times=t, parms = params, func = sir)
out_df = as.data.frame(output)

plot(out_df$t, out_df$s,type="l", col="red",xlab="days", ylab="pop",ylim=c(0,500), lwd=3)
lines(out_df$t, out_df$i, col="blue", lwd=3)
lines(out_df$t, out_df$r, col="brown", lwd=3)