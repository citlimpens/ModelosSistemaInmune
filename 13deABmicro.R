#deAngelis-Beddington micropar??sitos
library(deSolve)
library(pracma)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    fp = alpha*p
    gpi =  ((beta*p*i)/(1+(beta*h*p)+(s*i)))
    dp = fp-gpi
    di = -i*mu + m*gpi
    return(list(c(dp,di)))
  })
}

Pars <- c(alpha = .6, beta = 2, mu = 0.8, m = 1.2, K=10, h=.2, s=.2)
State <- c(p = 1, i = 1)
Time <- seq(0,100, by =1)


out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "deAngelis-Beddington", box.lwd =0)
