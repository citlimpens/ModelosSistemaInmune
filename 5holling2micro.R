#Holling ii microparasitos
library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    fp = alpha*p
    gpi = beta*p*i/(1+beta*h*p)
    dp = fp-gpi
    di = -i*mu + m*gpi
    return(list(c(dp,di)))
  })
}

Pars <- c(alpha = .6, beta = 2, mu = 0.8, m = 1.2, K=10, h=.2)
State <- c(p = 1, i = 1)
Time <- seq(0,100, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Holling II", box.lwd =0)

write.csv(out,file="h2out.csv")
