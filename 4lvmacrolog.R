#Lotka-Volterra macroparasitos log??stico
library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    fp = gamma - (gamma*p^2/K)
    gpi = beta*p*i
    dp = fp-gpi
    di = -i*mu + m*gpi
    return(list(c(dp,di)))
  })
}

Pars <- c(gamma = 2, beta = 2, mu = .5, m = .2, K=10)
State <- c(p = 10, i = 10)
Time <- seq(0,20, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Lotka-Volterra macro log.", box.lwd =0)

write.csv(out,file="lvout.csv")

