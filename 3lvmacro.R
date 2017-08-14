#Lotka-Volterra macroparasitos
library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dp = gamma-beta*i*p
    di = -i*(mu - m*beta*p)
    return(list(c(dp,di)))
  })
}

Pars <- c(gamma = 4, beta = .18, mu = .8, m = .2)
State <- c(p = .5, i = .5)
Time <- seq(0,40, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Lotka-Volterra", box.lwd =0)

write.csv(out,file="lvout.csv")
