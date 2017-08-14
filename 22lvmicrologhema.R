#Lotka-Volterra microparasitos log??stico hematopoyesis
library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    fp = alpha*p-(alpha*p^2/K)
    gpi = beta*p*i
    dp = fp-gpi
    di = o -i*mu + m*gpi
    return(list(c(dp,di)))
  })
}

Pars <- c(alpha = 2, beta = 1, mu = 0.8, m =1.2, K=10, o =0)
State <- c(p = 10, i = 10)
Time <- seq(0,500, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Lotka-Volterra", box.lwd =0)

write.csv(out,file="lvout.csv")
