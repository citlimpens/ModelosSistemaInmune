#Lotka-Volterra microparasitos con hematopoyesis
library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dp = p*(alpha-beta*i)
    di = o -i*(mu - m*beta*p)
    return(list(c(dp,di)))
  })
}

Pars <- c(alpha = .48, beta = 0.018, mu = 0.8, m = 1.2, o=2)
State <- c(p = .5, i = .5)
Time <- seq(0,40, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Lotka-Volterra", box.lwd =0)

write.csv(out,file="lvout.csv")
