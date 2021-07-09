
library(haven)


#
# Duracion de la atencion
#

salud.pacientes = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")

beta.hat = mean(salud.pacientes$C1P14,na.rm = T)
beta.hat
error.st = sqrt(beta.hat^2/dim(salud.pacientes)[1])
error.st

hist(salud.pacientes$C1P14,probability = T,
     xlab="Duración de atención (minutos)",
     ylab="Densidad",main="")
x = seq(0,max(salud.pacientes$C1P14,na.rm = T),0.001)
lines(x,dexp(x,rate=1/beta.hat))

#
# Gamma
#

bar.x     <- mean(salud.pacientes$C1P14,na.rm = T)
bar.x2    <- mean(salud.pacientes$C1P14^2,na.rm = T)
alpha.hat <- bar.x^2*(bar.x2 - bar.x^2)^(-1)
alpha.hat
beta.hat  <- (bar.x2 - bar.x^2)*bar.x^(-1)
beta.hat

hist(salud.pacientes$C1P14,probability = T,
     xlab="Duración de atención (minutos)",
     ylab="Densidad",main="")
x = seq(0,max(salud.pacientes$C1P14,na.rm = T),0.001)
lines(x,dgamma(x,shape = alpha.hat,scale=beta.hat))

salud.pacientes <- salud.pacientes[!is.na(salud.pacientes$C1P14),]
lik <- function(x) -sum(dgamma(salud.pacientes$C1P14,shape=x[1],
                              scale=x[2],log=T))
optim(c(1,1),lik)

# Comparacion de modelos
salud.pacientes <- salud.pacientes[!is.na(salud.pacientes$C1P14),]

lik.exp     <- function(x) -sum(dexp(salud.pacientes$C1P14,rate=1/x,
                                       log=T))
lik.gamma   <- function(x) -sum(dgamma(salud.pacientes$C1P14,shape=x[1],
                               scale=x[2],log=T))
lik.weibull <- function(x) -sum(dweibull(salud.pacientes$C1P14,shape=x[1],
                                     scale=x[2],log=T))

optim(par=1,fn=lik.exp)
optim(par=c(1,1),fn=lik.gamma)
optim(par=c(1,1),fn=lik.weibull)


model.exp     <- optim(par=1,fn=lik.exp)
model.gamma   <- optim(par=c(1,1),fn=lik.gamma)
model.weibull <- optim(par=c(1,1),fn=lik.weibull)

hist(salud.pacientes$C1P14,probability = T,
     xlab="Duración de atención (minutos)",
     ylab="Densidad",main="")
x = seq(0,max(salud.pacientes$C1P14,na.rm = T),0.001)
lines(x,dexp(x,rate=1/model.exp$par))
lines(x,dgamma(x,shape = model.gamma$par[1],
               scale = model.gamma$par[2]),
               lty=2)
lines(x,dweibull(x,shape = model.weibull$par[1],
               scale = model.weibull$par[2]),
               lty=3)
legend("topright",legend=c("Exponencial","Gamma","Weibull"),
       lty=1:3,bty="n",title = "Modelo")



