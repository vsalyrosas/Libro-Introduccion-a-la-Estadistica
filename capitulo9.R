#
# Capitulo 9
# Modelos estadisticos

1 - ppois(4,2)
ppois(3,2) - ppois(1,2)
ppois(0:5,2)
qpois(0.75,2)

library(haven)

salud.pacientes = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")

plot(prop.table(table(salud.pacientes$C1P96)),
     type="h",ylim=c(0,0.4),
     xlab="Número de dependientes",
     ylab="Proporción")
points(0:20+0.1,dpois(0:20,2),type="h",
       col=2,lwd=2)
points(0:20+0.2,dpois(0:20,2.5),type="h",
       col=3,lwd=2)
legend(11,0.3,c(2,2.5),title = expression(lambda),
       col=2:3,bty="n",pch=15)


dpois(0,2)
dpois(1,2)
round(prop.table(table(salud.pacientes$C1P96)),3)
mean(salud.pacientes$C1P96)
var(salud.pacientes$C1P96)

#
# Duracion de la atencion
#

hist(salud.pacientes$C1P14,prob=T,
     xlab="Duración de la atención (minutos)",
     ylab="Densidad",main="")
x   = seq(0,80,0.001)
lines(x,dexp(x,rate=1/9),col=1)
lines(x,dexp(x,rate=1/10),col=2)
lines(x,dexp(x,rate=1/11),col=3)
legend(40,0.07,legend=c(9,10,11),
       title=expression(beta),
       bty="n",pch=15,col=1:3)

pexp(9,rate=1/10)
pexp(12,rate=1/10)

pexp(10,rate=1/10) - pexp(5,rate=1/10)
1 - pexp(5,rate=1/10)

Fn = ecdf(salud.pacientes$C1P14)
Fn(10) - Fn(5)
1 - Fn(10)
mean(salud.pacientes$C1P14,na.rm = T)
var(salud.pacientes$C1P14,na.rm = T)


#
# Gamma
#

hist(salud.pacientes$C1P14,prob=T,
     xlab="Duración de la atención",
     ylab="Densidad",main="")
x   = seq(0,80,0.001)
lines(x,dgamma(x,shape=1,scale=10),col=1)
lines(x,dgamma(x,shape=2,scale=5),col=2)
lines(x,dgamma(x,shape=3,scale=3.5),col=3)
legend(40,0.06,c(1,2,3),
       title=expression(alpha),
       bty="n",col=1:3,pch=15)
legend(60,0.06,c(10,5,3.5),
       title=expression(beta),
       bty="n",col=1:3,pch=15)



pgamma(9,shape=3,scale=3.5)
1 - pgamma(12,shape=3,scale=3.5)
qgamma(0.9,shape=3,scale=3.5)

Fn(9)
1 - Fn(12)

# Weibull

hist(salud.pacientes$C1P14,prob=T,
     xlab="Duración de la atención (minutos)",
     ylab="Densidad",main="",
     ylim=c(0,0.15))
x   = seq(0,80,0.001)
lines(x,dweibull(x,shape=2,scale=10),col=1)
lines(x,dweibull(x,shape=2.5,scale=10),col=2)
lines(x,dweibull(x,shape=3.5,scale=10),col=3)
legend(40,0.10,c(2,2.5,3.5),
       title=expression(alpha),
       bty="n",col=1:3,pch=15)
legend(60,0.10,c(10,10,10),
       title=expression(beta),
       bty="n",pch=15,
       col=1:3)

pweibull(9,shape=3,scale=10)
1 - pweibull(12,shape=3,scale=10)
qweibull(0.9,shape=3,scale=10)

#
# Normal
#

x = seq(-10,10,by=0.1)

x1 = par(mfrow=c(1,2))
plot(x,dnorm(x,0,1),type="l",xlab="X",
     ylab="Densidad")
lines(x,dnorm(x,2,1),type="l",lty=2)
lines(x,dnorm(x,-2,1),type="l",lty=3)
legend(5,0.4,legend=c(0,2,-2),
       title = expression(paste(mu," , ",sigma,"=1")),
       lty=1:3,bty="n")
#
plot(x,dnorm(x,0,0.5),type="l",xlab="X",
     ylab="Densidad")
lines(x,dnorm(x,0,1),type="l",lty=2)
lines(x,dnorm(x,0,2),type="l",lty=3)
legend(5,0.8,legend=c(0.5,1,2),
       title = expression(paste(sigma," , ",mu,"=0")),
       lty=1:3,bty="n")
par(x1)


1 - pnorm(30,25,4)
pnorm(30,25,4) - pnorm(25,25,4)
qnorm(0.6,25,4)

datos.bmi = read.csv(file.choose())

hist(datos.bmi$bmi,prob=T,
     xlab="Indice de masa corporal (kg/m^2)",
     ylab="Densidad",main="",
     ylim=c(0,0.1),
     xlim=c(0,50))
x = seq(0,45,by=0.1)
lines(x,dnorm(x,25,4),type="l",col=1)
lines(x,dnorm(x,25,5),type="l",col=2)
lines(x,dnorm(x,25,6),type="l",col=3)
legend(35,0.08,legend=c(25,25,25),bty="n",
       title=expression(mu),
       col=1:3,pch=15)
legend(40,0.08,legend=c(4,5,6),bty="n",
       title=expression(sigma),
       col=1:3,pch=15)

#
# t
#

x = seq(-4,4,0.001)
plot(x,dnorm(x),type="l",xlab="Z",
     ylab="Densidad")
lines(x,dt(x,df=5),col=2)
lines(x,dt(x,df=10),col=3)
lines(x,dt(x,df=20),col=4)
legend(2,0.3,legend=c(5,10,20),bty="n",
       title=expression(nu),
       col=2:4,pch=15)

