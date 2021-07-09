#
# Capitulo 10
# Modelos conjuntos


library(haven)

salud.profesional = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     = salud.profesional[salud.profesional$C2P1==1,]

attr(salud.medicos$C2P4,"label")
attr(salud.medicos$C2P9,"label")

tab = prop.table(table(salud.medicos$C2P9,salud.medicos$C2P4),2)
tab

plot(prop.table(table(salud.medicos$C2P9)),
     type="h",ylim=c(0,0.5),
     xlab="Número de dependientes",
     ylab="Proporción",lwd=3)
points(c(0:10,12) - 0.1,
       tab[,1],
       type="h",
       col=2,lwd=3)
points(c(0:10,12) + 0.1,
       tab[,2],
       type="h",
       col=3,lwd=3)
legend(8,0.3,c("Todos","Hombres","Mujeres"),
       col=1:3,bty="n",pch=15)


plot(prop.table(table(salud.medicos$C2P9)),
     type="h",ylim=c(0,0.5),
     xlab="Número de dependientes",
     ylab="Proporción de respuestas",lwd=2)
x = 0:12
points(x+0.1,0.4*dpois(x,2) + 0.6*dpois(x,3),
       type="h",col=2,lwd=2)
points(x + 0.2,0.3*dpois(x,2) + 0.7*dpois(x,3),
       type="h",col=3,lwd=2)
legend(6,0.3,title = "p",
       c(0.4,0.3),
       col=1:3,bty="n",pch=15)


#
# Una continua y una discreta
#

plot(density(salud.medicos$C2P2EDAD),
     ylim=c(0,0.05),
     ylab="Densidad empírica",main = "",
     xlab="Edad (años)")
lines(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==1]),
      col=2)
lines(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==2]),
      col=3)
legend(60,0.05,
       c("Todos","Hombres","Mujeres"),pch=15,
       col=1:3,bty = "n")

attr(salud.medicos$C2P4,"labels")
by(salud.medicos$C2P2EDAD,salud.medicos$C2P4,mean)
by(salud.medicos$C2P2EDAD,salud.medicos$C2P4,sd)


x1 <- par(mfrow=c(1,3))
x <- seq(18,90,0.01)
plot(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==1]),
     ylim=c(0,0.05),
     ylab="Densidad empírica",main = "",
     xlab="Edad en hombres (años)")
lines(x,dnorm(x,48,12),col=2)
legend("topright",c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
#
plot(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==2]),
     ylim=c(0,0.05),
     ylab="Densidad empírica",main = "",
     xlab="Edad en mujeres (años)")
lines(x,dnorm(x,43,11),col=2)
legend("topright",c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
#
plot(density(salud.medicos$C2P2EDAD),
     ylim=c(0,0.05),
     ylab="Densidad empírica",main = "",
     xlab="Edad (años)")
lines(x,0.6*dnorm(x,48,12) + 0.4*dnorm(x,43,11),
      col=2)
legend("topright",c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
par(x1)


lines(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==1]),
      lty=2)
lines(density(salud.medicos$C2P2EDAD[salud.medicos$C2P4==2]),
      lty=3)
legend(60,0.05,
       c("Todos","Hombres","Mujeres"),
       lty=1:3,bty = "n")


x = par(mfrow=c(1,4))
plot(density(salud.medicos$C2P2EDAD),
     ylim=c(0,0.05),
     ylab="Densidad",main = "(A)",
     xlab="Edad (años)")

plot(density(salud.medicos$C2P2EDAD[salud.medicos$INSTITUCION==1]),
     ylim=c(0,0.05),
     ylab="Densidad",main = "(B)",
     xlab="Edad (años)")
lines(density(salud.medicos$C2P2EDAD[salud.medicos$INSTITUCION==2]),
      col=2)
lines(density(salud.medicos$C2P2EDAD[salud.medicos$INSTITUCION==3]),
      col=3)
lines(density(salud.medicos$C2P2EDAD[salud.medicos$INSTITUCION==4]),
      col=4)
legend(40,0.05,title = "Institución",
       c("MINSA","ESSALUD","FFAA","Clinicas"),
       bty="n",col=1:4,pch=15)
boxplot(salud.medicos$C2P2EDAD,
        main="(C)",
        ylab="Edad (años)")
boxplot(salud.medicos$C2P2EDAD ~ salud.medicos$INSTITUCION,
        ylab="Edad (años)",xaxt="n",
        main="(D)",
        xlab="Institución")
axis(1,1:4,c("MINSA","ESSALUD","FFAA","Clinicas"))
par(x)

#
# Dos variables continuas
#

datos.bmi = read.csv(file.choose())

plot(datos$bmi.mama,datos$bmi,
     xlab = "IMC de la madre (kg/m^2)",
     ylab = "IMC del hijo (kg/m^2)")


y0 <- seq(10,40,0.01)
plot(y0,dnorm(y0,25,4),ylab="Densidad",
     xlab="Indice de masa corporal (kg/m^2)",lty=1,type="l")
lines(y0,dnorm(y0,25-0.5,4),col=2)
legend(31,0.08,legend=c("Mujer","Hombre"),
       col=1:2,bty = "n",pch=15)

model.1 <- lm(bmi~sexo,datos.bmi)
summary(model.1)
hist(model.1$residuals,pro=T,
     xlab="Residuos estimados",
     ylab="Densidad",main="")
sd(model.1$residuals)


datos.truck <- read.csv(file.choose())

plot(datos.truck$WEIGHT,datos.truck$Y,
     xlab="Peso (Kg)",
     ylab="Rigidez crítica (Nm/rad)")


model.2 <- lm(Y~WEIGHT,datos.truck)
summary(model.2)
hist(model.2$residuals,prob=T,
     xlab="Residuos estimados",
     ylab="Densidad",main="")
sd(model.2$residuals)




