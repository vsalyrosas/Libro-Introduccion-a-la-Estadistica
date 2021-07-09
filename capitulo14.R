
library(haven)

#
# Ingresos mayores a S/5000 soles mensuales
#

salud.profesional <- read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     <- salud.profesional[salud.profesional$C2P1==1,]

attr(salud.medicos$C2P28,"label")
table(salud.medicos$C2P28)


salud.medicos$y <- ifelse(salud.medicos$C2P28==6,1,
                         ifelse(salud.medicos$C2P28==7,NA,0))
p <- 0.6
n <- 2219
qnorm(c(0.025,0.975),p,sqrt(p*(1-p)/n))
# Encuesta
mean(salud.medicos$y,na.rm = T)

# Figura 1
x = seq(0,1,0.001)
plot(x,dnorm(x,p,sqrt(p*(1-p)/n)),
     type="l",
     xlab="Proporción muestral (n = 2 219)",
     ylab="Densidad",
     xlim=c(0.5,0.7))
abline(v=qnorm(c(0.01,0.99),
               p,sqrt(p*(1-p)/n)),
       lty=3,col=2,lwd=4)
points(x=mean(salud.medicos$y,na.rm = T),
       y=0,lwd=4)


valorp <- 2*pnorm(mean(salud.medicos$y,na.rm = T),
         p,sqrt(p*(1-p)/n))
valorp



#
# Encuesta
#

n <- 1000
qnorm(0.95,0.48,sqrt(0.48*(1-0.48)/n))
qnorm(0.95,0.49,sqrt(0.49*(1-0.49)/n))
qnorm(0.95,0.50,sqrt(0.50*(1-0.50)/n))


# Datos
encuesta <- read.csv(file.choose())
p <- 0.5
n <- dim(encuesta)[1]
qnorm(0.95,p,sqrt(p*(1-p)/n))
mean(encuesta$y)
valorp <- 1 - pnorm(mean(encuesta$y),p,sqrt(p*(1-p)/n))
valorp

x <- seq(0.3,0.6,0.001)
p <- 0.5
n <- 1000
plot(x,dnorm(x,p,sqrt(p*(1-p)/n)),type="l",
     xlab="Proporción muestral (n = 1 000)",
     ylab="Densidad")
abline(v=qnorm(0.95,
               p,sqrt(p*(1-p)/n)),
       lty=3,col=2,lwd=4)
points(x=mean(encuesta$y),y=0,lwd=4)

#
# Medicamento
#

medicamento <- read.csv(file.choose())
p <- 0.8 ; n <- 200
qnorm(0.02,p,sqrt(p*(1-p)/n))
mean(medicamento$y)
valorp <- pnorm(mean(medicamento$y,na.rm = T),
                 p,sqrt(p*(1-p)/n))
valorp

x <- seq(0.6,0.9,0.001)
p <- 0.8
n <- 200
plot(x,dnorm(x,p,sqrt(p*(1-p)/n)),type="l",
     xlab ="Proporcion muestral (n = 200)",
     ylab = "Densidad")
abline(v=qnorm(0.02,
               p,sqrt(p*(1-p)/n)),
       lty=3,col=2,lwd=4)
points(x=mean(medicamento$y),y=0,lwd=5)


#
# Funcion prop.test
#

prop.test(sum(medicamento$y),length(medicamento$y),
          p = 0.8,alternative="less",
          conf.level = 0.98,
          correct = F)

p <- 0.8
n <- 200
p.hat <- mean(medicamento$y)
z     <- (p.hat - p)/sqrt(p*(1-p)/n)
z
X2    <- z^2
X2
#
# Valor p: Chi-cuadrado
valorp <- (1-pchisq(X2,df=1))/2
valorp
#
# Valro p: Estadistico z
valorp <- pnorm(mean(medicamento$y),p,sqrt(p*(1-p)/n))
valorp

#
# Dados
#

EX   <- sum(rep(1/6,6)*1:6) ; EX2  <- sum(rep(1/6,6)*c(1:6)^2)
VARX <- EX2 - EX^2  ; n    <- 40
qnorm(c(0.02,0.98),EX,sqrt(VARX/n))
valorp <- 2*(1 - pnorm(4.2,EX,sqrt(VARX/n)))
valorp
        
x <- seq(2,5,0.01)
plot(x,dnorm(x,EX,sqrt(VARX/n)),
     type="l",ylab="Densidad",
     xlab="Media muestral (n = 40)")
abline(v=qnorm(c(0.02,0.98),
               EX,sqrt(VARX/n)),
       lty=3,lwd=4,col=2)
points(x=4.2,y=0,lwd=4)



#
# Duracion de la atencion
#

salud.pacientes = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")


# Estadisticos
mu.hat <-mean(salud.pacientes$C1P14,na.rm = T)
mu.hat
sd.hat <- sd(salud.pacientes$C1P14,na.rm=T)
sd.hat

# Prueba de hipotesis
n      <- dim(salud.pacientes)[1]
qt(c(0.025,0.975),n-1)
t.stat <- (mu.hat - 10)/sqrt(sd.hat^2/n)
t.stat
valorp <- 2*(1-pt(t.stat,df=n-1))
valorp

x <- seq(-10,30,0.001)
plot(x,dt(x,n-1),type="l",
     xlab = "t",
     ylab = "Densidad")
li <- qt(0.025,n-1)
ls <- qt(0.975,n-1)
abline(v=qt(c(0.025,0.975),n-1),lty=3,
       col=2,lwd=4)
points(x=t.stat,y=0,lwd=4)

t.test(salud.pacientes$C1P14,mu=10,alternative = "two.sided")

#
# Gaseosa
#

gaseosa <- read.csv(file.choose())
mu.hat  <- mean(gaseosa$contenido)
mu.hat
sd.hat  <- sd(gaseosa$contenido)
sd.hat
n       <- dim(gaseosa)[1]
t.stat  <- (mu.hat - 500)/(sd.hat/sqrt(n))
t.stat
qt(0.01,df=99)


n <- dim(gaseosa)[1]
x <- seq(-30,5,0.001)
plot(x,dt(x,n-1),type="l",
     xlab = "t",
     ylab = "Densidad")
li <- qt(0.01,n-1)
abline(v=qt(0.01,n-1),
         lty=2,col=2,lwd=4)
points(x=t.stat,y=0,lwd=4)


t.test(gaseosa$contenido,mu=500,
       alternative = "less",
       conf.level = 0.99)

