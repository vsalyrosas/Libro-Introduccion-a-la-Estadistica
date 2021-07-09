
datos.bmi = read.csv(file.choose())
mean(datos.bmi$bmi)
var(datos.bmi$bmi)

# Muestra de normales
n       <- 1:10000        # Tamano de muestra
epsilon <- 0.1            # Fijamos epsilon = 0.1
sigma   <- 5              # Fijamos sigma   = 5
plot(n,2*pnorm(-epsilon*sqrt(n)/sigma),
     ylim=c(0,1),type="l",
     xlab="Tamaño de muestra",
     ylab=expression(paste("P(|",bar(X)," - ",mu,"| >",epsilon,")")))
abline(h=0,lty=2)

# Muestra de bernoullis

n       = 5:200
epsilon = 0.1
p       = 0.5
plot(n,pbinom(n*(p-epsilon),n,p) + (1-pbinom(n*(p + epsilon),n,p)),
     ylim=c(0,1),type="l",
     xlab="Tamaño de muestra",
     ylab=expression(paste("P(|",bar(X)," - ",mu,"| >",epsilon,")")))
points(n,pbinom(n*(p-epsilon),n,p) + (1-pbinom(n*(p + epsilon),n,p)))
abline(h=0,lty=2)



#
# TLC: Lanzar dados
#

K <- 5000     # Numero de simulaciones
res.50 = res.100 = res.200 = res.500 = rep(0,K)   # Almacenamos las medias
for(i in 1:K){
  res.50[i]   <- mean(sample(1:6,50,replace=T))   # Media de 50 lanzamientes
  res.100[i]  <- mean(sample(1:6,100,replace=T))  # Media de 100 lanzamientos
  res.200[i]  <- mean(sample(1:6,200,replace=T))  # Meida de 200 lanzamientos
}

x <- seq(0,6,0.001)
x1 <- par(mfrow=c(1,2))
hist(res.50,prob=T,main="",xlim=c(2,5),
     ylab = "Densidad",ylim=c(0,3),
     xlab = "Media muestral (n=50)")
lines(x,dnorm(x,3.5,sqrt(2.92/50)),col=2,lwd=3)
legend(2.5,3,c("Simulación","Predicción vía el TLC"),
       col=1:2,bty="n",pch=15)
#
hist(res.100,prob=T,main="",xlim=c(2,5),
     ylab = "Densidad",ylim=c(0,3),
     xlab = "Media muestral (n=100)")
lines(x,dnorm(x,3.5,sqrt(2.92/100)),col=2,lwd=3)
legend(2.5,3,c("Simulación","Predicción vía el TLC"),
       col=1:2,bty="n",pch=15)
#
par(x1)


#
# TLC: Hincha de futbol
#

K = 5000
p = 0.7
res.200 = res.300 = rep(0,K)
for(i in 1:K){
  res.200[i]  = mean(rbinom(200,1,p))
  res.300[i]  = mean(rbinom(300,1,p))
}

x = seq(0,1,0.001)
x1 = par(mfrow=c(1,2))
hist(res.200,prob=T,main="",ylim=c(0,20),
     ylab = "Densidad",xlim=c(0.5,0.9),
     xlab = "Media muestral (n=200)")
lines(x,dnorm(x,p,sqrt(p*(1-p)/200)),col=2,lwd=3)
legend(0.6,19,c("Simulación","Predicción vía el TLC"),
       col=1:2,bty="n",pch=15)
#
hist(res.300,prob=T,main="",ylim=c(0,20),
     ylab = "Densidad",xlim=c(0.5,0.9),
     xlab = "Media muestral (n=300)")
lines(x,dnorm(x,p,sqrt(p*(1-p)/300)),col=2,lwd=3)
legend(0.6,19,c("Simulación","Predicción via el TLC"),
       col=1:2,bty="n",pch=15)
#
par(x1)


# Dado
x = seq(1,6,0.01)
plot(x,dnorm(x,3.5,sqrt(2.92/50)),
     type="l",xlab=expression(bar(X)),
     ylab="Densidad")
points(x=4.2,y=0,lwd=4)
1 - pnorm(4.3,3.5,sqrt(2.92/40))



# Ingresos mensuales
library(haven)
salud.profesional = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     = salud.profesional[salud.profesional$C2P1==1,]

attr(salud.medicos$C2P28,"label")
table(salud.medicos$C2P28)
salud.medicos$y = ifelse(salud.medicos$C2P28==6,1,
                         ifelse(salud.medicos$C2P28==7,NA,0))
salud.medicos   = salud.medicos[!is.na(salud.medicos$y),]
p.hat           = mean(salud.medicos$y)
p.hat
p = 0.6
n = dim(salud.medicos)[1]
n
pnorm(p.hat,p,sqrt(p*(1-p)/n))

x  = seq(0,1,0.001)
plot(x,dnorm(x,p,sqrt(p*(1-p)/n)),
     type="l",
     xlab="Proporción muestral (n=2154)",
     ylab="Densidad",
     xlim=c(0.5,0.7))
points(x=mean(salud.medicos$y,na.rm = T),
       y=0,lwd=4)


