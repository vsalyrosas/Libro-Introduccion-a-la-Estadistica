
library(haven)


#
# Duracion de la atencion
#

salud.pacientes = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")

salud.pacientes = salud.pacientes[!is.na(salud.pacientes$C1P14),]
n      = dim(salud.pacientes)[1]
n
mu.hat = mean(salud.pacientes$C1P14,na.rm = T)
mu.hat
var.hat = var(salud.pacientes$C1P14,na.rm=T)
var.hat
ic     = mu.hat + qt(c(0.025,0.975),df=n-1)*sqrt(var.hat/n)
ic

t.test(salud.pacientes$C1P14)

# Por institucion
by(salud.pacientes$C1P14,salud.pacientes$INSTITUCION,
   function(x) c(length(x),mean(x),var(x)))


res = by(salud.pacientes$C1P14,salud.pacientes$INSTITUCION,
      function(x) as.numeric(t.test(x)$conf.int))

plot(c(10,15),c(1,4),type="n",
     xlab="Duración de atención (minutos)",
     ylab="Institución",yaxt="n")
segments(x0=res$"1"[1],x1=res$"1"[2],y0=1,y1=1,
          lwd=2)
segments(x0=res$"2"[1],x1=res$"2"[2],y0=2,y1=2,
         lwd=2)
segments(x0=res$"3"[1],x1=res$"3"[2],y0=3,y1=3,
         lwd=2)
segments(x0=res$"4"[1],x1=res$"4"[2],y0=4,y1=4,
         lwd=2)
axis(2,at=1:4,labels=c("MINSA","ESSALUD","FFAA","Clinicas"))



# Ingresos mensuales

salud.profesional = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     = salud.profesional[salud.profesional$C2P1==1,]

attr(salud.medicos$C2P28,"label")
table(salud.medicos$C2P28)
salud.medicos$y = ifelse(salud.medicos$C2P28==6,1,
                         ifelse(salud.medicos$C2P28==7,NA,0))
salud.medicos   = salud.medicos[!is.na(salud.medicos$y),]
n     = dim(salud.medicos)[1]
n
p.hat = mean(salud.medicos$y)
p.hat
ic    = p.hat + qnorm(c(0.025,0.975))*sqrt(p.hat*(1-p.hat)/n)
ic

n     = dim(salud.medicos)[1]
prop.test(x=sum(salud.medicos$y),n=n)


#
# Regresion
#

datos.bmi = read.csv(file.choose())

model.1     = lm(bmi ~ bmi.mama,datos.bmi)
summary(model.1)
confint(model.1,level = 0.95)


model.2     = lm(bmi ~ bmi.papa + edad.papa +  bmi.mama + edad.mama,
                 datos.bmi)
summary(model.2)
confint(model.2,level = 0.95)

#
# Tama;o de muestra
#

datos = read.csv(file.choose())
var(datos$bmi)
var(datos$bmi.mama)
var(datos$bmi.papa)

#
# Media
#

alpha <- 0.05
sigma <- 4
e     <- 1
n     <- qnorm(alpha/2)^2*sigma^2*e^(-2)
n

#
# Proporcion
#

alpha <- 0.05
p     <- 0.75
e     <- 0.2
n     <- qnorm(alpha/2)^2*p*(1-p)*e^(-2)
n


# Confiabilidad vs. Probabilidad
# Simulacion

K      = 100     # Numero de repeticiones del experimento
n      = 50      # Numero de observaciones del experimento
li = ls = rep(0,K)
for(i in 1:K){
        datos   = sample(1:6,n,replace = T)
        mu.hat  = mean(datos)
        var.hat = var(datos)
        li[i]   = mu.hat + qnorm(0.025)*sqrt(var.hat/n)
        ls[i]   = mu.hat + qnorm(0.975)*sqrt(var.hat/n)
}

plot(c(1,100),c(1,6),type="n",
     xlab="Simulación",
     ylab="Intervalo de confianza")
for(i in 1:100){
  if(li[i] > 3.5 | ls[i] < 3.5){
          segments(x0=i,x1=i,y0=li[i],y1=ls[i],col=2,lwd=2)}
  else segments(x0=i,x1=i,y0=li[i],y1=ls[i])
}
abline(h=3.5,lwd=2)


