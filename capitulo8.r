

# Funcion de distribuci√≥n acumululada

cdf.discreta <- function(y,py,myxlab="Y",
                         myylab="P(Y<=y)")
{
        n  <- length(y)
        ny <- c(y[1]-1,y,y[n] + 1)        
        Fy <- cumsum(c(0,py,0))
        plot(ny,Fy,type="n",
             xlab=myxlab,
             ylab="F(x)")
        points(ny[-c(1,(n+2))],Fy[-c(1,(n+2))],pch=19)
        points(y,Fy[-c((n+1):(n+2))],pch=1)         
        for(i in 1:(n+1)){
                segments(ny[i],Fy[i],ny[i+1],Fy[i])
                segments(ny[i+1],Fy[i],ny[i+1],Fy[i+1],lty=3)         
        } 
}

library(haven)


# Bases de datos
salud.profesionales  <- read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.pacientes      <- read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")
salud.medicos        <- salud.profesionales[salud.profesionales$C2P1==1,]


x1 = par(mfrow=c(1,2))
y  <- 2:12
py <- c(1,2,3,4,5,6,5,4,3,2,1)/36
plot(y,py,type="h",
     xlab="N˙mero de intentos",
     ylab="P(X=x)",
     ylim=c(0,1))
points(y,py)
cdf.discreta(y,py,myxlab="N˙mero de intentos")
par(x1)

dpois(0,2)
1 - ppois(3,2)

x1 = par(mfrow=c(1,2))
plot(prop.table(table(salud.pacientes$C1P96)),
     xlab="N˙mero de  dependientes",
     ylab="ProporciÛn")
points(0:5+0.2,dpois(0:5,2),
       type="h",col=2,lwd=2)
legend(11,0.2,c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
#
plot(ecdf(salud.pacientes$C1P96),
     main="",xlab="N˙mero de dependientes")
points(0:20,ppois(0:20,2),col=2,lwd=2)
legend(11,0.8,c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
par(x1)

hist(salud.medicos$C2P2EDAD,
     xlab = "Edad (aÒos)",
     ylab = "Densidad",prob=T,
     main = "",ylim=c(0,0.03))
lines(density(salud.medicos$C2P2EDAD),col=2)

x2 = par(mfrow=c(1,2))
hist(salud.medicos$C2P2EDAD,
     xlab = "Edad (aÒos)",
     ylab = "Densidad",prob=T,
     main = "",ylim=c(0,0.03))
x = seq(0,90,0.01)
lines(x,dunif(x,min=25,max=80),
      col=2)
legend(70,0.03,c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
#
plot(ecdf(salud.medicos$C2P2EDAD),
     xlab="Edad (aÒos)",main="")
lines(x,punif(x,min=25,max=80),col=2)
legend(60,0.3,c("Datos","Modelo"),
       col=1:2,bty="n",pch=15)
par(x2)

K       <- 2000
muestra <- sample(1:6,K,replace = T) + sample(1:6,K,replace = T)
summary(muestra)

