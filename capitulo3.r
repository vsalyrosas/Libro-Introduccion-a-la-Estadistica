


library(haven)

# Base de datos
salud.paciente = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")

attr(salud.paciente$C1P39_3,"label")
cbind(attr(salud.paciente$C1P39_3,"labels"))


# Nivel de Satisfaccion
table(salud.paciente$C1P39_3,useNA = "always")
round(100*prop.table(table(salud.paciente$C1P39_3)),1)

# Grafico simple
res = prop.table(table(salud.paciente$C1P39_3))
barplot(res,ylim=c(0,1),
        xlab ="Niveles de satisfaccion",
        ylab ="Proporcion")

# Figura 1
b1 <- barplot(prop.table(table(salud.paciente$C1P39_3)),
        ylim=c(0,1),
        xaxt = "n",
        ylab="Proporción")
text(x=b1,
     y=prop.table(table(salud.paciente$C1P39_3))+0.08,
     labels = table(salud.paciente$C1P39_3))
lab <- names(attr(salud.paciente$C1P39_3,
                  "labels"))[-1]
text(b1, -0.03, labels = lab, 
     srt = 45, adj = 1, xpd = TRUE)

#
# Ingresos mensuales
#

# Datos de medicos encuestados
salud.personal <- read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos  <- salud.personal[salud.personal$C2P1 == 1,]  # Solo medicos

attr(salud.medicos$C2P28,"label")
cbind(attr(salud.medicos$C2P28,"labels"))
table(salud.medicos$C2P28)
round(prop.table(table(salud.medicos$C2P28)),2)


# Figura 2
lab <- c("750 - 1000",
         "1001 - 2000",
         "2001 - 3000",
         "3001 - 4000",
         "4001 - 5000",
         "5001 - .","NA") 
tab    = round(prop.table(table(salud.medicos$C2P28)),2)
c1     = barplot(tab,beside = T, 
                      main = "", xlab = "",
                      ylim = c(0, 1),
                      ylab = "Proporción", xaxt = "n")
text(x=c1,
     y=prop.table(table(salud.medicos$C2P28)) + 0.05,
     labels = table(salud.medicos$C2P28))
text(c1, -0.05, labels = lab, 
     srt = 45, adj = 1, xpd = TRUE)


# Numero de dependientes
# Figura 3
plot(prop.table(table(salud.paciente$C1P96)),
     ylim=c(0,0.4),ylab="Proporción",
     xlab="Número de dependientes",xaxt="n")
axis(1,at=seq(0,20,2),seq(0,20,2))


# Variable continua

c(50,29,30,48,10,45,35,60,45)
sort(c(50,29,30,48,10,45,35,60,45))

# Media
attr(salud.paciente$C1P14,"label")
salud.paciente$C1P14[1:5]
mean(salud.paciente$C1P14)
mean(salud.paciente$C1P14,na.rm=T)




fun <- function(x){
        n   = length(x)
        res = NULL
        for(i in 1:n){
          res[i] = sum((salud.paciente$C1P14 -x[i])^2,na.rm = T)
        }
        res
}
x   <- seq(10,13,0.01)
plot(x,fun(x),type="l",
     ylab="Distancia de los datos a x",
     xlab="x")

# Mediana
median(salud.paciente$C1P14)
median(salud.paciente$C1P14,na.rm=T)

#
# Quantile
#

quantile(salud.medicos$C2P2EDAD,
         prob=c(0.25,0.5,0.75))

quantile(salud.paciente$C1P14,
         prob=c(0.25,0.5,0.75),
         na.rm=T)


# Varianza
var(salud.paciente$C1P14,na.rm=T)
# DesviaciÃ³n Ã©standar
sd(salud.paciente$C1P14,na.rm=T)

# Rango
range(salud.paciente$C1P14,na.rm=T)


# RIC
quantile(salud2015$C1P14,c(0.25,0.75),na.rm=T)


mean(salud2015$C1P14,na.rm=T)
median(salud2015$C1P14,na.rm=T)
sd(salud2015$C1P14,na.rm=T)

# Asimetria Pearson
As <- function(x) 2*(mean(x,na.rm = T) - median(x,na.rm = T))/sd(x,na.rm = T)
As(salud.paciente$C1P14)

# Asimetria Fisher
Af <- function(x){
     x     = na.omit(x)
     n     = length(x)
     G     = sqrt(n*(n-1))/(n-2)*mean((x-mean(x))^3)/var(x)
     G
}
Af(salud.paciente$C1P14)



#
# Histograma
#

x1 = par(mfrow=c(1,2))
hist(salud.paciente$C1P14,ylab="Densidad",
     probability = T,
     xlab="Duración de la atención (minutos)",
     main="")
hist(salud.paciente$C1P76EDAD,ylab="Densidad",
     prob=T,
     xlab="Edad (años)",
     main="")
par(x1)

hist(salud.paciente$C1P14,ylab="Densidad",
     probability = T,
     xlab="Duración de la atención (minutos)",
     main="")


res <- hist(salud.paciente$C1P14,prob=T)
res



x1 = par(mfrow=c(1,2))
boxplot(salud.paciente$C1P14,
        xlab="Duración de atención (minutos)",
        horizontal = T)
boxplot(salud.paciente$C1P76EDAD,
        xlab="Edad (años)",
        horizontal = T)
par(x1)

#
# Funcion de distribucion acumulada
#


x = na.omit(salud.paciente$C1P14)
length(x)
table(x)
Fn = ecdf(x)  # Calcula la Funcion de distribucion acumulada
Fn(5.5)
Fn(7)
# Figura
plot(ecdf(salud.paciente$C1P14),
     xlab="Duración de atención (minutos)",
     main="")


