
#
# Capitulo IV
#

library(haven)  # activa la libreria

#
# Cualitativa vs. Cuantitativa
#


salud.paciente = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2001%20-%20CAPITULOS.sav")

attr(salud.paciente$INSTITUCION,"labels")
table(salud.paciente$INSTITUCION)
round(100*prop.table(table(salud.paciente$INSTITUCION)),1)
# Medidas de resumen
by(salud.paciente$C1P14,
   salud.paciente$INSTITUCION,
   summary)

# Figura 1
x = par(mfrow=c(1,2))
lab  = c("MINSA","ESSALUD","FFAA","Clínicas")
boxplot(salud.paciente$C1P14 ~ salud.paciente$INSTITUCION,
        ylab="Duración de atención (minutos)", 
        xlab="Institución",
        xaxt = "n")
axis(1,at=1:4,labels = lab)

plot(ecdf(salud.paciente$C1P14[salud.paciente$INSTITUCION==1]),
     main="",xlab="Duración de la atención (minutos)")
lines(ecdf(salud.paciente$C1P14[salud.paciente$INSTITUCION==2]),
      col=2)
lines(ecdf(salud.paciente$C1P14[salud.paciente$INSTITUCION==3]),
      col=3)
lines(ecdf(salud.paciente$C1P14[salud.paciente$INSTITUCION==4]),
      col=4)
legend(40,0.6,lab,col=1:4,bty="n",pch=15)
par(x)

#
# Cualitativa vs. Cualitiva
#

# Encuesta Nacional ENSUSALUD 2015: Medicos/Enfermeras
salud.profesional = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     = salud.profesional[salud.profesional$C2P1 == 1,]  # Solo medicos

# Genero entre los medicos
attr(salud.medicos$C2P4,"labels")
table(salud.medicos$C2P4)
round(100*prop.table(table(salud.medicos$C2P4)),2)

# Ingresos
attr(salud.medicos$C2P28,"labels")
table(salud.medicos$C2P28)
round(100*prop.table(table(salud.medicos$C2P28)),2)

table(salud.medicos$C2P4,salud.medicos$C2P28)

# Condicionales
# a) Condicionado en el genero del  profesional
100*prop.table(table(salud.medicos$C2P4,salud.medicos$C2P28),1)
# b) Condicional en cada nivel de ingreso
100*prop.table(table(salud.medicos$C2P4,salud.medicos$C2P28),2)

# Conjunta
100*prop.table(table(salud.medicos$C2P4,salud.medicos$C2P28))

# Figura 2
x = par(mfrow=c(1,3))
lab <- c("750 - 1000",
         "1001 - 2000",
         "2001 - 3000",
         "3001 - 4000",
         "4001 - 5000",
         "5001 - .","NA")
res  = c(0,prop.table(table(salud.medicos$C2P28[salud.medicos$C2P4==1])))
c1   = barplot(res,beside = T, 
               main = "Hombres", xlab = "",
               ylim = c(0, 1),
               ylab = "Proporción", xaxt = "n")
text(c1, -0.05, labels = lab, 
     srt = 45, adj = 1, xpd = TRUE)
#
res  = prop.table(table(salud.medicos$C2P28[salud.medicos$C2P4==2]))
c1   = barplot(res,beside = T, 
               main = "Mujeres", xlab = "",
               ylim = c(0, 1),
               ylab = "Proporción", xaxt = "n")
text(c1, -0.05, labels = lab, 
     srt = 45, adj = 1, xpd = TRUE)
#

res  = prop.table(table(salud.medicos$C2P4,salud.medicos$C2P28),1)
c1   = barplot(res,beside = T, 
                      main = "", xlab = "",
                      ylim = c(0, 1),col=1:2,
                      ylab = "Proporción", xaxt = "n")
text(c1[1,], -0.05, labels = lab, 
     srt = 45, adj = 1, xpd = TRUE)
legend(c1[1,1],0.6,c("Hombre","Mujer"),pch=15,col=1:2,bty="n")
par(x)


##################################################
#
# Correlacion
#
##################################################

databmi = read.csv(file.choose())
names(databmi)

x = par(mfrow=c(1,2))
plot(databmi$bmi.papa,databmi$bmi,
     xlab="Indice de masa corporal del padre (kg/m^2)",
     ylab="Indice de masa corporal del hijo (kg/m^2)",
     main="(A)")
databmi$s.bmi.papa = (databmi$bmi.papa - mean(databmi$bmi.papa))/sd(databmi$bmi.papa)
databmi$s.bmi      = (databmi$bmi - mean(databmi$bmi))/sd(databmi$bmi)
plot(databmi$s.bmi.papa,databmi$s.bmi,
     xlab="IMC estandarizado del padre",
     ylab="IMC estandarizado del hijo",
     main="(B)",
     xlim=c(-3,3),
     ylim=c(-3,3))
abline(h=0,lty=2)
abline(v=0,lty=2)
text(2,2,"I",cex = 2)
text(-2,2,"II",cex = 2)
text(-2,-2,"III",cex = 2)
text(2,-2,"IV",cex = 2)
par(x)

# CorrelaciÃ³n
cor(databmi$bmi,databmi$bmi.papa)

# Armas de fuego


datos = read.csv(file.choose())

plot(datos$armas,datos$muertes,
     xlab="Media de armas por cada 100 habitantes",
     ylab="Número de muertes por cada 100 000 habitantes")
text(datos$armas[26]-0.5,datos$muertes[26]-0.5,labels = "US")
text(datos$armas[21]-0.5,datos$muertes[21]-0.5,labels = "Sudáfrica")

cor(datos.armas$armas,datos.armas$muertes)

# Retirar US
cor(datos.armas$armas[-26],
    datos.armas$muertes[-26])
# Retirar Sudafrica
cor(datos.armas$armas[-21],
    datos.armas$muertes[-21])
# Retirar Sudafrica y US
cor(datos.armas$armas[-c(21,26)],
    datos.armas$muertes[-c(21,26)])


