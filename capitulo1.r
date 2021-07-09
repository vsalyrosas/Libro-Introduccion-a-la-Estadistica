
library(haven)

datos = read.csv(file.choose())

plot(datos$bmi.papa,datos$bmi,
     xlab="Índice de masa corporal del padre (kg/m^2)",
     ylab="Índice de masa corporal del hijo (kg/m^2)")


# Ingresos en hombres y mujeres

salud.profesional = read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")
salud.medicos     = salud.profesional[salud.profesional$C2P1 == 1,]  # Solo medicos

# Figura 2
x = par(mfrow=c(1,2))
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
par(x)

