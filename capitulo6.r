

poblacion = read.csv(file.choose())

# Muestreo simple

set.seed(100)
muestra = sample(1:100,size=20,replace=T)  # Ubicacion de la muestra
muestra
poblacion$edad[muestra]
mean(poblacion$edad[muestra])

res = rep(0,200)
for(i in 1:200){
  muestra = sample(1:100,size=20,replace=T)  # Ubicacion de la muestra
  res[i]  = mean(poblacion$edad[muestra])
}
summary(res)
hist(res,prob=T,main="",
     ylim = c(0,0.15),
     xlab = "Media muestral de edades (años)",
     ylab = "Densidad")

# Muestreo estratificado

set.seed(100)
muestraA = sample(1:70,size=20*0.7,replace=T)  
muestraA
muestraB = sample(71:100,size=20*0.3,replace=T)  
muestraB
poblacion$edad[c(muestraA,muestraB)]
0.7*mean(poblacion$edad[muestraA]) + 0.3*mean(poblacion$edad[muestraB])


resA = resB = res = rep(0,200)
for(i in 1:200){
  muestraA = sample(1:70,size=20*0.7,replace=T)  
  muestraB = sample(71:100,size=20*0.3,replace=T)  
  resA[i]  = mean(poblacion$edad[muestraA])
  resB[i]  = mean(poblacion$edad[muestraB])
  res[i]   = 0.7*resA[i] + 0.3*resB[i]
}
summary(resA)
summary(resB)
summary(res)

x1 <- par(mfrow=c(1,3))
hist(resA,prob=T,main="",
     ylim = c(0,0.15),
     xlab = "Media muestral subpoblación A",
     ylab = "Densidad")
hist(resB,prob=T,main="",
     ylim = c(0,0.15),
     xlab = "Media muestral subpoblación B",
     ylab = "Densidad")
hist(res,prob=T,main="",
     ylim = c(0,0.15),
     xlab = "Media muestral",
     ylab = "Densidad")
par(x1)

# Muestreo conveniencia

set.seed(100)
muestra = 80:100
poblacion$edad[muestraA]
mean(poblacion$edad[muestra])



