

# 1 = cara
# 0 = sello
poblacion   = c(0,1)
muestra.2   = sample(poblacion,size=2,replace=T)
muestra.2
mean(muestra.2)          # Proporcionn que son "1"
muestra.10   = sample(poblacion,size=10,replace=T)
muestra.10
mean(muestra.10)
muestra.20   = sample(poblacion,size=20,replace=T)
muestra.20
mean(muestra.20)


x1 = par(mfrow=c(1,2))
N = 100
resultados  = rep(0,N)
for(i in 1:N){
  resultados[i] = mean(sample(poblacion,size=i,replace=T))
}

plot(1:N,resultados,ylim=c(0,1),
     xlab = "Número de Lanzamientos",
     ylab="Proporción de caras",
     main="(A)")
lines(1:N,resultados,lty=2)
abline(h=0.5,col=2,lwd=3)
#
N = 5000
resultados  = rep(0,N)
for(i in 1:N){
  resultados[i] = mean(sample(poblacion,size=i,replace=T))
}

plot(1:N,resultados,ylim=c(0,1),
     xlab = "Número de lanzamientos",
     ylab="Proporción de caras",
     lty  = 2,type="n",main="(B)")
lines(1:N,resultados,lty=2)
abline(h=0.5,col=2,lwd=3)
abline(v=100)
par(x1)

#
#
#
installed.packages("prob")
library(prob)

# Lanzar dos dados
sample.space = urnsamples(1:6, size = 2, replace = T, ordered = TRUE)
plot(sample.space)

sample.space = urnsamples(1:6, size = 2, replace = T, ordered = TRUE)
A  = rowSums(sample.space) > 5
A
nA = sum(A)
nA

D  = rowSums(sample.space) ==  6
D
nD = sum(D)
nD
