
#
# Capitulo V: Regresion lineal
#

databmi = read.csv(file.choose())
names(databmi)

plot(databmi$bmi.papa,databmi$bmi,
     xlab="Indice de masa corporal del padre (kg/m^2)",
     ylab="Indice de masa corporal del hijo (kg/m^2)",
     main="")
abline(c(18,0.1),col=2,lwd=2,lty=2)
abline(c(22,0.3),col=3,lwd=2,lty=2)
abline(c(20,0.12),col=4,lwd=2,lty=2)


model  = lm(bmi ~ bmi.papa,databmi)
model
sd.y   = sd(databmi$bmi)
sd.x   = sd(databmi$bmi.papa)
cor.xy = cor(databmi$bmi,databmi$bmi.papa)
mean.x = mean(databmi$bmi.papa)
mean.y = mean(databmi$bmi)
beta1  = cor.xy*sd.y/sd.x
beta0  = mean.y - beta1*mean.x
beta0
beta1


databmi$y.hat.1 = predict(model,databmi)
databmi$y.hat.2 = beta0 + beta1*databmi$bmi.papa
head(databmi)
sum((databmi$y.hat.1 - databmi$bmi)^2)


plot(databmi$bmi.papa,databmi$bmi,
     xlab="Indice de masa corporal del padre (kg/m^2)",
     ylab="Indice de masa corporal del hijo (kg/m^2)",
     main="")
model = lm(bmi ~ bmi.papa,databmi)
abline(coef(model))

y.hat = predict(model,databmi)
sum((y.hat - databmi$bmi)^2)

model = lm(bmi ~ bmi.papa,databmi)
summary(model)

#
# Dos variables
#

datos.control = read.csv(file.choose())

plot(datos.control$peso,datos.control$Y,
     xlab="Peso (Kg)",
     ylab="Rígidez crítica (Nm/rad)",type="n")
points(datos.control$peso[datos.control$sexo=="M"],
       datos.control$Y[datos.control$sexo=="M"],
       pch=1,col=2)
points(datos.control$peso[datos.control$sexo=="F"],
       datos.control$Y[datos.control$sexo=="F"],
       pch=2,col=3)
legend(50,180,c("Hombre","Mujer"),
       pch=1:2,bty="n",col=2:3)


model1 = lm(Y ~ peso,datos.control)
model1
model2 = lm(Y ~ peso + factor(sexo),datos.control)
model2


summary(model1)
summary(model2)

