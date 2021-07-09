
# Escalar y vector

a     <- 1
a
b     <- c(1,5,9)
b
datos <- read.csv(file.choose())
head(datos)

# Tipo de datos

Los tipos de datos mas comunes en R son

a <- 1
a
b <- "hola"
b
d <- (5 < 2)
d
e <- NA
e
f <- NULL
f

# Ejemplo 1
a <- 0
if(a == 1){
  print("Buenos dias")
}else{
  print("Buenas tardes")
}

# Ejemplo 2
a <- 5
if(a < 0){
  b = 2*a
  print(b)
}else{
  b = 3*a
  print(b)
}

# Ejemplo 3
x <- c(1,2,3,4,5,6)
y <- ifelse(x < 4,0,1)
y

# For
n <- 5
x <- c(1:5)
y <- c(1:5)
for(i in 1:n){
  y[i] <- 2*x[i]
}
y

# While
n <- 5
i <- 1
y <- x <- c(1:5)
x
y
while(i < 3){
  y[i] <- x[i] + 10
  i    <- i + 1
}
y

media <- function(x){
   suma  <- sum(x)      # suma
   n     <- length(x)   # número de elementos
   media <- suma/n      # calcula media 
   media
}

x <- c(1,5,6,9)
media(x)

media.tres <- function(x,y,z){
   suma   <- x + y + z
   media <- suma/3
   media
}

media.tres(1,4,5)

media.suma <- function(x){
  suma <- sum(x)
  n    <- length(x)
  media <- suma/n
  return(list(media=media,suma=suma))
}

x <- c(1,5,6,9)
media.suma(x)



# Bases de datos

library(haven)
datos          <- read.csv("C:/Users/Giancarlo/Documents/datos_bmi.csv")
salud.personal <- read_sav("http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav")


datos[1:5,]
datos[c(1,4,5),]
datos[datos$bmi > 40,]
datos[datos$bmi < 40 & datos$edad > 50,]

