## Metodos cuantitativos
# Sesion 00

# Base de datos con digimones de la primera y segunda serie

# Pero antes ... 
# Algunas funciones 
10000000 + (100000 * 100)

# A veces en Rstudio se nos muestran resultados con notacion cientifica
# Para eliminarla

options(scipen = 999)

# Volvemos a hacerlo
10000000 + (100000 * 100)

## Crear numeros aleatorios
Matemáticas <- c(sample(70:100, 10))
Química <- c(sample(70:100, 10))

# Convertir a data.frame
notas <- data.frame(Matemáticas,Química)
names(notas)

## Otra forma con la funcion round
x1 = round(runif(100,0,1), 2) # donde 100 es el numero de datos, 0 y 1 el rango, 2 decimales

aleatorios <- data.frame(x1)

## Ahora volvemos a los datos de digimon

digimon <- read.csv("DigiDB_digimonlist.csv")

## Jugando con los datos
table(digimon$Type) # operador signo de pesos para llamar variables
summary(digimon$LvAtk) #algunas estadisticas descriptivas
plot(digimon$LvAtk, digimon$LvDef) #grafica de dispersion
cor(digimon$LvAtk, digimon$LvDef) #correlacion

plot(digimon$LvAtk, digimon$LvDef, pch = 14) #del 1 al 25

# Tablas

digimon.tabla <- table(digimon$Stage)
plot(digimon.tabla)

# Grafico de barras
barplot(digimon.tabla)

hist(digimon$LvAtk) # distribucion poisson

hist(digimon$LvAtk,
     xlab = "Ataque",
     main = "Histograma poder de digimones")

# Regresion
fit1 <- lm (LvHP ~ LvAtk, data = digimon) 
summary(fit1)  

