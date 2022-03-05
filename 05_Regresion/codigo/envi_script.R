# Encuesta Nacional de Vivienda

## Cargamos librerias 
library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, foreign, survey, dplyr, ggplot2)

# Cargamos datos demograficos
demografico <- read_csv("TSDEM.csv")

# Convertir a numerico
demografico$P3_4 <- as.numeric(demografico$P3_4)

# Descriptivos
summary(demografico$P3_4) #Solo para ver la conversion a numerico, falta filtrar

# Ingresos sin no responde o no sabe que en la encuesta lo toma como 99999
ingresos_dem <- demografico %>%
  filter(P3_4 %in% c(100:98000))

# Cada cuanto gana
ingresos_dem <- demografico %>%
  filter(P3_4A %in% c(1:4))

# Convertir a mensual
ingresos_dem$convertir <- NA
ingresos_dem$convertir[ingresos_dem$P3_4A == "1"] <- 4
ingresos_dem$convertir[ingresos_dem$P3_4A == "2"] <- 2
ingresos_dem$convertir[ingresos_dem$P3_4A == "3"] <- 1

# Ingresos mensuales
ingresos_dem$ing_mes <- NA
ingresos_dem$ing_mes <- ingresos_dem$P3_4 * ingresos_dem$convertir

# Descriptivos
summary(ingresos_dem$ing_mes)

## Crear NSE
ingresos_dem$nse <- NA
ingresos_dem$nse[ingresos_dem$ing_mes <= 2000] <- "E"
ingresos_dem$nse[ingresos_dem$ing_mes >= 2001 & ingresos_dem$ing_mes <= 4790] <- "D"
ingresos_dem$nse[ingresos_dem$ing_mes >= 4791 & ingresos_dem$ing_mes <= 10500] <- "D+"
ingresos_dem$nse[ingresos_dem$ing_mes >= 10501 & ingresos_dem$ing_mes <= 22900] <- "C-"
ingresos_dem$nse[ingresos_dem$ing_mes >= 22901 & ingresos_dem$ing_mes <= 34900] <- "C"
ingresos_dem$nse[ingresos_dem$ing_mes >= 34901 & ingresos_dem$ing_mes <= 64900] <- "C+"
ingresos_dem$nse[ingresos_dem$ing_mes >= 64901] <- "A/B"

## Frecuencias de NSE
ingresos_dem %>% frq(nse, weights= FACTOR)

# Cargamos datos de viviendas
viviendas <- read_csv("TVIVIENDA.csv")

# Union de bases de datos
data_caracteristicas <- left_join(ingresos_dem, viviendas)

## Prop table
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P4_21_2), 1), 2)

round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_2A), 1), 2)

# Pensado en esta vivienda, dígame, ¿qué tan satisfechos están con la calidad del piso?
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_3_1), 1), 2)

# Pensado en esta vivienda, dígame, ¿qué tan satisfechos están con la calidad de los muros y techos?
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_3_2), 1), 2)

# Pensado en esta vivienda, dígame, ¿qué tan satisfechos están con la pintura, recubrimientos y otros acabados?
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_3_3), 1), 2)

# Pensado en esta vivienda, dígame, ¿qué tan satisfechos están con la iluminación natural?
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_3_4), 1), 2)

# Pensado en esta vivienda, dígame, ¿qué tan satisfechos están con la ventilación natural?
round(prop.table(table(data_caracteristicas$nse, data_caracteristicas$P6_3_5), 1), 2)

#Convertimos las variables en numéricas

# ¿Hace cuánto tiempo fue construida esta vivienda?
data_caracteristicas$P4_19_1 <- as.numeric(data_caracteristicas$P4_19_1)
table(data_caracteristicas$P4_19_1)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_19_1 %in% c(0:97))

# ¿Cuántos metros de construcción tiene esta vivienda?
data_caracteristicas$P4_21_1 <- as.numeric(data_caracteristicas$P4_21_1)
table(data_caracteristicas$P4_21_1)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_21_1 %in% c(0:997))

#¿Cuántos metros de construcción tiene esta vivienda?
data_caracteristicas$P4_21_2 <- as.numeric(data_caracteristicas$P4_21_2)

# ¿Esta vivienda está equipada con lavadero?
data_caracteristicas$P4_22_1 <- as.numeric(data_caracteristicas$P4_22_1)
table(data_caracteristicas$P4_22_1)
data_caracteristicas$lavadero <- NA
data_caracteristicas$lavadero[data_caracteristicas$P4_22_1 == 1] <- "1"
data_caracteristicas$lavadero[data_caracteristicas$P4_22_1 == 2] <- "0"
table(data_caracteristicas$lavadero)

# ¿Esta vivienda está equipada con fregadero o tarja?
data_caracteristicas$P4_22_2 <- as.numeric(data_caracteristicas$P4_22_2)
table(data_caracteristicas$P4_22_2)
data_caracteristicas$fregadero <- NA
data_caracteristicas$fregadero[data_caracteristicas$P4_22_2 == 1] <- "1"
data_caracteristicas$fregadero[data_caracteristicas$P4_22_2 == 2] <- "0"
table(data_caracteristicas$fregadero)

# ¿Esta vivienda está equipada con tinaco?
data_caracteristicas$P4_22_3 <- as.numeric(data_caracteristicas$P4_22_3)
table(data_caracteristicas$P4_22_3)
data_caracteristicas$tinaco <- NA
data_caracteristicas$tinaco[data_caracteristicas$P4_22_3 == 1] <- "1"
data_caracteristicas$tinaco[data_caracteristicas$P4_22_3 == 2] <- "0"
table(data_caracteristicas$tinaco)

# ¿Esta vivienda está equipada con aire acondicionado?
data_caracteristicas$P4_22_8 <- as.numeric(data_caracteristicas$P4_22_8)
table(data_caracteristicas$P4_22_8)
data_caracteristicas$aire <- NA
data_caracteristicas$aire[data_caracteristicas$P4_22_8 == 1] <- "1"
data_caracteristicas$aire[data_caracteristicas$P4_22_8 == 2] <- "0"
table(data_caracteristicas$aire)

# ¿Esta vivienda cuenta con sala comedor?
data_caracteristicas$P4_23_1 <- as.numeric(data_caracteristicas$P4_23_1)
table(data_caracteristicas$P4_23_1)
data_caracteristicas$comedor <- NA
data_caracteristicas$comedor[data_caracteristicas$P4_23_1 == 1] <- "1"
data_caracteristicas$comedor[data_caracteristicas$P4_23_1 == 2] <- "0"
table(data_caracteristicas$comedor)

# ¿Esta vivienda cuenta con jardín?
data_caracteristicas$P4_23_2 <- as.numeric(data_caracteristicas$P4_23_2)
table(data_caracteristicas$P4_23_2)
data_caracteristicas$jardin <- NA
data_caracteristicas$jardin[data_caracteristicas$P4_23_2 == 1] <- "1"
data_caracteristicas$jardin[data_caracteristicas$P4_23_2 == 2] <- "0"
table(data_caracteristicas$jardin)

# ¿Esta vivienda cuenta con cochera o cajón de estacionamiento?
data_caracteristicas$P4_23_6 <- as.numeric(data_caracteristicas$P4_23_6)
table(data_caracteristicas$P4_23_6)
data_caracteristicas$cochera <- NA
data_caracteristicas$cochera[data_caracteristicas$P4_23_6 == 1] <- "1"
data_caracteristicas$cochera[data_caracteristicas$P4_23_6 == 2] <- "0"
table(data_caracteristicas$cochera)

# Esta vivienda, ¿tiene problemas con grietas o cuarteaduras en techos o muros?
data_caracteristicas$P4_25_1 <- as.numeric(data_caracteristicas$P4_25_1)
table(data_caracteristicas$P4_25_1)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_25_1 %in% c(1:2))
data_caracteristicas$grietas <- NA
data_caracteristicas$grietas[data_caracteristicas$P4_25_1 == 1] <- "1"
data_caracteristicas$grietas[data_caracteristicas$P4_25_1 == 2] <- "0"
table(data_caracteristicas$grietas)

# Esta vivienda, ¿tiene problemas con levantamientos o hundimientos del piso?
data_caracteristicas$P4_25_3 <- as.numeric(data_caracteristicas$P4_25_3)
table(data_caracteristicas$P4_25_3)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_25_3 %in% c(1:2))
data_caracteristicas$hundimientos <- NA
data_caracteristicas$hundimientos[data_caracteristicas$P4_25_3 == 1] <- "1"
data_caracteristicas$hundimientos[data_caracteristicas$P4_25_3 == 2] <- "0"
table(data_caracteristicas$hundimientos)

# Esta vivienda, ¿tiene problemas con humedad o filtraciones de agua en cimientos, muros o techos?
data_caracteristicas$P4_25_4 <- as.numeric(data_caracteristicas$P4_25_4)
table(data_caracteristicas$P4_25_4)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_25_4 %in% c(1:2))
data_caracteristicas$humedad <- NA
data_caracteristicas$humedad[data_caracteristicas$P4_25_4 == 1] <- "1"
data_caracteristicas$humedad[data_caracteristicas$P4_25_4 == 2] <- "0"
table(data_caracteristicas$humedad)

# Esta vivienda, ¿tiene problemas con el sistema eléctrico (muros, techos, etcétera)?
data_caracteristicas$P4_25_6 <- as.numeric(data_caracteristicas$P4_25_6)
table(data_caracteristicas$P4_25_6)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_25_6 %in% c(1:2))
data_caracteristicas$sistema <- NA
data_caracteristicas$sistema[data_caracteristicas$P4_25_6 == 1] <- "1"
data_caracteristicas$sistema[data_caracteristicas$P4_25_6 == 2] <- "0"
table(data_caracteristicas$sistema)

# Esta vivienda, ¿tiene problemas con las tuberías de agua o drenaje dentro de la vivienda?
data_caracteristicas$P4_25_7 <- as.numeric(data_caracteristicas$P4_25_7)
table(data_caracteristicas$P4_25_7)
data_caracteristicas <- data_caracteristicas %>%
  filter(P4_25_7 %in% c(1:2))
data_caracteristicas$tuberias <- NA
data_caracteristicas$tuberias[data_caracteristicas$P4_25_7 == 1] <- "1"
data_caracteristicas$tuberias[data_caracteristicas$P4_25_7 == 2] <- "0"
table(data_caracteristicas$tuberias)


# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y el trabajo?
data_caracteristicas$P6_5_1 <- as.numeric(data_caracteristicas$P6_5_1)

# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y los centros escolares?
data_caracteristicas$P6_5_2 <- as.numeric(data_caracteristicas$P6_5_2)

# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y los centros de salud?
data_caracteristicas$P6_5_3 <- as.numeric(data_caracteristicas$P6_5_3)

# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y mercados o centros comerciales?
data_caracteristicas$P6_5_4 <- as.numeric(data_caracteristicas$P6_5_4)

# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y parques o espacios deportivos?
data_caracteristicas$P6_5_5 <- as.numeric(data_caracteristicas$P6_5_5)

# ¿Qué tan satisfechos están  con la “distancia-tiempo” entre esta vivienda y los centros de recreación o instalaciones culturales?
data_caracteristicas$P6_5_6 <- as.numeric(data_caracteristicas$P6_5_6)

# En su colonia o barrio (localidad), ¿qué tanto problema tienen con la falta de rampas (o elevadores) para personas con discapacidad?
data_caracteristicas$P6_9_1 <- as.numeric(data_caracteristicas$P6_9_1)
table(data_caracteristicas$P6_9_1)
data_caracteristicas <- data_caracteristicas %>%
  filter(P6_9_1 %in% c(1:4))
data_caracteristicas$rampas <- NA
data_caracteristicas$rampas[data_caracteristicas$P6_9_1 == 1] <- "1"
data_caracteristicas$rampas[data_caracteristicas$P6_9_1 == 2] <- "1"
data_caracteristicas$rampas[data_caracteristicas$P6_9_1 == 3] <- "0"
data_caracteristicas$rampas[data_caracteristicas$P6_9_1 == 4] <- "0"
table(data_caracteristicas$rampas)

#En su colonia o barrio (localidad), ¿qué tanto problema tienen con el exceso de ruido por vecinos o del exterior?
data_caracteristicas$P6_9_2 <- as.numeric(data_caracteristicas$P6_9_2)
data_caracteristicas <- data_caracteristicas %>%
  filter(P6_9_2 %in% c(1:4))
data_caracteristicas$ruido <- NA
data_caracteristicas$ruido[data_caracteristicas$P6_9_2 == 1] <- "1"
data_caracteristicas$ruido[data_caracteristicas$P6_9_2 == 2] <- "1"
data_caracteristicas$ruido[data_caracteristicas$P6_9_2 == 3] <- "0"
data_caracteristicas$ruido[data_caracteristicas$P6_9_2 == 4] <- "0"
table(data_caracteristicas$ruido)

# En su colonia o barrio (localidad), ¿qué tanto problema tienen con la basura tirada en las calles?
data_caracteristicas$P6_9_3 <- as.numeric(data_caracteristicas$P6_9_3)
data_caracteristicas <- data_caracteristicas %>%
  filter(P6_9_3 %in% c(1:4))
data_caracteristicas$basura <- NA
data_caracteristicas$basura[data_caracteristicas$P6_9_3 == 1] <- "1"
data_caracteristicas$basura[data_caracteristicas$P6_9_3 == 2] <- "1"
data_caracteristicas$basura[data_caracteristicas$P6_9_3 == 3] <- "0"
data_caracteristicas$basura[data_caracteristicas$P6_9_3 == 4] <- "0"
table(data_caracteristicas$basura)

# En su colonia o barrio (localidad), ¿qué tanto problema tienen con los robos o asaltos?
data_caracteristicas$P6_9_7 <- as.numeric(data_caracteristicas$P6_9_7)
data_caracteristicas <- data_caracteristicas %>%
  filter(P6_9_7 %in% c(1:4))
data_caracteristicas$robos <- NA
data_caracteristicas$robos[data_caracteristicas$P6_9_7 == 1] <- "1"
data_caracteristicas$robos[data_caracteristicas$P6_9_7 == 2] <- "1"
data_caracteristicas$robos[data_caracteristicas$P6_9_7 == 3] <- "0"
data_caracteristicas$robos[data_caracteristicas$P6_9_7 == 4] <- "0"
table(data_caracteristicas$robos)

## Por último convertimos ingreso a logaritmo
summary(data_caracteristicas$ing_mes)
data_caracteristicas <- data_caracteristicas %>%
  filter(ing_mes %in% c(40:392000))
data_caracteristicas$ing_mes <- as.numeric(data_caracteristicas$ing_mes)
data_caracteristicas$log_ing <- NA
data_caracteristicas$log_ing <- log(data_caracteristicas$ing_mes)
summary(data_caracteristicas$log_ing)

## SATISFACCION EN ESCALA
# En una escala del 0 al 10, dígame, ¿qué tan satisfechos están con la vivienda? 
data_caracteristicas$P6_8 <- as.numeric(data_caracteristicas$P6_8)

# Descriptivos
summary(data_caracteristicas$P6_8)
table(data_caracteristicas$P6_8)

# Quitamos datos missing values
satisfaccion <- data_caracteristicas %>%
  filter(P6_8 %in% c(0:10))

# Descriptivos
summary(satisfaccion$P6_8)

# Por NSE
tapply(satisfaccion$P6_8, satisfaccion$nse, summary)


# Numericas
satisfaccion_num <- as.data.frame(apply(satisfaccion, 2, as.numeric))  # Convertir todas las variables a numerico


## Modelo logit ordenado
# Cargamos libreria y removemos notacion cientifica
library(rms)
options(scipen = 999)
## Vinculamos base de datos
attach(satisfaccion_num)

Y <- cbind(P6_8)
X <- cbind(log_ing, P4_19_1, P4_21_1, lavadero, fregadero, tinaco, jardin, cochera, grietas, hundimientos, humedad, ruido, rampas, basura, robos)
Xvar <- c("log_ing", "P4_19_1", "P4_21_1", "lavadero", "fregadero", "tinaco", "jardin", "cochera", "grietas", "hundimientos", "humedad", "ruido", "rampas", "basura", "robos")


# Estadisticos descriptivos
summary(Y)
summary(X)
table(Y)

# Coeficientes del modelo logit ordenado
ddist<- datadist(Xvar)
options(datadist='ddist')

ologit<- lrm(Y ~ X, data=satisfaccion_num)
print(ologit)

# Pasar data.frame a xls
# write.csv(satisfaccion, "C:/Users/Naim/Desktop/envi_data_limpia.csv")


# log file 
# sink(file = "modelo_logit.txt")

# log close
# sink(file = NULL)
