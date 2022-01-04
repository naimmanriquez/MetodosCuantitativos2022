## Limpieza da datos
# Dr. Naim Manriquez

# Instalar librerias
install.packages("tidyverse")
install.packages("dplyr")
install.packages("reshape")
install.packages("data.table")

# Cargamos librerias
library(tidyverse) # Manejo de datos
library(dplyr) # Manejo de datos
library(reshape) # Es util para reestructurar datos
library(data.table) # cargar archivos tabulares, o de texto. 
library(viridis)
library(haven)

# Descarga archivo de gastos de los hogares

# activity_url <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_gastoshogar_csv.zip"
# temp <- tempfile()
# download.file(activity_url, temp)
# unzip(temp, "gastoshogar.csv")
# gastoshogar <- read.csv("gastoshogar.csv")
# unlink(temp)

# Archivo de concentrado hogar

concentradohogar <- read_dta("concentradohogar.dta") 

# concentradohogar <- readRDS("concentradohogar.rds") # Por si lo cargan desde Stata

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(concentradohogar)
glimpse(concentradohogar)

# Para una sola variable
str(concentradohogar$sexo_jefe)

# Nombre de las variables
names(concentradohogar)

# Detectar valores perdidos
any(is.na(concentradohogar))

# Ejemplo
# expl_vec1 <- c(4, 8, 12, NA, 99, - 20, NA) # Ejemplo con vector con NA's
# is.na(expl_vec1)
# which(is.na(expl_vec1))

# Ejemplo 2

# expl_data1 <- data.frame(x1 = c(NA, 7, 8, 9, 3), 
#                          x2 = c(4, 1, NA, NA, 4), 
#                          x3 = c(1, 4, 2, 9, 6), 
#                          x4 = c("Hello", "I am not NA", NA, "I love R", NA)) 
# 
# which(is.na(expl_data1$x1))
# which(is.na(expl_data1$x2))
# which(is.na(expl_data1$x3))


# Detectar valores perdidos
any(is.na(gastoshogar))
which(is.na(gastoshogar$gasto_tri))

## Primero concentrarnos en algunos gastos
## Generando los datos para bebidas alcoholicas
# A224 Cerveza
# A229 Aguardiente, alcohol de caña, charanda, mezcal
# A233 Tequila añejo, azul y blanco
# A234 Vino

datos_bebidas <- gastoshogar %>% 
  filter(clave %in% c("A224", "A229", "A233", "A234"))

# seleccionar solo variables de interes: folioviv, gasto
base_bebidas_limpia <- datos_bebidas %>%
  select(folioviv, clave, gasto_tri)

## Funcion reshape para estructurar los datos

bd_bebidas <- cast(base_bebidas_limpia, folioviv ~ clave, sum, value = 'gasto_tri')

# Filtramos y quitamos NA
## Eliminamos datos faltantes
base_limpia_vf <- na.omit(bd_bebidas)

## Union de bases
base_unida <- merge(x = concentradohogar, y = base_limpia_vf, all.x = TRUE)


## Eliminamos datos faltantes
bd_unida_limpia <- na.omit(base_unida)

## Pasar a excel

write.csv(bd_unida_limpia, "C:/Users/Naim/Desktop/bd_beidas_VF.csv")


# Grafica
# Cambiamos nombre a valores de NSE
bd_unida_limpia$est_socio<- factor(bd_unida_limpia$est_socio,label= c("Bajo", "Medio bajo", "Medio alto", "Alto"))

# Crear nueva variable mensual
bd_unida_limpia['gasto_mensual'] = bd_unida_limpia['A233'] / 3

## Filtrar outliers
## nueva_base <- bd_unida_limpia %>% filter(gasto_mensual < 6000)
## nueva_base_2 <- nueva_base %>% filter(gasto_mensual > 0)


ggplot(bd_unida_limpia, aes(x=est_socio, y=gasto_mensual)) + 
  geom_jitter()

# ggplot(nueva_base, aes(x=est_socio, y=gasto_mensual)) + 
#  geom_jitter()

ggplot(nueva_base_2, aes(x=est_socio, y=gasto_mensual)) + 
  geom_jitter()

p <-ggplot(bd_unida_limpia, aes(x=est_socio, y=A233, color=est_socio)) +
  geom_jitter(position=position_jitter(0.2))

p

# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_minimal()

p + scale_color_viridis_d() +
    labs(x = "Nivel Socioeconómico",
         title = "Gasto en tequila añejo, azul y blanco por NSE",
         caption = "Fuente: Encuesta Nacional de Ingresos y Gastos de los Hogares, 2020",
         y = "Gasto trimestral",
         color = "NSE") +
  theme_light(base_size = 12) +
  theme(panel.grid.minor = element_blank())

