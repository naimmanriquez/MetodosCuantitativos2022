## Metodos Cuantitativos en R ##
## Enero, 2022 ##
# Autor: Dr. Naim Manriquez Garcia

## Removemos notacion cientifica
options(scipen = 999)

## Caracteres usados en el idioma español
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

## Instalar librerias
install.packages("readxl") # Para importar datos o archivos desde excel
install.packages("foreign") # Para importar datos de otros formatos
install.packages("haven") # Para importar archivos desde stata o spss 
install.packages("tidyverse") # Para manipular datos
install.packages("dplyr") # Para manipular datos
install.packages("ggplot2") # Para graficas

# Recomendado: para consultar información de librerias y funciones
# https://www.rdocumentation.org/

## Cargar librerias

library(readxl)
library(foreign)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Otra alternativa para cargar librerias es con pacman 
# install.packages("pacman")
# library(pacman)
# p_load(readxl, foreign, haven, tidyverse, dplyr, ggplot2)

## Abrir archivo de Excel
pokemon <- read_excel("Pokemon.xlsx")

## Pasar archivo o guardar a formato rds.  
saveRDS(pokemon, file = "pokemon.rds")

# Y si queremos leer archivos rds.
base_pokemon <- readRDS("pokemon.rds")

## Leer archivos desde otros formatos
## dbf
## hogares <- read.dbf("TIC_2015_HOGARES.dbf")
## dta
## concentradohogar <- read_dta("concentradohogar.dta")
## Otros ejemplos pueden ser con read.spss

## Estadisticos descriptivos de la base de pokemon
# Media, mediana, minimo, máximo, primer y tercer cuantil
summary(pokemon$Attack)
# Desviacion estandar
sd(pokemon$Attack)
# Cuantiles
quantile(pokemon$Attack)

# Si estamos interesados en algunos tipos de pokemones
# Primero vemos los tipos de pokemones
table(pokemon$`Type 1`)

# Podemos usar tapply para ver la media por tipo de pokemon
tapply(pokemon$Attack, pokemon$`Type 1`, mean)

# Para conocer la defensa de los pokemones
tapply(pokemon$Defense, pokemon$`Type 1`, mean)

# Prop table para ver porcentajes de tipo por generacion
round(prop.table(table(pokemon$`Type 1`, pokemon$Generation), 1), 2)

# Prop table para ver porcentajes de generacion y tipos
round(prop.table(table(pokemon$`Type 1`, pokemon$Generation), 2), 2)

# es lo mismo que:
# round(prop.table(table(pokemon$Generation, pokemon$`Type 1`), 1), 2)

# Filtrar datos
# Quiero una nueva base con los pokemones de la primera generacion
pokemones_primera_generacion <- pokemon %>%    
  filter (Generation == "1")

## Filtramos por poder de pelea 

pokemones_fuertes <- pokemones_primera_generacion %>%
  filter(Attack %in% c(90:200))





