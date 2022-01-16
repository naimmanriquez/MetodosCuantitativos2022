## Metodos Cuantitativos en R ##
## Enero, 2022 ##
# Autor: Dr. Naim Manriquez Garcia

## Removemos notacion cientifica
options(scipen = 999)

## Caracteres usados en el idioma español
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

# Cargar librerias
library(tidyverse)
library(dplyr)

# Datos
## https://www.inegi.org.mx/app/preciospromedio/?bs=18

precios_limon <- read_excel("precios_limon.xlsx")

table(precios_limon$Nombre_ciudad)

## Convertir a numerico
precios_limon$Precio_promedio <- as.numeric(precios_limon$Precio_promedio)


## GUADALAJARA
gdl <- precios_limon %>%    
  filter(str_detect(Nombre_ciudad, regex("Guadalajara, Jal.", ignore_case = TRUE))) %>% 
  filter (Año == "2021")

## Precios 
gdl_precios <- data.frame(tapply(gdl$Precio_promedio, gdl$Mes, mean)) 
gdl_precios <- rename(gdl_precios, gdl_promedio = tapply.gdl.Precio_promedio..gdl.Mes..mean.)

## CULIACAN
cln <- precios_limon %>%    
  filter(str_detect(Nombre_ciudad, regex("Culiacán, Sin.", ignore_case = TRUE))) %>% 
  filter (Año == "2021")

## Precios 
cln_precios <- data.frame(tapply(cln$Precio_promedio, cln$Mes, mean)) 
cln_precios <- rename(cln_precios, cln_promedio = tapply.cln.Precio_promedio..cln.Mes..mean.)

## MONTERREY
mty <- precios_limon %>%    
  filter(str_detect(Nombre_ciudad, regex("Monterrey, N.L.", ignore_case = TRUE))) %>% 
  filter (Año == "2021")

## Precios 
mty_precios <- data.frame(tapply(mty$Precio_promedio, mty$Mes, mean)) 
mty_precios <- rename(mty_precios, mty_promedio = tapply.mty.Precio_promedio..mty.Mes..mean.)

## OAXACA
oax <- precios_limon %>%    
  filter(str_detect(Nombre_ciudad, regex("Oaxaca, Oax.", ignore_case = TRUE))) %>% 
  filter (Año == "2021")

## Precios 
oax_precios <- data.frame(tapply(oax$Precio_promedio, oax$Mes, mean)) 
oax_precios <- rename(oax_precios, oax_promedio = tapply.oax.Precio_promedio..oax.Mes..mean.)


# Unir data
ciudades <- data.frame(cln_precios, gdl_precios, mty_precios, oax_precios)

# Crear mes
ciudades$meses = NA

ciudades$meses = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Wrangled
wrangled_data <- data.frame(Mes = ciudades$meses, stack(ciudades[1:4]))
# Nombre
names(wrangled_data)[2] <- "Precio"
names(wrangled_data)[3] <- "Ciudad"

# Checamos los datos
head(wrangled_data)

wrangled_data <- wrangled_data %>%
  mutate(Ciudad = factor(Ciudad, levels = c("cln_promedio", "gdl_promedio", "mty_promedio", "oax_promedio"), 
                          labels = c("Culiacán", "Guadalajara", "Monterrey", "Oaxaca")))

# Grafica de precio promedio por ciudad
ggplot(data = wrangled_data, aes(x = Mes, y = Precio)) +
  geom_point(color = "steelblue") +
  facet_wrap(. ~ Ciudad) +
  labs(title = "¿Demasiado caro el limón?",
       subtitle = "Precio promedio mensual por kg, 2021",
       caption = "Consulta en línea de los precios promedio del INPC, INEGI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 18)) +
  theme(text = element_text(size=12))

# Otra
ggplot(data = wrangled_data, aes(x = Mes, y = Precio)) +
  geom_point(color = "steelblue") +
  facet_grid(Ciudad ~ .) +
  labs(title = "¿Demasiado caro el limón?",
       subtitle = "Precio promedio mensual por kg, 2021",
       caption = "Consulta en línea de los precios promedio del INPC, INEGI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 18)) +
  theme(text = element_text(size=9))




