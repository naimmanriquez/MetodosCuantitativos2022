# Capas shp
## https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463807469


# Abrimos las paqueterías con un sólo comando:
library(pacman)

p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr,gmodels, foreign, RColorBrewer)


##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
library("colorspace")
#hcl_palettes(plot = TRUE)

##############
#Abrir capas para mapas
##############

##############
#AGEBs
ageb_sin <- st_read("25a.shp")

#Crear variable para filtrar por municipios de interés
ageb_sin$mun <- substr(ageb_sin$CVEGEO, 3, 5)

#Filtrar 
ageb_mazatlan <- ageb_sin %>%    
  filter (mun == "012")

## Mapa Mazatlan
plot(ageb_mazatlan)


## Datos censo
RESAGEB20 <- read_excel("RESAGEB20.xlsx")

# Filtrar por municipio
ageb_datos <- RESAGEB20 %>%    
  filter (MUN == "012")

ageb_urbanas <- ageb_datos %>% 
  filter (NOM_LOC == "Total AGEB urbana")

ageb_urbanas <- rename(ageb_urbanas, CVE_AGEB = AGEB)


# Union de bases

bd_final <- left_join(ageb_mazatlan, ageb_urbanas)

bd_final <- bd_final %>%
  mutate_at("POBTOT", ~as.numeric(.)) 

mapa <- ggplot(bd_final) +
  geom_sf(aes(fill = POBTOT)) +
  ggtitle("Población total") +
  scale_fill_gradientn(colors=brewer.pal(name="Oranges", n=6)) + theme_void()

mapa

mapa_final <- mapa + 
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(1000, 3000, 5000))

mapa_final


# Mapa final con tema

library(extrafont)

theme_map <- function(...) {
  theme_void() +
    theme(
      plot.title = element_text(size = 20, face = "bold",
                                hjust=0.5,family="Arial Narrow",color="gray35"),
      plot.subtitle = element_text(size = 14,hjust=0.5,
                                   family="Arial Narrow",color="gray35"),
      plot.caption = element_text(size=10,
                                  family="Arial Narrow",color="gray35"),
      strip.text.x = element_text(size=14,hjust=0.1,vjust=0, face = "bold",
                                  family="Arial Narrow",color="gray35"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      plot.margin = margin(0.8, 0.5, 0.5, 0.5, "cm"),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      legend.position = 'bottom',
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.title = element_text(family="Arial Narrow",color="gray35"),
      legend.text = element_text(family="Arial Narrow",color="gray35"),
      legend.key = element_rect()
    )}


mapa2 <- ggplot(bd_final) +
  geom_sf(aes(fill = POBTOT)) +
  labs(title = 'Población por AGEB, 2020',
       subtitle = "Población en la Ciudad de Mazatlán por Área Geoestadistica Base.",
       caption='Censo de Población y Vivienda - INEGI || data.viz.nm',
       x = NULL,
       y = NULL) +
  scale_fill_gradientn(colors=brewer.pal(name="Oranges", n=6)) + theme_void()

mapa2 + theme_map() +
  guides(fill = guide_colourbar(direction = 'horizontal',  ## transform legend
                                title='Cantidad de población',  ##rename default legend
                                title.position='top',
                                title.hjust=0.5,
                                ticks.colour='#f5f5f2',
                                ticks.linewidth=2,
                                barwidth = 30,
                                barheight = 0.5))

ggsave("poblacion_mazatlan.png", width = 10, height = 7)

## mas opciones
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

plot(bd_final["POBTOT"], 
     main = "Poblacion total", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)




# mas con leaflet
# https://rstudio.github.io/leaflet/
# Leaflet es una librería utilizada para la publicación de mapas en la web.
  
library(leaflet) 

# reproject
mazatlan_leaflet <- st_transform(bd_final, 4326)

leaflet(mazatlan_leaflet) %>%
  addPolygons()

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

p_popup <- paste0("<strong>Poblacion: </strong>", bd_final$POBTOT)

leaflet(mazatlan_leaflet) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(POBTOT), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
    popup = p_popup)  # add popup

leaflet(mazatlan_leaflet) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(POBTOT),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles()

leaflet(mazatlan_leaflet) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(POBTOT),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addProviderTiles(providers$Stamen.Toner)
  addTiles()

leaflet(mazatlan_leaflet) %>%
  addPolygons(
      stroke = FALSE, 
      fillColor = ~pal_fun(POBTOT),
      fillOpacity = 0.8, smoothFactor = 0.5,
      popup = p_popup) %>%
  addProviderTiles(providers$CartoDB.Positron)
  addTiles()
  
leaflet(mazatlan_leaflet) %>%
  addPolygons(
      stroke = FALSE, 
      fillColor = ~pal_fun(POBTOT),
      fillOpacity = 0.8, smoothFactor = 0.5,
      popup = p_popup) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap)
  addTiles()

  
m <- leaflet(mazatlan_leaflet) %>%
    addPolygons(
      stroke = FALSE, 
      fillColor = ~pal_fun(POBTOT),
      fillOpacity = 0.8, smoothFactor = 0.5,
      popup = p_popup) %>%
    addTiles()

m <- addMarkers(m, lng=-106.43, lat=23.23, popup="Imperia Beach Tower")


