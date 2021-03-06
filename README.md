# Metodos Cuantitativos 2022
Repositorio de materiales utilizados para manejo de datos cuantitativos, 2022. Este repositorio es con fines academicos y de base para aquellos que estan aprendiendo a usar RStudio para análisis de datos. 

Algunos libros recomendados para iniciar:

* [Cookbook for R](http://www.cookbook-r.com/)
* [R for Data Science](https://r4ds.had.co.nz/)

Para visualización de datos:

* [Data Visualization with R](https://rkabacoff.github.io/datavis/)

Para mapas y gráficos:

* [Geocomputation with R](https://geocompr.robinlovelace.net/index.html)
* [Elegant Graphics for Data Analysis](https://ggplot2-book.org/)

## Introducción a R y manejo de datos.
Repaso de RStudio y tidyverse. Conceptos, forma de trabajo, operadores, filosofía, funciones, pipe, etc. 

## Limpieza de bases de datos

En esta sesión se revisa como limpiar bases de datos, filtrar y seleccionar variables clave. 

## Visualización, gráficos y presentación de información

En esta sesión se revisa la forma de crear visualizaciones a través de gráficos, se utilizaran las librerias de ggplot2, ggthemes, plotly, RColorBrewer, viridis. 

## Mapas y georreferenciación

En esta sesion veremos como generar mapas a partir de puntos, líneas y polígonos. Particularmente se verá como crear un mapa estático (para impresión) a partir de la librería de visualización ggplot2, repasando desde la apertura de datos geográficos (con la librería sf), la unión (con merge() y *_join()) hasta la creación de mapas, la edición del tema y la impresión en un archivo *.png. 

## Regresión lineal, múltiple, probit, logit

Introducción a la creación e inspección de los modelos de regresión lineal en R. Revisamos la función lm() para generar modelos, la interpretación de los coeficientes, del valor p, la hipótesis que prueban esos valores p, una introducción al R2 y un ejemplo de inferencia respecto a los estimadores

## Mineria de datos

En esta sesión se hará el ejercicio de descargar múltiples archivos a través de loops for automatizados. Se realizaron 5 ejemplos durante la clase, todos de descarga masiva de datos del INEGI. Veremos una introducción al web scrapping. 

## Encuestas

En esta sesión tendremos una introducción a las bases de datos de encuestas de INEGI, abordaremos un análisis inicial de la ENIGH 2020. Durante esta sesión repasaremos:

* Una introducción a la Encuesta Nacional de Ingresos y Gastos de los Hogares - [ENIGH 2020](https://www.inegi.org.mx/programas/enigh/nc/2020/)
* ¿Qué son los factores de expansión?
* Manejo de datos de encuestas con survey
* Tablas de frecuencias
* Estadísticos descriptivos

## Machine Learning

En esta sesión veremos las ideas básicas para análisis predictivo desde el punto de vista de machine learning, incluyendo la construcción de modelos y su validación predictiva. Se entenderán los conceptos que hay detrás de técnicas probadas de machine learning.

## Análisis de Componentes Principales

Análisis de componentes principales es una técnica estadística que su principal objetivo es simplificar la complejidad dimensional de un conjunto de datos; asegurando que se conserve la información de conjunto de datos. Es decir, este método de componentes principales condensa la información aportada por múltiples variables en tan sólo pocas variables (componentes) combinando las técnicas de álgebra lineal y estadística.

## Series de Tiempo

En esta sesión veremos la aplicación de los modelos de series de tiempo en el ámbito de las ciencias sociales. Estudiaremos los modelos de series de tiempo de una variable (ARIMA y de la familia GARCH) para la estimación, pronóstico y simulación del comportamiento de variables a través del tiempo.
