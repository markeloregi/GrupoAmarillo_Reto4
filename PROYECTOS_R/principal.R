#Principal - Reto 04 - Eroski - Grupo Amarillo

#Librerias
source('Librerias.R',encoding = "UTF-8")


#Preprocesamiento de datos

#Tratamiendo de num_ticket y reducción y creación de la matriz de datos
source('Funcion.R',encoding = "UTF-8")

source('Scripts_preprocesamiento/Preprocesamiento.R',encoding = "UTF-8")
#
rm(list=ls())

source('Scripts_preprocesamiento/Reducción.R',encoding = "UTF-8")
#
rm(list=ls())


#Análisis

#Análisis exploratorio
source('Análisis exploratorio.R', encoding = "UTF-8")  
#
rm(list=ls())

#Clustering
source('Clustering.R', encoding = "UTF-8")  
#
rm(list=ls())


#Objetivos
source('Scripts_objetivos/Objetivo 1.R',encoding = "UTF-8")
#
rm(list=ls())

source('Scripts_objetivos/Objetivo 2.R',encoding = "UTF-8")
#
rm(list=ls())

source('Scripts_objetivos/Objetivo 3.R',encoding = "UTF-8")
#
rm(list=ls())

source('Scripts_objetivos/Objetivo 4.R',encoding = "UTF-8")
#
rm(list=ls())


#Asignaturas

#Data Mining
source('Scripts_modelos/Modelos_Recomendacion_DMining.R',encoding = "UTF-8")
#
rm(list=ls())

#Visualizacion de Datos
source('Shiny/Visu.R',encoding = "UTF-8")
#
rm(list=ls())

source('VISUALIZACION/Visu.R',encoding = "UTF-8")
#
rm(list=ls())

