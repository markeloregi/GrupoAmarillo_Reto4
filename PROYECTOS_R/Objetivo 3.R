#OBJETIVO 3: OFERTA PARA TI
#Cargar librerias necesarias:
library(tidyverse)
library(recommenderlab)
library(rsparse)
library(Matrix)
library(readxl)
library(readr)
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(stringr)

#Cargar los datos
matriz_datos<-read.csv("Datos\\Transformados\\Matriz_Datos.csv")
objetivos<-readRDS("Datos\\Originales\\objetivos.RDS")
productos<-readRDS("Datos\\Originales\\maestroestr.RDS")

objetivos[[3]]
productos_oferta<-objetivos[[3]]$obj


#Ahora convierte la matriz en una matriz dispersa de tipo dgCMatrix (Compressed Sparse Column)
matriz_numerica<-as.matrix(matriz_datos[,-1])
matriz_dispersa<-Matrix(matriz_numerica, sparse = TRUE)

#Modelo WRF
modelo_wrmf<-WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf$fit_transform(matriz_dispersa, n_iter = 1000L, convergence_tol = 0.000001)


#Añadir el prefijo 'X' a los productos de oferta
productos_oferta<-paste0("X", productos_oferta)
productos_oferta[!productos_oferta %in% colnames(matriz_dispersa)]

#Todos los productos (columnas de la matriz)
productos_totales<-colnames(matriz_dispersa)

#Excluimos todos los productos que NO estan en la oferta
productos_a_excluir<-setdiff(productos_totales, productos_oferta)
preds<-modelo_wrmf$predict(matriz_dispersa, k = 1, items_exclude = productos_a_excluir)
preds
attr(preds, 'ids')

#Convertir la salida en una lista para ver las recomendaciones
recomendaciones<-as(preds, "list")

#Ver las primeras 5 recomendaciones para ver cómo se ve
head(recomendaciones, 5)

#Convertimos los índices de las recomendaciones a nombres de productos
productos_recomendados_nombres<-sapply(recomendaciones, function(x) colnames(matriz_dispersa)[x])

#Ver las recomendaciones con el nombre de producto para los primeros usuarios
head(productos_recomendados_nombres, 5)

#Crear el data.frame con cliente y producto recomendado
clientes<-matriz_datos[[1]]
productos_recomendados_nombres
recomendaciones<-data.frame(merge(clientes, productos))


#PRODUCTOS:
#Plátano de Canarias: 231 (01012310)
#Mandarinas Postre: 3569 (01013315)
#Calabacines: 641 (01026410)
#Puerros: 3401 (01027205)
#Zanahorias: 1888 (01027405)
#Cebollas Amarillas: 146 (01201005)
#Patatas Todo Uso: 1482 (01201505)
#Jamón de Cerdo: 143 (04200505)
#Pechuga de pavo: 556 (04201005)
#Leche UHT Entera: 3822 (05030101)
#Leche UHT Semidesnatada: 2923 (05030102)
#Entero Natural: 4273 (05040180)
#Entero de Sabores: 3 (05040181)
#Tomate Frito Liso Brik Pack: 2718 (08100903)
#Atún Claro en Aceite de Oliva: 544 (08230125)
#Arroz Redondo: 2726 (09070103)
#Agua de Mesa: 2059 (11040303)
#P.H. SECO BASICO 2C NORMAL: 158 (12650101)
#P.H. SECO BASICO 2C LARGO: 1618 (12650103)
#Rollo Cocina Largo: 442 (12670111)


#Tabla de frecuencias de las recomendaciones de los productos:
frecuencias<-table(unlist(productos_recomendados_nombres))
df_recomendaciones<-as.data.frame(frecuencias)
colnames(df_recomendaciones)<-c("Producto", "Recomendaciones")


df_recomendaciones$Producto<-as.character(df_recomendaciones$Producto)
maestro$Producto<-as.character(maestro$Producto)

#Cambiamos el nombre y quitamos la X para poder unir ambosa data frames
maestro<-maestro %>%
  rename(Producto = cod_est)
df_recomendaciones$Producto<-str_remove_all(df_recomendaciones$Producto, "X")

#Nos aseguramos que las columnas "Producto" son de tipo texto y sin espacios
df_recomendaciones$Producto<-trimws(as.character(df_recomendaciones$Producto))
maestro$Producto<-trimws(as.character(maestro$Producto))

#Ahora unimos por "Producto"
df<-df_recomendaciones %>%
  left_join(maestro, by = "Producto")


#GRÁFICOS
df_grafico<-df %>%
  arrange(desc(Recomendaciones))

grafico_recomendaciones<-ggplot(df, aes(x = reorder(descripcion, Recomendaciones), y = Recomendaciones), fill = descripcion) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 productos más recomendados",
       x = "Producto",
       y = "Número de recomendaciones") +
  theme_minimal()

tabla_recomendaciones <- data.frame(
  Cliente = clientes,
  Producto_Recomendado = unlist(productos_recomendados_nombres)
)

datatable(tabla_recomendaciones, 
          options = list(pageLength = 10),
          caption = "Producto recomendado por cliente")
