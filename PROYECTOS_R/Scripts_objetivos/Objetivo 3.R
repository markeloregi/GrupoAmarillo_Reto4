

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

#Tabla de frecuencias de las recomendaciones de los productos:
frecuencias<-table(unlist(productos_recomendados_nombres))
df_frecuencias<-as.data.frame(frecuencias)
colnames(df_frecuencias)<-c("Producto", "Recomendaciones")
df_frecuencias<-merge(df_frecuencias, productos)
df_frecuencias$Producto<-as.character(df_frecuencias$Producto)

#Cambiamos el nombre y quitamos la X para poder unir ambosa data frames
productos<-productos %>%
  rename(Producto = cod_est)
df_frecuencias$Producto<-str_remove_all(df_frecuencias$Producto, "X")

#Nos aseguramos que las columnas "Producto" son de tipo texto y sin espacios
df_frecuencias$Producto<-trimws(as.character(df_frecuencias$Producto))
productos$Producto<-trimws(as.character(productos$Producto))

df <- df_frecuencias %>%
  left_join(productos, by = "Producto")

df <- df %>%
  mutate(descripcion = descripcion.y)

df_grafico <- df %>%
  arrange(desc(Recomendaciones))

#GRAFICOS
grafico_recomendaciones <- ggplot(df_grafico, aes(x = reorder(descripcion, Recomendaciones), y = Recomendaciones)) +
  geom_bar(stat = "identity", fill = "#d31e17") +
  coord_flip() +
  labs(x = "Producto", y = "Número de recomendaciones") +
  theme_minimal()


tabla_recomendaciones <- data.frame(
  Cliente = clientes,
  Producto_Recomendado = unlist(productos_recomendados_nombres)
)

tabla<- datatable(tabla_recomendaciones, 
          options = list(pageLength = 10),
          caption = "Producto recomendado por cliente")

saveRDS(grafico_recomendaciones, "VISUALIZACION/graficos/grafico_recomendaciones.rds")
        

