#Cargar los datos
maestro<-readRDS("Datos\\Originales\\maestroestr.RDS")
tickets<-readRDS("Datos\\Originales\\tickets_enc.RDS")


#Data discovering
str(maestro)
str(tickets)
duplicados <- tickets %>%
  group_by(num_ticket) %>%
  summarise(duplicados = sum(duplicated(id_cliente_enc)))
#Duplicados
sum(duplicados$duplicados)
unique(tickets$num_ticket)

duplicados_prueba <- tickets %>%
  group_by(num_ticket) %>%
  summarise(clientes_unicos = n_distinct(id_cliente_enc)) %>% 
  filter(clientes_unicos > 1)

crear_nuevo_numero_ticket <- function(data) {
  data_nueva<<-data %>%
    mutate(grupo = paste(num_ticket, id_cliente_enc, sep = "_")) %>%
    mutate(nuevo_num_ticket = as.integer(factor(grupo))) %>%
    select(-grupo)
  return(data_nueva)
}
crear_nuevo_numero_ticket(tickets)

#Cambiamos el formato de la columna dia a tipo fecha
data_nueva$dia<-ymd(data_nueva$dia)

#1.1. Calcular productos distintos por ticket por cliente
productos_por_ticket <- data_nueva %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(productos_por_ticket = n_distinct(cod_est), .groups = "drop")

#1.2. Calcular variables resumen por cliente
resumen_clientes <- productos_por_ticket %>%
  group_by(id_cliente_enc) %>%
  summarise(
    media_productos_por_ticket = mean(productos_por_ticket)
  )

#1.3. Otra tabla con frecuencia de visitas y número total de tickets
resumen_clientes2 <- data_nueva %>%
  group_by(id_cliente_enc) %>%
  summarise(
    frecuencia_visitas = n_distinct(dia),
    tickets_totales = n()
  )

#Unir los dos resúmenes,
resumen_final <- left_join(resumen_clientes, resumen_clientes2, by = "id_cliente_enc")

#Escalamos los datos para no sesgar el clustering
#Escalar todas las columnas excepto la primera (por ejemplo, ID)
datos_escalados <- data.frame(resumen_final[, 1, drop = FALSE],
                              scale(resumen_final[, -1]))

#Finalmente, añadir las columnas a data_nueva
data_nueva <- left_join(data_nueva, datos_escalados, by = "id_cliente_enc")

#--------------------------
#1. Clustering jerárquico
#--------------------------
#Hacemos la matriz de distancias con todas las variables menos el id_cliente
matriz_euclidea<-dist(datos_escalados[,-1], method = "euclidean")
matriz_manhattan<-dist(datos_escalados[,-1], method = "manhattan")

#Algoritmo de clustering jerárquico con distancia euclidea
clustering_jerarquico_euc<-hclust(matriz_euclidea, method = "ward.D")

#Paleta de colores corporativos de Eroski
eroski_rojo <- "#d31e17"
eroski_azul <- "#194586"
eroski_blanco <- "#ffffff"
eroski_negro <- "#000000"
eroski_colores <- c("1" = eroski_rojo, "2" = "#000000", "3" = eroski_azul)
#Dendrograma de culstering jerárquico con distancia euclidea
dendrograma_jerarquico_euc<-as.dendrogram(clustering_jerarquico_euc)
dendrograma_jerarquico_euc<-color_branches(dendrograma_jerarquico_euc, k = 3, eroski_colores)

#Visualización del dendrograma con plot
ggdendro_data_euc<-dendro_data(dendrograma_jerarquico_euc)
ggplot_dendro_euc<-ggplot(segment(ggdendro_data_euc)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               color = eroski_colores[1]) +
  theme_minimal() +
  labs(x = "Empresas", y = "Distancia")
plotly_dendro_euc<-ggplotly(ggplot_dendro_euc)
plotly_dendro_euc
saveRDS(plotly_dendro_euc, "VISUALIZACION/graficos/plotly_dendro_euc.rds")

#Ponemos al azar como número de clusters 7
numero_clusters_jerarquico_euc<-cutree(clustering_jerarquico_euc, k = 7)
table(numero_clusters_jerarquico_euc)

#Método del codo para determinar el mejor número de clusters en el clustering jerárquico y k-medians
#Saber cual es el valor de "k" mediante el metodo del codo
resultados<-1:10
for (k in 1:10){
  model<- kmeans(x = resumen_final[,-1], centers = k)
  model$tot.withinss
  resultados[k]<-model$tot.withinss
}
#Gráfico del Método del Codo
metodo_codo<- plot(1:10, resultados, type = "b", pch = 19, col = eroski_colores,
     xlab = "Número de Clusters (k)", 
     ylab = "Suma de cuadrados dentro del cluster")
metodo_codo
saveRDS(metodo_codo, "VISUALIZACION/graficos/metodo_codo.rds")

#Tras hacer el método del codo, hemos llegado a la conclusión de que el mejor número de clusters en 2021 y 2022 es 3 y en 2023 4
clusters_codo_euc<-cutree(clustering_jerarquico_euc, k = 3)
table(clusters_codo_euc)

#Evaluar la calidad del clustering jerárquico usando el índice de silueta
silueta_jerarquico_euc <- silhouette(clusters_codo_euc, matriz_euclidea)
mean_sil_jerarquico_euc <- mean(silueta_jerarquico_euc[,3])
cat("Índice de silueta medio para clustering jerárquico con distancia euclidea:", mean_sil_jerarquico_euc, "\n")

#Sacamos los centroides del clustering jerárquico
centroides_clustering_euc<-aggregate(datos_escalados[,-1], by = list(cluster = clusters_codo_euc), FUN = mean)


#Algoritmo de clustering jerárquico con distancia canberra
clustering_jerarquico_man<-hclust(matriz_manhattan, method = "ward.D")

#Dendrograma de culstering jerárquico con distancia euclidea
dendrograma_jerarquico_man<-as.dendrogram(clustering_jerarquico_man)
dendrograma_jerarquico_man<-color_branches(dendrograma_jerarquico_man, k = 3)

#Visualización del dendrograma con plot
ggdendro_data_man<-dendro_data(dendrograma_jerarquico_man)
ggplot_dendro_man<-ggplot(segment(ggdendro_data_man)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               color = eroski_colores[2]) +
  theme_minimal() +
  labs(x = "Empresas", y = "Distancia")
plotly_dendro_man<-ggplotly(ggplot_dendro_man)
plotly_dendro_man
saveRDS(plotly_dendro_man, "VISUALIZACION/graficos/plotly_dendro_man.rds")

#Ponemos al azar como número de clusters 7
numero_clusters_jerarquico_man<-cutree(clustering_jerarquico_man, k = 7)
table(numero_clusters_jerarquico_man)

#Tras hacer el método del codo, hemos llegado a la conclusión de que el mejor número de clusters es 3
clusters_codo_man<-cutree(clustering_jerarquico_man, k = 3)
table(clusters_codo_man)

#Evaluar la calidad del clustering jerárquico usando el índice de silueta
silueta_jerarquico_man <- silhouette(clusters_codo_man, matriz_manhattan)
mean_sil_jerarquico_man <- mean(silueta_jerarquico_man[,3])
cat("Índice de silueta medio para clustering jerárquico con distancia manhattan:", mean_sil_jerarquico_man, "\n")

#Sacamos los centroides del clustering jerárquico
centroides_clustering_man<-aggregate(datos_escalados[,-1], by = list(cluster = clusters_codo_man), FUN = mean)

#------------------
#2. K-MEANS
#------------------
set.seed(123)
#Algoritmo de clustering K-means con distancia euclidea
kmeans_euc<-kmeans(datos_escalados[,-1], centers = 3, nstart = 25)
table(kmeans_euc$cluster)

#Evaluar la calidad del clustering de k-means usando el índice de silueta
silueta_kmeans_euc <- silhouette(kmeans_euc$cluster, matriz_euclidea)
mean_sil_kmeans_euc <- mean(silueta_kmeans_euc[,3])
cat("Índice de silueta medio para K-means con distancia euclidea", mean_sil_kmeans_euc, "\n")

#Centroides de K-means con distancia euclidea
kmeans_euc$centers

#Algoritmo de clustering K-means con distancia manhattan (es el mismo que con la euclidea)
kmeans_man<-kmeans(datos_escalados[,-1], centers = 3, nstart = 25)
table(kmeans_man$cluster)

#Evaluar la calidad del clustering de k-means usando el índice de silueta
silueta_kmeans_man <- silhouette(kmeans_man$cluster, matriz_manhattan)
mean_sil_kmeans_man <- mean(silueta_kmeans_man[,3])
cat("Índice de silueta medio para K-means con ditancia manhattan", mean_sil_kmeans_man, "\n")
kmeans_man$centers

#Al comparar los clusterings, se ha visto mediante "La Silueta" que el K-MEANS 
#con la distancia euclidea es el más eficiente
#Unimos los clusters con los datos
resumen_final<-resumen_final %>%
  mutate(Clusters = kmeans_euc$cluster)

datos_escalados<-datos_escalados %>%
  mutate(Clusters = kmeans_euc$cluster)
write_rds(datos_escalados, "Scripts_modelos/clusters.RDS")


#GRÁFICOS
#Visualizar los clusters del Clustering K-Means con la
#Crear un dataframe para plotly
#Crear dataframe con las primeras dos componentes principales
#Hacemos PCA a los datos escalados (quitando el ID)
 
#Asumiendo que ya tenemos estos resultados:
#clusters_codo_euc, clusters_codo_man, kmeans_euc$cluster, kmeans_man$cluster

#Primero hacemos el PCA para reducir a 2 dimensiones
pca_result <- prcomp(datos_escalados[,-1], scale. = FALSE)
pca_df <- as.data.frame(pca_result$x[,1:2])
colnames(pca_df) <- c("PC1", "PC2")

# Añadimos las etiquetas de clustering
pca_df$Jerarquico_Euclidea <- as.factor(clusters_codo_euc)
pca_df$Jerarquico_Manhattan <- as.factor(clusters_codo_man)
pca_df$KMeans_Euclidea <- as.factor(kmeans_euc$cluster)
pca_df$KMeans_Manhattan <- as.factor(kmeans_man$cluster)

# Reorganizamos los datos en formato largo
pca_long <- pivot_longer(
  pca_df,
  cols = starts_with("Jera") | starts_with("KMeans"),
  names_to = "Metodo",
  values_to = "Cluster"
)

# --- Gráfico 1: Jerárquico con distancia Euclídea ---
plot_jer_euc <- plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2,
  color = ~Jerarquico_Euclidea,
  colors = eroski_colores,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Clustering Jerárquico (Distancia Euclídea)",
    xaxis = list(title = "Componente Principal 1"),
    yaxis = list(title = "Componente Principal 2")
  )

# --- Gráfico 2: Jerárquico con distancia Manhattan ---
plot_jer_man <- plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2,
  color = ~Jerarquico_Manhattan,
  colors = eroski_colores,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Clustering Jerárquico (Distancia Manhattan)",
    xaxis = list(title = "Componente Principal 1"),
    yaxis = list(title = "Componente Principal 2")
  )

# --- Gráfico 3: K-Means con distancia Euclídea ---
plot_kmeans_euc <- plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2,
  color = ~KMeans_Euclidea,
  colors = eroski_colores,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    xaxis = list(title = "Componente Principal 1"),
    yaxis = list(title = "Componente Principal 2"),
    plot_bgcolor = "#f0f0f0",
    paper_bgcolor = "#f0f0f0"   
  )

# --- Gráfico 4: K-Means con distancia Manhattan ---
plot_kmeans_man <- plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2,
  color = ~KMeans_Manhattan,
  colors = "Pastel1",
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Clustering K-Means (Distancia Manhattan)",
    xaxis = list(title = "Componente Principal 1"),
    yaxis = list(title = "Componente Principal 2")
  )


#GRÁFICO 3D DEL CLUSTERING KMEANS CON DISTANCIA EUCLÍDEA
#Hacemos el PCA con tres componentes principales
pca_3d <- prcomp(datos_escalados[,-1], center = TRUE, scale. = TRUE)
pca_3d_df <- as.data.frame(pca_3d$x[, 1:3])
colnames(pca_3d_df) <- c("PC1", "PC2", "PC3")

# Añadimos el cluster de KMeans con distancia euclídea
pca_3d_df$Cluster <- as.factor(kmeans_euc$cluster)
pca_3d_df$Empresa <- datos_escalados[, 1]  # si quieres mostrar nombre

# Gráfico 3D con plotly
plot_3d <- plot_ly(
  data = pca_3d_df,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~Cluster,
  colors = eroski_colores,
  type = "scatter3d",
  mode = "markers",
  text = ~paste("Empresa:", Empresa),
  marker = list(size = 5)
) %>%
  layout(
    title = "Clustering K-Means (Distancia Euclídea) en 3D",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

plot_3d
saveRDS(plot_3d, "VISUALIZACION/graficos/plot_3d.rds")

# Mostrar los 4 gráficos
plot_jer_euc
plot_jer_man
plot_kmeans_euc
plot_kmeans_man

table(clusters_codo_euc)
table(clusters_codo_man)
table(kmeans_euc$cluster)
table(kmeans_man$cluster)


#Gráficos para ver la diferencia entre clústeres
resumen_final$Clusters <- as.factor(resumen_final$Clusters)

#Boxplots de cada variable por clúster
# Media productos por ticket

productos_clusters<-ggplot(resumen_final, aes(x = Clusters, y = media_productos_por_ticket, fill = Clusters)) +
  geom_boxplot() +
  labs(title = "Media de productos por ticket por Clúster", y = "Media de productos", x = "Clúster") +
  theme_minimal()
ggplotly(productos_clusters)

# Frecuencia de visitas
frecuencia_cluster<-ggplot(resumen_final, aes(x = Clusters, y = frecuencia_visitas, fill = Clusters)) +
  geom_boxplot() +
  labs(title = "Frecuencia de visitas por Clúster", y = "Frecuencia de visitas", x = "Clúster") +
  theme_minimal()
ggplotly(frecuencia_cluster)

# Tickets totales
tickets_cluster<-ggplot(resumen_final, aes(x = Clusters, y = tickets_totales, fill = Clusters)) +
  geom_boxplot() +
  labs(title = "Tickets totales por Clúster", y = "Tickets Totales", x = "Clúster") +
  theme_minimal()
ggplotly(tickets_cluster)


#Gráficos de medias de cada variable por clúster
# Calcular medias
medias_clusters <- resumen_final %>%
  group_by(Clusters) %>%
  summarise(
    media_productos = mean(media_productos_por_ticket),
    frecuencia = mean(frecuencia_visitas),
    tickets = mean(tickets_totales)
  )

# Convertir a formato largo para graficar
medias_long <- pivot_longer(medias_clusters, cols = -Clusters, names_to = "Variable", values_to = "Valor")

# Graficar
medias_grafico<-ggplot(medias_long, aes(x = Variable, y = Valor, fill = Clusters)) +
  geom_col(position = "dodge") +
  labs(title = "Comparación de medias por variable y clúster", x = "Variable", y = "Valor Medio") +
  theme_minimal()
ggplotly(medias_grafico)


#CLUSTER 1: Clientes de Compra Alta y Esporádica
#Media de productos por ticket alta: 44,43
#Frecuencia de visitas promedio baja: 2,5
#Ticket totales promedio: 106,48
#Clientes en cada grupo: 14.096
#Clientes que hacen pocas visitas, pero cuando compran, adquieren muchos productos. 
#Su gasto por compra probablemente es alto.

#CLUSTER 2: Clientes Frecuentes y Fieles
#Media de productos por ticket : 32,21
#Frecuencia de visitas promedio alta: 9,79
#Ticket totales promedio alto: 304,88
#Clientes en cada grupo: 5.623
#Clientes muy recurrentes, con visitas frecuentes y alto volumen de tickets. 
#Posiblemente los clientes más rentables y comprometidos.

#CLUSTER 3: Clientes Ocasionales
#Media de productos por ticket baja: 19,51
#Frecuencia de visitaspromedio  baja: 2,17
#Ticket totales promedio bajo: 41,88
#Clientes en cada grupo: 25.423
#Clientes poco comprometidos, con compras esporádicas y de bajo volumen. 
#Este grupo puede representar una oportunidad para activar o fidelizar.
