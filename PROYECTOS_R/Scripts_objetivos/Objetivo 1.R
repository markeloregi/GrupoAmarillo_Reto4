
# Cargar datos
datos<-readRDS("Datos\\Transformados\\Matriz_Datos.rds")
objetivos<-readRDS("Datos\\Originales\\objetivos.RDS")
maestro<-readRDS("Datos\\Originales\\maestroestr.RDS")

objetivos[[1]]
producto<-objetivos[[1]]$obj

maestro <- maestro %>%
  rename(Producto = cod_est)

maestro$Producto <- trimws(as.character(maestro$Producto))

# Preparar matriz
matriz <- as.matrix(datos)
matriz[is.na(matriz)] <- 0
matriz_sparse <- as(matriz, "dgCMatrix")

# Entrenar modelo WRMF
set.seed(123)
modelo <- WRMF$new(rank = 32, lambda = 0.01, feedback = "implicit")
user_factors <- modelo$fit_transform(matriz_sparse, n_iter = 20, n_threads = 4)
item_factors <- modelo$components

# ========================
# PROMOCIÓN DE UN PRODUCTO CONCRETO
# ========================

# ID del producto a promocionar (como aparece en las columnas de la matriz)
producto_a_promocionar <- producto  # Usa el ID real con prefijo "X" si lo tiene

# Verificar si existe el producto en la matriz
if (!(producto_a_promocionar %in% colnames(matriz_sparse))) {
  stop("El producto no se encuentra en la matriz.")
}

# Obtener índice de columna del producto
col_idx <- which(colnames(matriz_sparse) == producto_a_promocionar)

# Obtener el vector de factores latentes del producto
v <- item_factors[, col_idx, drop = FALSE]

# Calcular puntuaciones de afinidad de todos los usuarios hacia el producto
scores <- user_factors %*% v  # Producto escalar por cada usuario

# Obtener los 10 usuarios con mayor puntuación
top_usuarios_idx <- head(order(scores[,1], decreasing = TRUE), 10)
clientes_top <- rownames(matriz_sparse)[top_usuarios_idx]
puntuaciones_top <- round(scores[top_usuarios_idx, 1], 3)

# Crear tabla con resultados
recomendacion_promocional <- data.frame(
  Cliente = clientes_top,
  Puntuacion = puntuaciones_top,
  stringsAsFactors = FALSE
)

# Mostrar resultado
cat("Recomendación promocional del producto:", producto_a_promocionar, "\n\n")
print(recomendacion_promocional)

library(ggplot2)

# Calcular las puntuaciones de todos los usuarios
scores_all <- data.frame(
  Cliente = rownames(matriz_sparse),
  Puntuacion = round(as.vector(scores), 3),
  stringsAsFactors = FALSE
)

# Ordenar de mayor a menor
scores_all <- scores_all %>% arrange(desc(Puntuacion))

library(ggplot2)

Afinidad<- ggplot(scores_all, aes(x = reorder(Cliente, -Puntuacion), y = Puntuacion)) +
  geom_bar(stat = "identity", fill = "#194586") +
  labs(
    title = paste("Afinidad de TODOS los clientes con el producto:", producto_a_promocionar),
    x = "Cliente (ordenados por afinidad)",
    y = "Puntuación"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  # Opcional: ocultar etiquetas de cliente
saveRDS(Afinidad, "VISUALIZACION/graficos/Afinidad.rds")

Top10_afinidad<- ggplot(recomendacion_promocional, aes(x = reorder(Cliente, Puntuacion), y = Puntuacion)) +
  geom_col(fill = "#d31e17") +
  geom_text(aes(label = Puntuacion), hjust = -0.1) +
  coord_flip() +
  labs(
    title = paste("Top 10 Clientes con Puntuaciones de Afinidad"),
    x = "Cliente",
    y = "Puntuación"
  ) +
  theme_minimal()
saveRDS(Top10_afinidad, "VISUALIZACION/graficos/Top10_afinidad.rds")
