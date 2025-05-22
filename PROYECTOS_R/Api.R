library(plumber)
library(dplyr)
library(stringr)
library(Matrix)
library(rsparse)


datos <- read.csv("Datos/Transformados/Matriz_Datos.csv")
matriz <- as.matrix(datos[, -1])
rownames(matriz) <- datos[[1]]
matriz[is.na(matriz)] <- 0
matriz_sparse <- as(matriz, "dgCMatrix")


maestro <- readRDS("Datos/Originales/maestroestr.RDS")
maestro <- maestro %>%
  rename(Producto = cod_est) %>%
  mutate(Producto = trimws(as.character(Producto)),
         descripcion = trimws(as.character(descripcion)))

#Entrenar modelo
set.seed(123)
modelo <- WRMF$new(rank = 32, lambda = 0.01, feedback = "implicit")
user_factors <- modelo$fit_transform(matriz_sparse, n_iter = 20, n_threads = 4)
item_factors <- modelo$components

#* Endpoint para recomendaciones
#* @param cliente_id El ID del cliente para el que se generan recomendaciones
#* @get /recomendar
#* @json
function(cliente_id = "") {
  tryCatch({
    if (cliente_id == "") {
      return(list(error = "Debe proporcionar un cliente_id"))
    }
    
    if (!(cliente_id %in% rownames(matriz_sparse))) {
      return(list(error = "Cliente no encontrado en la matriz."))
    }
    
    i <- which(rownames(matriz_sparse) == cliente_id)
    
    u <- user_factors[i, , drop = FALSE]
    scores <- u %*% item_factors
    
    productos_comprados <- which(matriz_sparse[i, ] != 0)
    scores[productos_comprados] <- -Inf
    
    top_idx <- head(order(scores, decreasing = TRUE), 5)
    codigos <- colnames(matriz_sparse)[top_idx]
    puntuaciones <- round(scores[1, top_idx], 3)
    
    codigos_limpios <- str_remove_all(codigos, "X")
    
    resultados <- data.frame(
      Producto = codigos_limpios,
      Puntuacion = puntuaciones,
      stringsAsFactors = FALSE
    )
    
    df_final <- left_join(resultados, maestro, by = "Producto")
    df_final$descripcion[is.na(df_final$descripcion)] <- "Sin descripción"
    
    return(df_final)
  }, error = function(e) {
    return(list(error = paste("Error interno:", e$message)))
  })
}

