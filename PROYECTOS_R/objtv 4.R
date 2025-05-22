set.seed(777)
library(dplyr)
library(Matrix)
library(rsparse)
library(tidyr)


objetivos <- readRDS("C:/Users/garat/OneDrive - Mondragon Unibertsitatea/PROYECTOS GRUPO AMARILLO/Reto 04/PROYECTOS_R/Datos/Originales/objetivos.RDS")
productos <- readRDS("C:/Users/garat/OneDrive - Mondragon Unibertsitatea/PROYECTOS GRUPO AMARILLO/Reto 04/PROYECTOS_R/Datos/Originales/maestroestr.RDS")
tickets <- read.csv("C:/Users/garat/OneDrive - Mondragon Unibertsitatea/PROYECTOS GRUPO AMARILLO/Reto 04/PROYECTOS_R/Datos/Transformados/Tickets_Limpio.csv")

str(objetivos)
str(productos)
str(tickets)


# VECTOR DE CLIENTES OBJETIVO 
clientes_objetivo <- objetivos$objetivo4$obj

# PREPARAR  DE INTERACCIONES
interacciones <- tickets %>%
  select(id_cliente_enc, cod_est) %>%
  distinct() %>%
  mutate(valor = 1)

# Mapear IDs a índices numéricos
cliente_idx <- interacciones %>%
  distinct(id_cliente_enc) %>%
  arrange(id_cliente_enc) %>%
  mutate(row = row_number())

producto_idx <- interacciones %>%
  distinct(cod_est) %>%
  arrange(cod_est) %>%
  mutate(col = row_number())

# Asociar índices
interacciones_idx <- interacciones %>%
  left_join(cliente_idx, by = "id_cliente_enc") %>%
  left_join(producto_idx, by = "cod_est")

# Matriz dispersa
matriz_train <- sparseMatrix(
  i = interacciones_idx$row,
  j = interacciones_idx$col,
  x = interacciones_idx$valor,
  dims = c(nrow(cliente_idx), nrow(producto_idx))
)

# Guardar mapeos
cliente_map <- cliente_idx
producto_map <- producto_idx

# MODELO ALS 
modelo <- WRMF$new(rank = 30, lambda = 0.1, feedback = "implicit")
modelo$fit_transform(matriz_train, n_iter = 30)

# ÚLTIMO TICKET DE CADA CLIENTE 
ultimos_tickets <- tickets %>%
  filter(id_cliente_enc %in% clientes_objetivo) %>%
  group_by(id_cliente_enc, nuevo_num_ticket) %>%
  summarise(dia = max(dia), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  slice_max(dia,     n = 1) %>%
  ungroup() %>%
  inner_join(tickets, by = c("id_cliente_enc", "nuevo_num_ticket", "dia"))

# GENERAR RECOMENDACIONES
clientes_objetivo_idx <- cliente_map %>%
  filter(id_cliente_enc %in% clientes_objetivo)

recomendaciones <- lapply(1:nrow(clientes_objetivo_idx), function(i) {
  cliente_row <- clientes_objetivo_idx$row[i]
  cliente_id <- clientes_objetivo_idx$id_cliente_enc[i]
  
  comprados <- ultimos_tickets %>%
    filter(id_cliente_enc == cliente_id) %>%
    pull(cod_est) %>%
    unique()
  
  no_recomendar_idx <- producto_map %>%
    filter(cod_est %in% comprados) %>%
    pull(col)
  
  user_vector <- matriz_train[cliente_row, , drop = FALSE]
  
  pred <- modelo$predict(user_vector, k = 10)
  
  if (length(pred) == 0) {
    return(data.frame(id_cliente_enc = cliente_id, producto_recomendado = NA))
  }
  
  # Filtrar recomendaciones evitando los productos ya comprados
  productos_recomendados <- pred[!(pred %in% no_recomendar_idx)]
  
  if (length(productos_recomendados) == 0) {
    return(data.frame(id_cliente_enc = cliente_id, producto_recomendado = NA))
  }
  
  cod_est <- producto_map %>% filter(col == productos_recomendados[1]) %>% pull(cod_est)
  
  data.frame(id_cliente_enc = cliente_id, producto_recomendado = cod_est)
})

recomendaciones <- bind_rows(recomendaciones)

print(recomendaciones)

unique(recomendaciones$producto_recomendado)






# Agregar a resultados
resultados <- rbind(recomendaciones, data.table(
  cliente = tickets$id_cliente_enxc,
  recomendacion = recomendaciones$producto_recomendado,
  descripcion = tickets$cod_est
))







