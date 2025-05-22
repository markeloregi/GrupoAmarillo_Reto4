library(dplyr)
library(tibble)
library(tidyr)

datos<- readRDS("Datos/Originales/tickets_enc.RDS")

unique(as.character(datos$id_cliente_enc))

objetivos<-readRDS("Datos/Originales/objetivos.RDS")


crear_nuevo_numero_ticket <- function(data) {
  datos<<-data %>%
    mutate(grupo = paste(num_ticket, id_cliente_enc, sep = "_")) %>%
    mutate(nuevo_num_ticket = as.integer(factor(grupo))) %>%
    select(-grupo)
  return(datos)
}
crear_nuevo_numero_ticket(datos)

unique(datos$nuevo_num_ticket)
unique(datos$id_cliente_enc)
unique(datos$cod_est)

objetivos_unlisted<-unlist(objetivos)

clientes_a_guardar<-datos[datos$id_cliente_enc %in% objetivos_unlisted,-2]
unique(clientes_a_guardar$id_cliente_enc)

producto_obj1<-datos[datos$cod_est %in% objetivos[[1]]$obj,]
#productos_a_guardar<-datos[datos$cod_est %in% objetivos_unlisted,-2]

#datos_a_restaurar<-rbind(clientes_a_guardar,productos_a_guardar)


# Total de tickets por cliente
tickets_por_cliente <- datos %>%
  group_by(id_cliente_enc) %>%
  summarise(n_tickets = n_distinct(num_ticket),
            n_productos = n()) %>% 
  mutate(cuartil=ntile(n_tickets,n = 10))



# Total de ventas por producto
ventas_por_producto <- datos %>%
  group_by(cod_est) %>%
  summarise(n_ventas = n(),
            n_clientes = n_distinct(id_cliente_enc)) %>% 
  mutate(cuartil=ntile(n_ventas,n = 10))


# Clientes con menos de 3 tickets
clientes_a_eliminar <- tickets_por_cliente %>%
  filter(cuartil > 9 | cuartil < 5)

# Productos con menos de 7 ventas o menos de 5 clientes distintos
productos_a_eliminar <- ventas_por_producto %>%
  filter(cuartil > 9 | cuartil < 5)


str(datos)
# Clientes con mayor frecuencia
classify_frequent_buyers <- function(df, max_dias_pasados) {
  df_resultado <<- df %>%
    mutate(dia = as.Date(dia)) %>%
    distinct(id_cliente_enc, nuevo_num_ticket, dia) %>%  # una fila por compra real
    arrange(id_cliente_enc, dia) %>%
    group_by(id_cliente_enc) %>%
    mutate(dia_prev = lag(dia),
           dias_pasados = as.numeric(dia - dia_prev)) %>%
    summarise(
      media_dias_pasados = mean(dias_pasados, na.rm = TRUE),
      compras = n(),
      frequent_buyer = media_dias_pasados < max_dias_pasados,
      .groups = "drop"
    ) %>%
    arrange(media_dias_pasados)
  return(df_resultado)
}
classify_frequent_buyers(datos,2)

clientes_eliminar_frecuentes<-df_resultado[df_resultado$frequent_buyer==T,1]

# Eliminar clientes y productos del data original
productos_a_guardar<-producto_obj1 %>% 
  filter(
    !id_cliente_enc %in% clientes_a_eliminar$id_cliente_enc,
    !id_cliente_enc %in% clientes_eliminar_frecuentes
  )

data_filtrada <- datos %>%
  filter(
    !id_cliente_enc %in% clientes_a_eliminar$id_cliente_enc,
    !cod_est %in% productos_a_eliminar$cod_est,
    !id_cliente_enc %in% clientes_eliminar_frecuentes
  )

data_nueva<-data_filtrada[,-2]

datos_a_reincorporar<-rbind(clientes_a_guardar,productos_a_guardar[,-2])

restore_missing_rows <- function(datos_reducidos, filas_a_incorporar) {
  for (col in c("dia","cod_est","id_cliente_enc","nuevo_num_ticket")) {
    datos_reducidos[[col]] <- as.character(datos_reducidos[[col]])
    filas_a_incorporar[[col]] <- as.character(filas_a_incorporar[[col]])
  }
  filtered_keys <- paste(datos_reducidos$dia, datos_reducidos$cod_est, datos_reducidos$id_cliente_enc,datos_reducidos$nuevo_num_ticket, sep = "_")
  stored_keys   <- paste(filas_a_incorporar$dia, filas_a_incorporar$cod_est, filas_a_incorporar$id_cliente_enc,filas_a_incorporar$nuevo_num_ticket, sep = "_")
  missing_rows <- filas_a_incorporar[!(paste(filas_a_incorporar$dia, filas_a_incorporar$cod_est, filas_a_incorporar$id_cliente_enc,filas_a_incorporar$nuevo_num_ticket, sep = "_") %in% filtered_keys), ]
  print(unique(missing_rows$id_cliente_enc))
  df_final <<- rbind(datos_reducidos,missing_rows)
  return(df_final)
}
restore_missing_rows(data_nueva,datos_a_reincorporar)

unique(df_final$id_cliente_enc)

write.csv(df_final,"Datos/Transformados/Tickets_Limpio.csv")

matriz_datos <- df_final %>%
  count(id_cliente_enc, cod_est) %>%
  pivot_wider(names_from = cod_est, values_from = n, values_fill = list(n = 0)) %>%
  column_to_rownames("id_cliente_enc")

write.csv(matriz_datos,"Datos/Transformados/Matriz_Datos.csv")

a<-sum(matriz_datos==0)
b<-sum(matriz_datos!=0)
c<-b/a
message(paste0("Porcentaje NA de: ",c,"."))

