df<- readRDS("C:/Users/unaip/OneDrive - Mondragon Unibertsitatea/PROYECTOS GRUPO AMARILLO/Reto 04/PROYECTOS_R/Datos/Originales/tickets_enc.RDS")

library(dplyr)

# Agrupar por num_ticket y contar los valores únicos de id_cliente_enc
resultado_M <- df %>%
  group_by(num_ticket) %>%
  summarise(num_clientes = n_distinct(id_cliente_enc)) %>%
  filter(num_clientes > 1)  # Filtrar solo los que tienen más de un id_cliente_enc

resultado_B <- df %>%
  group_by(num_ticket) %>%
  summarise(num_clientes = n_distinct(id_cliente_enc)) %>%
  filter(num_clientes == 1)

library(dplyr)

# Filtrar los num_ticket que tienen más de un id_cliente_enc
tickets_duplicados <- df %>%
  group_by(num_ticket) %>%
  filter(n_distinct(id_cliente_enc) > 1) %>%
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Mostrar el nuevo dataframe
print(tickets_duplicados)
