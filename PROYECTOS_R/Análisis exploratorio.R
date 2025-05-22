#Librerias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(factoextra)
library(RColorBrewer)


#Cargar los datos
maestro<-readRDS("Datos\\Originales\\maestroestr.RDS")
tickets<-readRDS("Datos\\Originales\\tickets_enc.RDS")

#Juntar ambos archivos
datos<-merge(maestro, tickets)

#Convertir un tipo fecha
datos$dia <- ymd(datos$dia)

#Resumen estadístico
summary(datos)

#Análisis de valores únicos
n_distinct(datos$cod_est)
n_distinct(datos$id_cliente_enc)
n_distinct(datos$num_ticket)

colors <- brewer.pal(8, "Set3")

#Distribución de compras por fecha
ggplot(datos, aes(x = dia)) + 
  geom_histogram(binwidth = 30, fill = colors[1], color = "black") +
  labs(title = "Distribución de compras en el tiempo", x = "Fecha", y = "Cantidad de ventas")

# Frecuencia de compra por cliente
frecuencia_clientes <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(compras = n())

clientes_compras<-frecuencia_clientes %>%
  arrange(desc(compras)) %>%
  head(10)
mejores_clientes<-ggplot(clientes_compras, aes(x = id_cliente_enc, y = compras)) +
  geom_col(fill = colors, bins = 30) +
  labs(title = "Distribución de frecuencia de compra por cliente", x = "Número de compras", y = "Clientes")

#Análisis de popularidad de productos
productos_populares <- datos %>%
  group_by(cod_est, descripcion) %>%
  summarise(ventas = n()) %>%
  arrange(desc(ventas))
mejores_productos<-head(productos_populares, 10)
productos <- ggplot(mejores_productos, aes(x = descripcion, y = ventas, fill = descripcion)) +
  geom_col() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggplotly(productos)


#Datos para hacer el clustering
datos_cluster <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(
    frecuencia_compra = n_distinct(num_ticket),
    productos_totales = n(),
    productos_distintos = n_distinct(cod_est),
    ticket_promedio = productos_totales / frecuencia_compra,
    media_productos = mean(productos_totales / frecuencia_compra, na.rm = TRUE)
  ) %>%
  ungroup()


# 5. Histograma del tamaño promedio del ticket
p5 <- ggplot(datos_cluster, aes(x = ticket_promedio)) +
  geom_histogram(binwidth = 2, fill = colors[3], alpha = 0.8) +
  labs(title = "Distribución del Tamaño Promedio del Ticket", x = "Productos por Ticket", y = "Cantidad de Clientes") +
  theme_minimal()

ggplotly(p5)

p5_boxplot <- ggplot(datos_cluster, aes(y = ticket_promedio)) +
  geom_boxplot(fill = colors[5], alpha = 0.7) +
  labs(title = "Distribución del Tamaño Promedio del Ticket", 
       y = "Productos por Ticket", 
       x = "Clientes") +
  theme_minimal()

ggplotly(p5_boxplot)

#Evolucion numero de clientes unicos por mes
clientes_mes <- clientes_mes %>%
  mutate(mes = floor_date(mes, unit = "month")) %>%  # Agrupa por mes completo
  group_by(mes) %>%
  summarise(clientes_unicos = sum(clientes_unicos))  # Asegura que haya un valor por mes

ggplot(clientes_mes, aes(x = mes, y = clientes_unicos)) +
  geom_line(color = colors[5], linewidth = 1) +  # Añade la línea
  geom_point(size = 4, color = colors[5]) +      # Los puntos siguen visibles
  geom_text(aes(label = clientes_unicos), vjust = -1.2) +
  labs(title = "Clientes Únicos por Mes",
       x = "Mes", y = "Clientes Únicos") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()

#Top 10 clientes por frequencia de compra
clientes_frecuentes <- datos %>%
  distinct(id_cliente_enc, num_ticket) %>%
  count(id_cliente_enc, name = "tickets") %>%
  arrange(desc(tickets)) %>%
  slice_head(n = 10)

ggplot(clientes_frecuentes, aes(x = reorder(id_cliente_enc, tickets), y = tickets)) +
  geom_col(fill = colors[6]) +
  coord_flip() +
  labs(title = "Top 10 Clientes por Frecuencia de Compra", 
       x = "Cliente", y = "N° de Tickets") +
  theme_minimal()

#Histograma numero de tickets por cliente
tickets_por_cliente <- datos %>%
  distinct(id_cliente_enc, num_ticket) %>%
  count(id_cliente_enc, name = "n_tickets")

ggplot(tickets_por_cliente, aes(x = n_tickets)) +
  geom_histogram(binwidth = 1, fill = colors[7], color = "black") +
  labs(title = "Distribución del Número de Tickets por Cliente", 
       x = "Tickets", y = "Número de Clientes") +
  theme_minimal()#Variedad promedio de productos por tickets


#Variedad promedio de productos por tickets
clientes_variedad <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(productos_distintos = n_distinct(cod_est), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  summarise(media_variedad = mean(productos_distintos), .groups = "drop") %>%
  arrange(desc(media_variedad)) %>%
  slice_head(n = 10)

ggplot(clientes_variedad, aes(x = reorder(id_cliente_enc, media_variedad), y = media_variedad)) +
  geom_col(fill = colors[8]) +
  coord_flip() +
  labs(title = "Top 10 Clientes por Variedad Promedio de Productos por Ticket", 
       x = "Cliente", y = "Media de Productos Distintos") +
  theme_minimal()

#Productos distintos en el total de productos
p8 <- ggplot(datos_cluster, aes(x = productos_distintos, y = productos_totales)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = "white") +
  scale_fill_viridis_c(option = "C") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Densidad de Productos Distintos vs Totales",
       x = "Productos Distintos",
       y = "Productos Totales",
       fill = "Densidad") +
  theme_minimal()

ggplotly(p8)


#Ventas por dia de la semana
datos$dow <- wday(datos$dia, label = TRUE, abbr = FALSE, week_start = 1)

ggplot(datos, aes(x = dow)) +
  geom_bar(fill = colors[3]) +
  labs(title = "Ventas por Día de la Semana", x = "Día", y = "Cantidad de ventas") +
  theme_minimal()


#Venta top 10 productos por mes
datos$mes <- floor_date(datos$dia, unit = "month")

top_prod <- datos %>%
  count(descripcion, mes) %>%
  group_by(descripcion) %>%
  summarise(total = sum(n)) %>%
  top_n(10, total) %>%
  pull(descripcion)

heatmap_data <- datos %>%
  filter(descripcion %in% top_prod) %>%
  count(mes, descripcion)

p15 <- ggplot(heatmap_data, aes(x = mes, y = descripcion, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Top 10 Productos por Mes", 
       x = "Mes", y = "Producto", fill = "Ventas") +
  theme_minimal()

ggplotly(p15)




