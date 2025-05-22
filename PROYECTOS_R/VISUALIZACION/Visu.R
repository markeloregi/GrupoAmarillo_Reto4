library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# Cargar datos
maestro <- readRDS("Datos/Originales/maestroestr.RDS")
tickets <- readRDS("Datos/Originales/tickets_enc.RDS")
datos <- merge(maestro, tickets)
datos$dia <- ymd(datos$dia)

# Datos para clustering y otras métricas
frecuencia_clientes <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(compras = n(), .groups = "drop")

clientes_compras <- frecuencia_clientes %>%
  arrange(desc(compras)) %>%
  head(10)

productos_populares <- datos %>%
  group_by(cod_est, descripcion) %>%
  summarise(ventas = n(), .groups = "drop") %>%
  arrange(desc(ventas))

mejores_productos <- head(productos_populares, 10)

datos_cluster <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(
    frecuencia_compra = n_distinct(num_ticket),
    productos_totales = n(),
    productos_distintos = n_distinct(cod_est),
    ticket_promedio = productos_totales / frecuencia_compra,
    media_productos = mean(productos_totales / frecuencia_compra, na.rm = TRUE),
    .groups = "drop"
  )

clientes_mes <- datos %>%
  mutate(mes = floor_date(dia, "month")) %>%
  group_by(mes) %>%
  summarise(clientes_unicos = n_distinct(id_cliente_enc), .groups = "drop")

clientes_frecuentes <- datos %>%
  distinct(id_cliente_enc, num_ticket) %>%
  count(id_cliente_enc, name = "tickets") %>%
  arrange(desc(tickets)) %>%
  slice_head(n = 10)

tickets_por_cliente <- datos %>%
  distinct(id_cliente_enc, num_ticket) %>%
  count(id_cliente_enc, name = "n_tickets")

clientes_variedad <- datos %>%
  group_by(id_cliente_enc, num_ticket) %>%
  summarise(productos_distintos = n_distinct(cod_est), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  summarise(media_variedad = mean(productos_distintos), .groups = "drop") %>%
  arrange(desc(media_variedad)) %>%
  slice_head(n = 10)

colors <- brewer.pal(8, "Set3")

ui <- fluidPage(
  titlePanel("Dashboard de Análisis Exploratorio"),
  
  # Sección: Distribución en el tiempo
  h3("Distribución en el Tiempo"),
  fluidRow(
    column(6, plotOutput("compras_tiempo")),
    column(6, plotlyOutput("clientes_mes"))
  ),
  br(), hr(),
  
  # Sección: Análisis de Clientes
  h3("Clientes"),
  fluidRow(
    column(6, plotlyOutput("frecuencia_clientes")),
    column(6, plotOutput("top_clientes"))
  ),
  fluidRow(
    column(6, plotOutput("tickets_hist")),
    column(6, plotOutput("variedad_clientes"))
  ),
  br(), hr(),
  
  # Sección: Productos
  h3("Productos"),
  fluidRow(
    column(12, plotlyOutput("productos_populares"))
  ),
  br(), hr(),
  
  # Sección: Clustering Previo
  h3("Clustering Previo"),
  fluidRow(
    column(6, plotlyOutput("dispersión_productos")),
    column(6, plotlyOutput("hist_ticket"))
  ),
  fluidRow(
    column(12, plotlyOutput("box_ticket"))
  )
)

server <- function(input, output) {
  output$resumen <- renderPrint({
    summary(datos)
  })
  
  output$valores_unicos <- renderPrint({
    cat("Productos únicos:", n_distinct(datos$cod_est), "\n")
    cat("Clientes únicos:", n_distinct(datos$id_cliente_enc), "\n")
    cat("Tickets únicos:", n_distinct(datos$num_ticket), "\n")
  })
  
  output$compras_tiempo <- renderPlot({
    ggplot(datos, aes(x = dia)) + 
      geom_histogram(binwidth = 30, fill = colors[1], color = "black") +
      labs(title = "Distribución de compras en el tiempo", x = "Fecha", y = "Cantidad de ventas")
  })
  
  output$frecuencia_clientes <- renderPlotly({
    p <- ggplot(clientes_compras, aes(x = id_cliente_enc, y = compras)) +
      geom_col(fill = colors[2]) +
      labs(title = "Frecuencia de Compra por Cliente", x = "Cliente", y = "Compras")
    ggplotly(p)
  })
  
  output$productos_populares <- renderPlotly({
    p <- ggplot(mejores_productos, aes(x = descripcion, y = ventas, fill = descripcion)) +
      geom_col() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(title = "Top 10 Productos Populares", x = "Producto", y = "Ventas")
    ggplotly(p)
  })
  
  output$dispersión_productos <- renderPlotly({
    p <- ggplot(datos_cluster, aes(x = productos_distintos, y = productos_totales)) +
      geom_point(alpha = 0.6, color = colors[4]) +
      labs(title = "Productos Distintos vs Totales", x = "Productos Distintos", y = "Totales")
    ggplotly(p)
  })
  
  output$hist_ticket <- renderPlotly({
    p <- ggplot(datos_cluster, aes(x = ticket_promedio)) +
      geom_histogram(binwidth = 2, fill = colors[3], alpha = 0.8) +
      labs(title = "Tamaño Promedio del Ticket", x = "Productos por Ticket", y = "Clientes")
    ggplotly(p)
  })
  
  output$box_ticket <- renderPlotly({
    p <- ggplot(datos_cluster, aes(y = ticket_promedio)) +
      geom_boxplot(fill = colors[5], alpha = 0.7) +
      labs(title = "Distribución del Tamaño Promedio del Ticket", y = "Productos por Ticket")
    ggplotly(p)
  })
  
  output$clientes_mes <- renderPlotly({
    p <- ggplot(clientes_mes, aes(x = mes, y = clientes_unicos)) +
      geom_line(color = colors[5], size = 1.2) +
      labs(title = "Clientes Únicos por Mes", x = "Mes", y = "Clientes Únicos")
    ggplotly(p)
  })
  
  output$top_clientes <- renderPlot({
    ggplot(clientes_frecuentes, aes(x = reorder(id_cliente_enc, tickets), y = tickets)) +
      geom_col(fill = colors[6]) +
      coord_flip() +
      labs(title = "Top 10 Clientes por Frecuencia de Compra", x = "Cliente", y = "Tickets")
  })
  
  output$tickets_hist <- renderPlot({
    ggplot(tickets_por_cliente, aes(x = n_tickets)) +
      geom_histogram(binwidth = 1, fill = colors[7], color = "black") +
      labs(title = "Distribución de Tickets por Cliente", x = "Tickets", y = "Clientes")
  })
  
  output$variedad_clientes <- renderPlot({
    ggplot(clientes_variedad, aes(x = reorder(id_cliente_enc, media_variedad), y = media_variedad)) +
      geom_col(fill = colors[8]) +
      coord_flip() +
      labs(title = "Top 10 Clientes por Variedad Promedio de Productos", x = "Cliente", y = "Variedad")
  })
}

shinyApp(ui, server)
