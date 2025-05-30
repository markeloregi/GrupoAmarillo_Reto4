# Cargar datos
maestro <- readRDS("Datos/Originales/maestroestr.RDS")
tickets <- readRDS("Datos/Originales/tickets_enc.RDS")
datos <- merge(maestro, tickets)
datos$dia <- ymd(datos$dia)

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

# UI
ui <- navbarPage("Visualización de Datos",
                 tabPanel("Portada",
                          div(
                            style = "text-align: center; background-color: #d31e17; height: 100vh; padding-top: 150px;",
                            tags$img(src = "logo_eroski.png", height = "180px", style = "margin-bottom: 30px;"),
                            h1("EROSKI RETO 04", style = "color: #ffffff; font-size: 60px; font-weight: bold; margin-bottom: 20px;"),
                            h2("GRUPO AMARILLO", style = "color: #194586; font-size: 40px; font-weight: bold;")
                          )
                 ),
                 tabPanel("Análisis Exploratorio",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("fecha", "Rango de fechas:", start = NULL, end = NULL),
                              sliderInput("rango_productos", "Rango de productos por ticket:", min = 1, max = 50, value = c(1, 50))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Evolución Temporal",
                                         plotlyOutput("grafico_compras_tiempo"),
                                         plotlyOutput("grafico_clientes_mes")),
                                tabPanel("Clientes",
                                         plotlyOutput("grafico_top_clientes"),
                                         plotlyOutput("grafico_tickets_cliente")),
                                tabPanel("Productos",
                                         plotlyOutput("grafico_productos_top"),
                                         plotlyOutput("grafico_top10_heatmap")),
                                tabPanel("Tickets",
                                         plotlyOutput("grafico_hist_ticket"),
                                         plotlyOutput("grafico_box_ticket"))
                              )
                            )
                          )
                 ),
                 tabPanel("Clustering",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Jerárquico",
                                       tabsetPanel(
                                         tabPanel("Distancia Euclídea", plotlyOutput("plot_jer_euc")),
                                         tabPanel("Distancia Manhattan", plotlyOutput("plot_jer_man"))
                                       )),
                              tabPanel("K-Means",
                                       tabsetPanel(
                                         tabPanel("Distancia Euclídea", plotlyOutput("plot_kmeans_euc")),
                                         tabPanel("Distancia Manhattan", plotlyOutput("plot_kmeans_man"))
                                       )),
                              tabPanel("Boxplots por Variable",
                                       tabsetPanel(
                                         tabPanel("Media productos por ticket", plotlyOutput("plot_productos")),
                                         tabPanel("Frecuencia de visitas", plotlyOutput("plot_frecuencia")),
                                         tabPanel("Tickets totales", plotlyOutput("plot_tickets"))
                                       )),
                              tabPanel("Clusters en 3D",
                                       plotlyOutput("plot_3d", height = "600px")),
                              tabPanel("Dendrogramas",
                                       tabsetPanel(
                                         tabPanel("Euclídea", plotlyOutput("plot_dendro_euc")),
                                         tabPanel("Manhattan", plotlyOutput("plot_dendro_man"))
                                       ))))),
                 tabPanel("Objetivos",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Objetivo 1",
                                       plotlyOutput("grafico_afinidad"),
                                       plotlyOutput("grafico_top10_afinidad")
                              ),
                              tabPanel("Objetivo 2",
                                       h3("Gráficos por cliente"),
                                       plotOutput("grafico_obj2_1"),
                                       plotOutput("grafico_obj2_2"),
                                       plotOutput("grafico_obj2_3"),
                                       plotOutput("grafico_obj2_4"),
                                       plotOutput("grafico_obj2_5"),
                                       plotOutput("grafico_obj2_6"),
                                       plotOutput("grafico_obj2_7"),
                                       plotOutput("grafico_obj2_8"),
                                       plotOutput("grafico_obj2_9"),
                                       plotOutput("grafico_obj2_10")
                              ),
                              tabPanel("Objetivo 3",
                                       plotlyOutput("grafico_recom_obj3"),
                                       DT::DTOutput("tabla_recomendaciones")
                              ),
                              tabPanel("Objetivo 4",
                                       plotlyOutput("grafico_obj4")
                              )
                            )
                          )
                 )
)



# SERVER
server <- function(input, output, session) {
  eroski_rojo <- "#d31e17"
  eroski_azul <- "#194586"
  eroski_blanco <- "#ffffff"
  cluster_colores <- c("1" = eroski_rojo, "2" = "#000000", "3" = eroski_azul)
  
  observe({
    updateDateRangeInput(session, "fecha", start = min(datos$dia), end = max(datos$dia))
    updateSliderInput(session, "rango_productos", max = max(datos_cluster$productos_totales, na.rm = TRUE),
                      value = c(1, max(datos_cluster$productos_totales, na.rm = TRUE)))
  })
  
  datos_filtrados <- reactive({
    datos %>% filter(dia >= input$fecha[1] & dia <= input$fecha[2])
  })
  
  datos_cluster_filtrados <- reactive({
    datos_cluster %>%
      filter(productos_totales >= input$rango_productos[1], productos_totales <= input$rango_productos[2])
  })
  
  output$grafico_compras_tiempo <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(x = dia)) +
      geom_histogram(binwidth = 30, fill = eroski_rojo, color = "black") +
      labs(title = "Distribución de compras en el tiempo", x = "Fecha", y = "Cantidad de ventas") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_clientes_mes <- renderPlotly({
    df <- datos_filtrados() %>% mutate(mes = floor_date(dia, "month")) %>%
      group_by(mes) %>% summarise(clientes_unicos = n_distinct(id_cliente_enc), .groups = "drop")
    p <- ggplot(df, aes(x = mes, y = clientes_unicos)) +
      geom_line(color = eroski_rojo, size = 1.2) +
      labs(title = "Evolución de Clientes Únicos por Mes", x = "Mes", y = "Clientes Únicos") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_top_clientes <- renderPlotly({
    df <- datos_filtrados() %>% count(id_cliente_enc, name = "tickets") %>%
      arrange(desc(tickets)) %>% slice_head(n = 10)
    p <- ggplot(df, aes(x = reorder(id_cliente_enc, tickets), y = tickets)) +
      geom_col(fill = eroski_azul) + coord_flip() +
      labs(title = "Top 10 Clientes por Frecuencia de Compra", x = "Cliente", y = "Nº de Tickets") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_tickets_cliente <- renderPlotly({
    df <- datos_filtrados() %>% count(id_cliente_enc) %>% rename(n_tickets = n)
    p <- ggplot(df, aes(x = n_tickets)) +
      geom_histogram(binwidth = 1, fill = eroski_azul, color = "black") +
      labs(title = "Distribución del Número de Tickets por Cliente", x = "Tickets", y = "Número de Clientes") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_productos_top <- renderPlotly({
    df <- datos_filtrados() %>%
      group_by(cod_est, descripcion) %>%
      summarise(ventas = n(), .groups = "drop") %>%
      arrange(desc(ventas)) %>% slice_head(n = 10)
    p <- ggplot(df, aes(x = reorder(descripcion, ventas), y = ventas)) +
      geom_col(fill = eroski_rojo) + coord_flip() +
      labs(title = "Top 10 Productos Más Vendidos", x = "Producto", y = "Ventas") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_top10_heatmap <- renderPlotly({
    df <- datos_filtrados()
    df$mes <- floor_date(df$dia, unit = "month")
    top_prod <- df %>%
      count(descripcion, mes) %>%
      group_by(descripcion) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      top_n(10, total) %>%
      pull(descripcion)
    
    heatmap_data <- df %>%
      filter(descripcion %in% top_prod) %>%
      count(mes, descripcion)
    
    p <- ggplot(heatmap_data, aes(x = mes, y = descripcion, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = eroski_blanco, high = eroski_rojo) +
      labs(title = "Top 10 Productos más Vendidos por Mes", x = "Mes", y = "Producto", fill = "Ventas") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_hist_ticket <- renderPlotly({
    p <- ggplot(datos_cluster_filtrados(), aes(x = ticket_promedio)) +
      geom_histogram(binwidth = 2, fill = eroski_blanco, color = "black") +
      labs(title = "Distribución del Tamaño Promedio del Ticket", x = "Productos promedio por Ticket", y = "Cantidad de Clientes") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$grafico_box_ticket <- renderPlotly({
    p <- ggplot(datos_cluster_filtrados(), aes(y = ticket_promedio)) +
      geom_boxplot(fill = eroski_blanco, color = "black") +
      labs(title = "Boxplot del Tamaño Promedio del Ticket", y = "Productos por Ticket", x = "") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Clustering
  output$plot_jer_euc <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/plot_jer_euc.rds") %>% style(colors = cluster_colores)
    p
  })
  
  output$plot_jer_man <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/plot_jer_man.rds") %>% style(colors = cluster_colores)
    p
  })
  
  output$plot_kmeans_euc <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/plot_kmeans_euc.rds") %>% style(colors = cluster_colores)
    p
  })
  
  output$plot_kmeans_man <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/plot_kmeans_man.rds") %>% style(colors = cluster_colores)
    p
  })
  
  output$plot_productos <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/productos_por_ticket.rds") %>% layout(boxmode = "group")
    for (i in 1:3) {
      p$x$data[[i]]$marker$color <- cluster_colores[as.character(i)]
      p$x$data[[i]]$marker$line <- list(color = "black")
    }
    p
  })
  
  output$plot_frecuencia <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/frecuencia_visitas.rds")
    for (i in 1:3) {
      p$x$data[[i]]$marker$color <- cluster_colores[as.character(i)]
      p$x$data[[i]]$marker$line <- list(color = "black")
    }
    p
  })
  
  output$plot_tickets <- renderPlotly({
    p <- readRDS("VISUALIZACION/graficos/tickets_totales.rds")
    for (i in 1:3) {
      p$x$data[[i]]$marker$color <- cluster_colores[as.character(i)]
      p$x$data[[i]]$marker$line <- list(color = "black")
    }
    p
  })
  output$plot_3d <- renderPlotly({
    readRDS("VISUALIZACION/graficos/plot_3d.rds")
  })
  
  output$plot_dendro_euc <- renderPlotly({
    readRDS("VISUALIZACION/graficos/plotly_dendro_euc.rds")
  })
  
  output$plot_dendro_man <- renderPlotly({
    readRDS("VISUALIZACION/graficos/plotly_dendro_man.rds")
  })
  
  output$grafico_afinidad <- renderPlotly({
    readRDS("VISUALIZACION/graficos/Afinidad.rds")
  })
  
  output$grafico_top10_afinidad <- renderPlotly({
    readRDS("VISUALIZACION/graficos/Top10_afinidad.rds")
  })
  
  output$grafico_obj2_1 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_1.rds")
  })
  
  output$grafico_obj2_2 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_2.rds")
  })
  
  output$grafico_obj2_3 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_3.rds")
  })
  
  output$grafico_obj2_4 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_4.rds")
  })
  
  output$grafico_obj2_5 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_5.rds")
  })
  
  output$grafico_obj2_6 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_6.rds")
  })
  
  output$grafico_obj2_7 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_7.rds")
  })
  
  output$grafico_obj2_8 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_8.rds")
  })
  
  output$grafico_obj2_9 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_9.rds")
  })
  
  output$grafico_obj2_10 <- renderPlot({
    readRDS("VISUALIZACION/graficos/grafico_obj2_10.rds")
  })
  
  output$grafico_recom_obj3 <- renderPlotly({
    readRDS("VISUALIZACION/graficos/grafico_recom_obj3.rds")
  })
  
  output$tabla_recomendaciones <- DT::renderDT({
    tabla <- readRDS("VISUALIZACION/graficos/tabla_recomendaciones.rds")
    DT::datatable(tabla,
                  options = list(pageLength = 10),
                  caption = "Producto recomendado por cliente")
  })
  
  output$grafico_obj4 <- renderPlotly({
    readRDS("VISUALIZACION/graficos/grafico_obj4.rds")
  })
  
}


shinyApp(ui, server)

