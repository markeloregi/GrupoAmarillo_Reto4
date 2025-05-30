set.seed(777)

objetivos <- readRDS("Datos/Originales/objetivos.RDS")
productos <- readRDS("Datos/Originales/maestroestr.RDS")
tickets <- readRDS("Datos/Transformados/Tickets_Limpio.RDS")

str(objetivos)
str(productos)
str(tickets)


# VECTOR DE CLIENTES OBJETIVO 
clientes_objetivo <- objetivos$objetivo4$obj

# PREPARAR MATRIZ  DE INTERACCIONES
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



# Filtrar productos para quedarnos solo con los recomendados
productos_filtrados <- productos %>%
  filter(cod_est %in% recomendaciones$producto_recomendado)

# Unir la descripción al dataframe de recomendaciones
recomendaciones_con_descripcion <- recomendaciones %>%
  left_join(productos_filtrados, by = c("producto_recomendado" = "cod_est"))

#Resultado
print(recomendaciones_con_descripcion)


#graficos
#ggplot
ggplot(recomendaciones_con_descripcion, aes(x = descripcion, y = id_cliente_enc)) +
  geom_segment(aes(xend = descripcion, yend = 0), color = "gray") +
  geom_point(color = "steelblue", size = 4) +
  labs(
    title = "Clientes y productos recomendados",
    x = "Producto",
    y = "ID Cliente"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#shiny
# UI
ui <- fluidPage(
  titlePanel("Clientes y productos recomendados - Conexiones visuales"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cliente", "Filtrar por cliente:", 
                  choices = c("Todos", unique(recomendaciones_con_descripcion$id_cliente_enc))),
      
      selectInput("producto", "Filtrar por producto:", 
                  choices = c("Todos", unique(recomendaciones_con_descripcion$descripcion)))
    ),
    
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Server
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    data <- recomendaciones_con_descripcion
    
    if (input$cliente != "Todos") {
      data <- data %>% filter(id_cliente_enc == input$cliente)
    }
    
    if (input$producto != "Todos") {
      data <- data %>% filter(descripcion == input$producto)
    }
    
    return(data)
  })
  
  output$grafico <- renderPlot({
    data <- datos_filtrados()
    
    # Para que se vean alineados en dos columnas
    data$cliente_y <- seq_along(data$id_cliente_enc)
    data$producto_y <- seq_along(data$id_cliente_enc)
    
    ggplot(data) +
      geom_segment(aes(x = 1, xend = 2, y = cliente_y, yend = producto_y), color = "gray") +
      geom_point(aes(x = 1, y = cliente_y), color = "darkgreen", size = 4) +
      geom_point(aes(x = 2, y = producto_y), color = "steelblue", size = 4) +
      geom_text(aes(x = 1, y = cliente_y, label = id_cliente_enc), hjust = 1.1, size = 4) +
      geom_text(aes(x = 2, y = producto_y, label = descripcion), hjust = -0.1, size = 4) +
      xlim(0.5, 2.5) +
      theme_void() +
      labs(title = "Relación Cliente → Producto Recomendado")
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)


# Ruta 
ruta_resultado <- "Resultados/"


#lista con los resultados
resultado_lista <- list(
  recomendaciones = recomendaciones_con_descripcion
)

# Guardar como .RDS
saveRDS(resultado_lista, file = file.path(ruta_resultado, "objetivo_04_recomendaciones_lista.RDS"))
