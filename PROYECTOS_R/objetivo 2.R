library(rsparse)
library(data.table)
library(Matrix)
library(shiny)
library(dplyr)
library(stringr)

datos <- read.csv("Datos/Transformados/Matriz_Datos.csv")
maestro <- readRDS("Datos/Originales/maestroestr.RDS")

maestro <- maestro %>%
  rename(Producto = cod_est)

maestro$Producto <- trimws(as.character(maestro$Producto))

#Convertir a matriz: filas = clientes, columnas = productos
matriz <- as.matrix(datos[, -1])
rownames(matriz) <- datos[[1]]

#Verificar que no haya NAs
matriz[is.na(matriz)] <- 0

#Convertir a matriz dispersa
matriz_sparse <- as(matriz, "dgCMatrix")

#Entrenar modelo
set.seed(123)
modelo <- WRMF$new(rank = 32, lambda = 0.01, feedback = "implicit")
user_factors <- modelo$fit_transform(matriz_sparse, n_iter = 20, n_threads = 4)

#Obtener producto por factores latentes
item_factors <- modelo$components

#IDs de los clientes del objetivo
clientes_objetivo <- c(
  "b51353fcf07cb61280eda45e33679871", 
  "02ff5edaa057b63ea0a0010c5402205c",
  "25d259d32a2bc254343715f2e347c518",   
  "53ffb83e85fd51cf1ec2fdef3c78b4fd", 
  "26f424b3bba6aaf97952ac599ed39f75", 
  "32cc820ac27ff143c3ea976f3fe69d34",
  "a57938025d714b65612bf2cfde12136d", 
  "af30d404e282749ccd5f5ad0c8e834c7",
  "8b9aa623b654a8be21b316a5fdf41007", 
  "e27ceb0a1576648212c4325fdf7d8002"
)

indices_clientes <- which(rownames(matriz_sparse) %in% clientes_objetivo)

#Generar recomendaciones
recomendaciones <- lapply(indices_clientes, function(i) {
  u <- user_factors[i, , drop = FALSE]
  scores <- u %*% item_factors
  
  productos_comprados <- which(matriz_sparse[i, ] != 0)
  scores[productos_comprados] <- -Inf
  
  top_idx <- head(order(scores, decreasing = TRUE), 5)
  
  productos <- colnames(matriz_sparse)[top_idx]
  puntuaciones <- round(scores[1, top_idx], 2)
  
  # Quitar la "X" para poder unir
  productos_limpios <- str_remove_all(productos, "X")
  
  df <- data.frame(Producto = productos_limpios,
                   Puntuacion = puntuaciones,
                   stringsAsFactors = FALSE)
  
  df <- df %>%
    left_join(maestro, by = "Producto")
  
  list(
    cliente_id = rownames(matriz_sparse)[i],
    recomendaciones = df
  )
})

#Resultados
for (rec in recomendaciones) {
  cat("\nCliente:", rec$cliente_id, "\n")
  cat("Productos recomendados:\n")
  print(rec$recomendaciones)
  cat("\n")
}


#App shiny
ui <- fluidPage(
  titlePanel("Recomendaciones Personalizadas"),
  sidebarLayout(
    sidebarPanel(
      textInput("cliente_id", "Introduce el cliente ID:", ""),
      actionButton("recomendar", "Mostrar Recomendaciones")
    ),
    mainPanel(
      tableOutput("tabla_recomendaciones")
    )
  )
)

server <- function(input, output) {
  
  obtener_recomendaciones <- eventReactive(input$recomendar, {
    req(input$cliente_id)
    
    #Verificar si el cliente existe en la matriz
    if (!(input$cliente_id %in% rownames(matriz_sparse))) {
      return(data.frame(Mensaje = "Cliente no encontrado en la matriz."))
    }
    
    i <- which(rownames(matriz_sparse) == input$cliente_id)
    
    #Obtener los factores latentes del cliente
    u <- user_factors[i, , drop = FALSE]
    scores <- u %*% item_factors
    
    #Excluir productos ya comprados
    productos_comprados <- which(matriz_sparse[i, ] != 0)
    scores[productos_comprados] <- -Inf
    
    #Seleccionar top 5 productos recomendados
    top_idx <- head(order(scores, decreasing = TRUE), 5)
    productos <- colnames(matriz_sparse)[top_idx]
    puntuaciones <- round(scores[1, top_idx], 3)
    
    data.frame(Producto = productos, Puntuacion = puntuaciones, row.names = NULL)
  })
  
  output$tabla_recomendaciones <- renderTable({
    obtener_recomendaciones()
  })
}

shinyApp(ui = ui, server = server)

#Graficos
library(ggplot2)
library(gridExtra)
library(grid)

graficos <- lapply(recomendaciones, function(rec) {
  df <- rec$recomendaciones
  cliente <- rec$cliente_id
  
  ggplot(df, aes(x = reorder(descripcion, Puntuacion), y = Puntuacion)) +
    geom_col(fill = "#1f78b4") +
    coord_flip() +
    labs(
      title = paste("Recomendaciones para el cliente", cliente),
      x = "Producto",
      y = "Puntuación"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 15, r = 10, b = 10, l = 10), 
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7)
    )
})

do.call(grid.arrange, c(grobs = graficos, ncol = 2))

