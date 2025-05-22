library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(tidyr)
library(corrplot)
library(factoextra)
library(ggpubr)
library(reshape2)
library(visdat)
library(lubridate)

maestroestr<-readRDS("Datos/Originales/maestroestr.RDS")
tickets_enc<-readRDS("Datos/Originales/tickets_enc.RDS")
objetivos<-readRDS("Datos/Originales/objetivos.RDS")


# Función para detectar y reemplazar outliers por NA usando IQR
reemplazar_outliers_na <- function(df) {
  numericas <- df %>% select(where(is.numeric))
  
  for (col in names(numericas)) {
    x <- df[[col]]
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_inf <- Q1 - 1.5 * IQR
    limite_sup <- Q3 + 1.5 * IQR
    
    # Reemplazar outliers con NA
    x[x < limite_inf | x > limite_sup] <- NA
    df[[col]] <- x
  }
  
  return(df)
}

# Aplicar la función a ambos datasets
tickets_enc_limpio <- reemplazar_outliers_na(tickets_enc)



cat("\nResumen tickets_enc (con outliers como NA):\n")
print(summary(tickets_enc_limpio))

#######################3


str(tickets_enc)

colSums(is.na(tickets_enc))

range(tickets_enc$dia)

tickets_enc <- tickets_enc %>%
  mutate(dia = as.Date(as.character(dia), format = "%Y%m%d"))

tickets_enc %>%
  duplicated() %>%
  sum()

# 3. Agrupaciones 
   compras_por_dia <- tickets_enc %>%
  group_by(dia) %>%
  summarise(compras = n())

# Gráfico de compras por día
ggplot(compras_por_dia, aes(x = dia, y = compras)) +
  geom_line(color = "steelblue") +
  labs(title = "Compras por Día", x = "Fecha", y = "Número de Compras")

# Agrupación por ticket (cuántos productos tiene cada ticket)
productos_por_ticket <- tickets_enc %>%
  group_by(num_ticket) %>%
  summarise(num_productos = n())

# Agrupación por producto (frecuencia de productos comprados)
productos_mas_comprados <- tickets_enc %>%
  group_by(cod_est) %>%
  summarise(veces_comprado = n()) %>%
  arrange(desc(veces_comprado))

# Agrupación por cliente (cuántos tickets/compras tiene cada cliente)
compras_por_cliente <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(compras = n()) %>%
  arrange(desc(compras))

# 4. Opcional: Mostrar los 10 principales en cada agrupación
head(compras_por_dia, 10)
head(productos_por_ticket, 10)
head(productos_mas_comprados, 10)
head(compras_por_cliente, 10)



