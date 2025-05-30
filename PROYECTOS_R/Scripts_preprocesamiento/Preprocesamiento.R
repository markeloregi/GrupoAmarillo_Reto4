
maestro<-readRDS("Datos/Originales/maestroestr.RDS")
objetivos<-readRDS("Datos/Originales/objetivos.RDS")
tickets_enc<-readRDS("Datos/Originales/tickets_enc.RDS")

#data discovering
str(maestro)
str(objetivos)
str(tickets_enc)

duplicados <- tickets_enc %>%
  group_by(num_ticket) %>%
  summarise(duplicados = sum(duplicated(id_cliente_enc)))

sum(duplicados$duplicados)
unique(tickets_enc$num_ticket)

duplicados_prueba <- tickets_enc %>%
  group_by(num_ticket) %>%
  summarise(clientes_unicos = n_distinct(id_cliente_enc)) %>% 
  filter(clientes_unicos>1)


crear_nuevo_numero_ticket(tickets_enc)



### Pruebas para reducción de dimensionalidad

contador <- data_nueva %>%
  group_by(nuevo_num_ticket) %>%
  summarise(productos_comprados = paste(cod_est, collapse = ", "), .groups = 'drop')

contador <- contador %>% 
  mutate(counter=str_count(productos_comprados,",")+1)

unico_producto<-contador %>% 
  filter(counter==1)

mayores_compradores<-contador %>% 
  filter(counter>40) %>% 
  arrange(desc(counter))
