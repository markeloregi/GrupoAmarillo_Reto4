crear_nuevo_numero_ticket <- function(data) {
  data_nueva<<-data %>%
    mutate(grupo = paste(num_ticket, id_cliente_enc, sep = "_")) %>%
    mutate(nuevo_num_ticket = as.integer(factor(grupo))) %>%
    select(-grupo)
  return(data_nueva)
}