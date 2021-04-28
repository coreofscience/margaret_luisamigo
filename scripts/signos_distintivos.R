#Data cleaning "Signos distintivos"

grupo_df_signos_distintivos <- 
  grupo_df %>%
  filter(categoria == "Signos distintivos") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3"), 
           sep = "\r\n" ) %>%
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
         tipo_producto = str_trim(tipo_producto),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo),
         pais = str_remove(info_2, ",.*"),
         pais = str_trim(pais),
         ano = str_trim(info_2),
         ano = str_remove(ano, ",$"),
         ano = str_remove(ano, ".*, "),
         numero_registro = str_remove(info_3, ", Nombre.*"),
         numero_registro = str_remove(numero_registro, ".*: "),
         nombre_titular = str_remove(info_3, ".*titular: ")) %>% 
  select(-info_1, -info_2, -info_3)