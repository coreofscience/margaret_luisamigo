# data cleaning (Regulaciones y Normas)

grupo_df_regulaciones_normas <- 
  grupo_df %>%
  filter(categoria == "Regulaciones y Normas") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"), 
           sep = "\r\n" ) %>% 
  select(-info_5) %>%
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo),
         pais = str_remove(info_2, ",.*"),
         pais = str_trim(pais),
         ano = str_trim(info_2),
         ano = str_remove(ano, ",$"),
         ano = str_remove(ano, ".*, "),
         ambito = str_remove(info_3, ", Fecha.*"),
         ambito = str_remove(ambito, ".*: "),
         fecha = str_remove(info_3, ".*publicación: "),
         fecha = str_trim(fecha),
         objeto = str_remove(info_4, ".*Objeto: "),
         objeto = str_trim(objeto),
         autores = str_remove(info_6, ".*Autores: ")) %>% 
  select(-info_1, -info_2, -info_3, -info_4, -info_6)