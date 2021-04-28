#Data cleaning "Generación de Contenido Impreso"

grupo_df_generacion_cont_impreso <- 
  grupo_df %>%
  filter(categoria == "Generación de Contenido Impreso") %>% 
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
         fecha = str_remove(info_2, ", Ambito.*"),
         fecha = str_trim(fecha),
         ambito = str_remove(info_2, ".*: "),
         ambito = str_trim(ambito),
         ambito = str_remove(ambito, ","),
         medio_circulacion = str_remove(info_3, " Lugar.*"),
         medio_circulacion = str_remove(medio_circulacion, ".*: "),
         medio_circulacion = str_trim(medio_circulacion),
         lugar_publicacion = str_remove(info_3, ".*:"),
         lugar_publicacion = str_remove(lugar_publicacion, ","),
         lugar_publicacion = str_trim(lugar_publicacion),
         sitio_web = str_remove(info_4, ".*web: "),
         autores = str_remove(info_6, ".*Autores: "),
         autores = str_trim(autores)) %>% 
  select(-info_1, -info_2, -info_3, -info_4, -info_6)