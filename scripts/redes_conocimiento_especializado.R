# data cleaning (Redes de Conocimiento Especializado)

grupo_df_redes_conocimiento <- 
  grupo_df %>%
  filter(categoria == "Redes de Conocimiento Especializado") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3"), 
           sep = "\r\n" ) %>% 
  mutate(titulo = str_remove(info_1, ":.*"),
         titulo = str_remove(titulo, ".*\\d.-"),
         titulo = str_trim(titulo),
         tipo_red = str_remove(info_1, ".*:"),
         tipo_red = str_trim(tipo_red),
         pais_ciudad = str_remove(info_2, ", desde.*"),
         pais_ciudad = str_remove(pais_ciudad, ".*en "),
         desde = str_remove(info_2, ".*desde "),
         desde = str_trim(desde),
         desde = str_remove(desde, "-$"),
         desde = str_trim(desde),
         hasta = str_remove(info_3, " Nro.*"),
         hasta = str_remove(hasta, ".*hasta "),
         numero_participantes = str_remove(info_3, ".*:"),
         numero_participantes = str_trim(numero_participantes)) %>% 
  select(-info_1, -info_2, -info_3)