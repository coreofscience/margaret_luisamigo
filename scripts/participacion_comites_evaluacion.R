#Data cleaning "Participación en comités de evaluación"

grupo_df_participacion_comites <- 
  grupo_df %>%
  filter(categoria == "Participación en comités de evaluación") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"), 
           sep = "\r\n" ) %>% 
  select(-info_5) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
  mutate(Pais = str_trim(pais),
         ano = str_trim(anno),
         sitio_web = str_remove(info_3,".*web"),
         sitio_web = str_remove(sitio_web,"^:"),
         sitio_web = str_trim(sitio_web),
         medio_divulgacion = str_remove(info_4,",.*"),
         medio_divulgacion = str_remove(medio_divulgacion,".*:"),
         medio_divulgacion = str_trim(medio_divulgacion),
         institucion = str_remove(info_4,".*,"),
         institucion = str_remove(institucion,".*:"),
         institucion = str_trim(institucion),
         autores = str_remove(info_6,".*:"),
         autores = str_trim(autores)) %>% 
  select(-info_1,-pais,-anno,-info_3,-info_4,-info_6)