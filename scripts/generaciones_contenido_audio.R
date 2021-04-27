# data cleaning (Generaciónes de contenido de audio)

grupo_df_generacion_audio <- 
  grupo_df %>%
  filter(categoria == "Generaciónes de contenido de audio") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>% 
  select(-info_2, -info_4) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*\\d. "),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo),
         ano = str_remove(info_3, ", Mes.*"),
         ano = str_remove(ano, ".*: "),
         mes = str_remove(info_3, ", Ciudad.*"),
         mes = str_remove(mes, ".*: "),
         ciudad = str_remove(info_3, ".*Ciudad: "),
         formato_archivo_digital = str_remove(info_5, ", Descripcion.*"),
         formato_archivo_digital = str_remove(formato_archivo_digital, ".*: "),
         descripcion_audio = str_remove(info_5, ".*audio: ")) %>% 
  select(-info_1, -info_3, -info_5)