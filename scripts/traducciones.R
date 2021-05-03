# data cleaning (Traducciones)

grupo_df_traducciones <- 
  grupo_df %>%
  filter(categoria == "Traducciones") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"), 
           sep = "\r\n" ) %>% 
  select(-info_8) %>%
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_remove(info_1, ".*: "),
         ano = str_remove(info_2, ",.*"),
         ano = str_trim(ano),
         revista = str_remove(info_2, ".*: "),
         revista = str_remove(revista, " ISSN.*"),
         revista = str_trim(revista),
         ISSN = str_remove(info_3, ", Libro.*"),
         ISSN = str_trim(ISSN), 
         libro = str_remove(info_3, ".*: "),
         libro = str_remove(libro, " ISBN.*"),
         ISBN = str_remove(info_4, ", Medio.*"),
         ISBN = str_trim(ISBN),
         medio_divulgacion = str_remove(info_4, ".*ción: "),
         idioma_documento_original = str_remove(info_5, ",.*"),
         idioma_documento_original = str_remove(idioma_documento_original, ".*: "),
         idioma_traduccion = str_remove(info_5, ".*ción: "),
         edicion = str_remove(info_6, ", Serie.*"),
         edicion = str_remove(edicion, ".*: "),
         serie = str_remove(info_6, ".*Serie: "),
         serie = str_remove(serie, ","),
         serie = str_trim(serie),
         autor_documento_original = str_remove(info_7, ".*: "),
         autores = str_remove(info_9, ".*: ")) %>% 
  select(-info_1, -info_2, -info_3, -info_4, -info_5, -info_6, -info_7, -info_9)