grupo_df_otra_publicacion_divulgativa <- 
  grupo_df %>%
  filter(categoria == "Otra publicación divulgativa") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Tipo_Publicacion_divulgativa = str_remove(info_1, ":.*"),
         Tipo_Publicacion_divulgativa = str_remove(Tipo_Publicacion_divulgativa, ".*-" ),
         Tipo_Publicacion_divulgativa = str_trim( Tipo_Publicacion_divulgativa),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:" ),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>% 
  mutate(info_2 = str_trim(info_2),
         pais= str_remove(info_2, ",.*"),
         pais= str_trim(pais),
         año=str_extract(info_2, ",.*"),
         año = str_remove(año, "^,"),
         info_2=str_extract(año, ",.*"),
         año= str_remove(año, ",.*"),
         libro= str_remove(info_2, "^,"),
         libro= str_remove(libro, "vol.*"),
         libro= str_remove(libro, ",.$"),
         libro= str_extract(libro, ".*,"),
         libro= str_remove(libro, ",$"),
         libro=str_trim(libro),
         ISBN= str_remove(info_2, "^,"),
         ISBN= str_remove(ISBN, "vol.*"),
         ISBN= str_remove(ISBN, ",.$"),
         ISBN= str_remove(ISBN, ".*,"),
         ISBN= str_trim(ISBN),
         volumen=str_extract(info_2, "vol.*"),
         volumen=str_remove(volumen, ",.*"),
         volumen=str_remove(volumen, "vol."),
         volumen=str_trim(volumen),
         Paginas=str_extract(info_2, "págs.*"),
         Paginas=str_remove(Paginas, ",.*"),
         Paginas=str_remove(Paginas, ".*:"),
         Paginas=str_trim(Paginas),
         Informacion=str_extract(info_2, "págs.*"),
         Informacion=str_extract(Informacion, ",.*"),
         Informacion=str_remove(Informacion, "^,"),
         info_2=str_extract(Informacion, ".*"),
         Informacion=str_extract(Informacion, ".*,"),
         Informacion=str_remove(Informacion, ",$"),
         Informacion=str_trim(Informacion),
         Informacion=str_remove(Informacion, "^-"),
         Informacion=str_trim(Informacion),
         Editorial=str_remove(info_2, ".*,"),
         Editorial=str_remove(Editorial, "Ed."),
         Editorial=str_trim(Editorial)
         ) %>% 
  select(-info_2,-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Autores=str_extract(info_4, ".*"),
         Autores=str_remove(Autores, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_4)