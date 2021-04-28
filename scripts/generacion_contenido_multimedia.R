grupo_df_generacion_contenido_multimedia <- 
  grupo_df%>%
  filter(categoria == "Generación de Contenido Multimedia") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Tipo_generacion = str_remove(info_1, ":.*"),
         Tipo_generacion = str_trim(Tipo_generacion),
         Tipo_generacion = str_remove(Tipo_generacion, ".*-"),
         Tipo_generacion = str_trim(Tipo_generacion),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>%
  mutate(info_2=str_trim(info_2),
         Año= str_remove(info_2, ",.*"),
         Año= str_trim(Año),
         Pais= str_extract(info_2, ",.*"),
         Pais= str_remove(Pais, "^,"),
         Pais= str_remove(Pais, ",$")) %>% 
  select(-info_2) %>% 
  mutate(info_3=str_trim(info_3),
         Idioma= str_remove(info_3, ".*:"),
         Idioma= str_trim(Idioma)) %>% 
  select(-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Medio_divulgacion= str_remove(info_4, ",.*"),
         Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
         Medio_divulgacion= str_trim(Medio_divulgacion),
         Sitio_web= str_extract(info_4, ",.*"),
         Sitio_web= str_remove(Sitio_web, "^,"),
         Sitio_web= str_remove(Sitio_web, ".*:"),
         Sitio_web= str_trim(Sitio_web)) %>% 
  select(-info_4) %>% 
  mutate(info_5=str_trim(info_5),
         Emisora= str_remove(info_5, ",.*"),
         Emisora= str_remove(Emisora, ".*:"),
         Emisora= str_trim(Emisora),
         Instituciones_participantes= str_extract(info_5, ",.*"),
         Instituciones_participantes= str_remove(Instituciones_participantes, "^,"),
         Instituciones_participantes= str_remove(Instituciones_participantes, ".*:"),
         Instituciones_participantes= str_trim(Instituciones_participantes)) %>% 
  select(-info_5,-info_6) %>% 
  mutate(info_7=str_trim(info_7),
         Autores= str_remove(info_7, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_7) 




