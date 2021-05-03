grupo_df_otros_productos_tecnologicos<- 
  grupo_df %>%
  filter(categoria == "Otros productos tecnológicos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>%
  mutate(info_2= str_trim(info_2),
         Pais= str_remove(info_2, ",.*"),
         Año= str_extract(info_2, ",.*"),
         Año= str_remove(Año, "^,"),
         Año= str_remove(Año, ","),
         Año= str_trim(Año)) %>% 
  select(-info_2) %>% 
  mutate(info_3 = str_trim(info_3),
         Disponibilidad= str_remove(info_3, ",.*"),
         Disponibilidad= str_remove(Disponibilidad, ".*:"),
         Disponibilidad= str_trim(Disponibilidad),
         Nombre_comercial= str_extract(info_3, ",.*"),
         Nombre_comercial= str_remove(info_3, "^,"),
         Nombre_comercial= str_remove(Nombre_comercial, ".*:"),
         Nombre_comercial= str_trim(Nombre_comercial)) %>% 
  select(-info_3) %>% 
  mutate(info_4= str_trim(info_4),
         Institucion_Financiadora= str_remove(info_4, ".*:"),
         Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>% 
  select(-info_4,-info_5) %>% 
  mutate(info_6= str_trim(info_6),
         Autores= str_remove(info_6, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_6)
         