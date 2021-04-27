grupo_df_nuevos_registros_cientificos<- 
  grupo_df %>%
  filter(categoria == "Nuevos registros científicos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8","info_9"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1, -info_2) %>%
  mutate(info_3= str_trim(info_3),
         Año= str_remove(info_3, ",.*"),
         Año= str_remove(Año, ".*:"),
         Año= str_trim(Año),
         Mes= str_extract(info_3, ",.*"),
         Mes= str_remove(Mes, "^,"),
         Mes= str_remove(Mes, ",.*"),
         Mes= str_remove(Mes, ".*:"),
         Mes= str_trim(Mes),
         Ciudad= str_extract(info_3, ",.*"),
         Ciudad= str_remove(Ciudad, "^,"),
         Ciudad= str_extract(Ciudad, ",.*"),
         Ciudad= str_remove(Ciudad, "^,"),
         Ciudad= str_remove(Ciudad, ".*:"),
         Ciudad= str_trim(Ciudad)) %>% 
  select(-info_3, -info_4) %>% 
  mutate(info_5= str_trim(info_5),
         Base_de_Datos= str_remove(info_5, ",.*"),
         Base_de_Datos= str_remove(Base_de_Datos, ".*:"),
         Base_de_Datos= str_trim(Base_de_Datos),
         Sitio_web= str_extract(info_5, ",.*"),
         Sitio_web= str_remove(Sitio_web, "^,"),
         Sitio_web= str_remove(Sitio_web, ",.*"),
         Sitio_web= str_remove(Sitio_web, ".*:"),
         Sitio_web= str_trim(Sitio_web),
         Institucion= str_extract(info_5, ",.*"),
         Institucion= str_remove(Institucion, "^,"),
         Institucion= str_extract(Institucion, ",.*"),
         Institucion= str_remove(Institucion, "^,"),
         Institucion= str_remove(Institucion, ".*:"),
         Institucion= str_trim(Institucion)) %>% 
  select(-info_5, -info_6) %>%
  mutate(info_7= str_trim(info_7),
         Instituccion_certificadora= str_remove(info_7, ",.*"),
         Instituccion_certificadora= str_remove(Instituccion_certificadora, ".*:"),
         Instituccion_certificadora= str_trim(Instituccion_certificadora),
         Descripcion_registro= str_extract(info_7, ",.*"),
         Descripcion_registro= str_remove(Descripcion_registro, "^,"),
         Descripcion_registro= str_extract(Descripcion_registro, ":.*"),
         Descripcion_registro= str_remove(Descripcion_registro, "^:"),
         Descripcion_registro= str_trim(Descripcion_registro)) %>% 
  select(-info_7, -info_8) %>%
  mutate(Descripcion=str_extract(info_9, ".*"),
         Descripcion= str_trim(Descripcion)) %>% 
  select(-info_9)