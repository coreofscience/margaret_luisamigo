grupo_df_generacion_contenido_virtual <- 
  grupo_df%>%
  filter(categoria == "Generación de Contenido Virtual") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Medio = str_remove(info_1, ":.*"),
         Medio = str_remove(Medio, ".*-"),
         Medio = str_trim(Medio),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>%
  mutate(info_2=str_trim(info_2),
         Fecha= str_remove(info_2, ",.*"),
         Fecha= str_trim(Fecha),
         Entidades_vinculadas= str_extract(info_2, ",.*"),
         Entidades_vinculadas= str_remove(Entidades_vinculadas, "^,"),
         Entidades_vinculadas= str_remove(Entidades_vinculadas, ".*:"),
         Entidades_vinculadas= str_remove(Entidades_vinculadas, ",$")) %>% 
  select(-info_2) %>% 
  mutate(info_3=str_trim(info_3),
         Sitio_web= str_remove(info_3, "Sitio web:"),
         Sitio_web= str_trim(Sitio_web))%>% 
  select(-info_3,-info_4) %>% 
  mutate(info_5=str_trim(info_5),
         Autores= str_remove(info_5, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_5) 




