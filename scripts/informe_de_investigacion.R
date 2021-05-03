grupo_df_informe_de_investigacion <- 
  grupo_df %>%
  filter(categoria == "Informes de investigación") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:" ),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>% 
  mutate(info_2 = str_trim(info_2),
         Año = str_remove(info_2, ",.*"),
         Año = str_trim(Año),
         Proyecto_de_investigacion = str_extract(info_2, ",.*" ),
         Proyecto_de_investigacion = str_extract(Proyecto_de_investigacion, ":.*"),
         Proyecto_de_investigacion = str_remove(Proyecto_de_investigacion, "^:"),
         Proyecto_de_investigacion = str_trim(Proyecto_de_investigacion)) %>% 
  select(-info_2,-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Autores=str_extract(info_4, ".*"),
         Autores=str_remove(Autores, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_4)
