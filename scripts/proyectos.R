grupo_df_proyectos <- 
  grupo_df %>%
  filter(categoria == "Proyectos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Tipo_proyecto = str_remove(info_1, ":.*"),
         Tipo_proyecto = str_remove(Tipo_proyecto, ".*-"),
         Tipo_proyecto = str_trim(Tipo_proyecto),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1, -info_2) %>% 
  mutate(info_3= str_trim(info_3),
         Fecha_inicio= str_remove(info_3, "-$"),
         Fecha_inicio= str_trim(Fecha_inicio)) %>% 
  select(-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Fecha_Fin=str_extract(info_4, ".*")) %>% 
  select(-info_4,-info_5)