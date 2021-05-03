grupo_df_espacio_participacion_ciudadano<- 
  grupo_df %>%
  filter(categoria == "Espacios de Participación Ciudadana") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Titulo = str_extract(info_1, ".*en"),
         Titulo = str_remove(Titulo, " en$"),
         Titulo = str_remove(Titulo, ".*- "),
         Titulo = str_trim(Titulo),
         Ciudad = str_remove(info_1,".*en"),
         Ciudad = str_trim(Ciudad)) %>% 
  select(-info_1) %>% 
  mutate(info_2 = str_trim(info_2),
         Fecha_inicio = str_remove(info_2,"hasta.*"),
         Fecha_inicio = str_remove(Fecha_inicio, "desde"),
         Fecha_inicio = str_remove(Fecha_inicio, "- $"),
         Fecha_inicio =str_trim(Fecha_inicio),
         Fecha_Fin = str_extract(info_2, "hasta.*"),
         Fecha_Fin = str_remove(Fecha_Fin, "hasta"),
         Fecha_Fin = str_trim(Fecha_Fin)) %>% 
  select(-info_2) %>% 
  mutate(info_3 = str_trim(info_3),
         N_participantes= str_extract(info_3, ".*,"),
         N_participantes= str_remove(N_participantes, ".*:"),
         N_participantes= str_remove(N_participantes, ",$"),
         N_participantes= str_trim(N_participantes),
         Pag_web= str_extract(info_3, ",.*"),
         Pag_web= str_remove(Pag_web, "^,"),
         Pag_web= str_remove(Pag_web, ".*:"),
         Pag_web= str_trim(Pag_web)) %>% 
  select(-info_3)