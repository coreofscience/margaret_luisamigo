grupo_df_consultorias_cientico_tecnologicas <- 
  grupo_df%>%
  filter(categoria == "Consultorías científico-tecnológicas") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Tipo_Consultoria = str_remove(info_1, ":.*"),
         Tipo_Consultoria = str_trim(Tipo_Consultoria),
         Tipo_Consultoria = str_remove(Tipo_Consultoria, ".*\\d."),
         Tipo_Consultoria = str_remove(Tipo_Consultoria, "."),
         Tipo_Consultoria = str_trim(Tipo_Consultoria),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1, -info_2) %>% 
  separate(info_3,
           c("i_1","i_2","i_3","i_4"),
           sep = ",") %>%
  mutate(i_1 = str_trim(i_1),
         i_1= str_remove(i_1, ".*:"),
         Año_inicio= str_extract(i_1, ".*"),
         i_2= str_trim(i_2),
         i_2= str_remove(i_2, ".*:"),
         Mes_inicio= str_extract(i_2, ".*"),
         i_3 = str_trim(i_3),
         i_3= str_remove(i_3, ".*:"),
         Año_fin= str_extract(i_3, ".*"),
         i_4= str_trim(i_4),
         i_4= str_remove(i_4, ".*:"),
         Mes_fin= str_extract(i_4, ".*")) %>% 
  select(-i_1,-i_2,-i_3,-i_4) %>% 
  mutate(info_5=str_trim(info_5),
         Idioma= str_remove(info_5, ",.*"),
         Idioma= str_remove(Idioma, ".*:"),
         Idioma= str_trim(Idioma),
         Ciudad= str_extract(info_5, ",.*"),
         Ciudad= str_remove(Ciudad, "^,"),
         Ciudad= str_remove(Ciudad, ",.*"),
         Ciudad= str_remove(Ciudad, ".*:"),
         Ciudad= str_trim(Ciudad),
         Disponibilidad= str_extract(info_5, "Disponibilidad.*"),
         Disponibilidad= str_remove(Disponibilidad, ",.*"),
         Disponibilidad= str_remove(Disponibilidad, ".*:"),
         Disponibilidad= str_trim(Disponibilidad),
         Duracion= str_extract(info_5, "Duración.*"),
         Duracion= str_remove(Duracion, ".*:"),
         Duracion= str_remove(Duracion, ",$"),
         Duracion= str_trim(Duracion)
         ) %>% 
  select(-info_4,-info_5) %>% 
  mutate(info_6=str_trim(info_6),
         Num_contrato= str_remove(info_6, ",.*"),
         Num_contrato= str_remove(Num_contrato, ".*:"),
         Num_contrato= str_trim(Num_contrato),
         Institucion_Prestadora_servicio= str_extract(info_6, ",.*"),
         Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, "^,"),
         Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, ".*:"),
         Institucion_Prestadora_servicio= str_trim(Institucion_Prestadora_servicio)
  ) %>% 
  select(-info_6,-info_7) 

  