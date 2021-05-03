grupo_df_informes_tecnicos<- 
  grupo_df %>%
  filter(categoria == "Informes técnicos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1, -info_2) %>%
  separate(info_3,
           c("ano","mes","idioma","ciudad"),
           sep = ",") %>% 
  mutate(ano = str_trim(ano),
         ano= str_remove(ano, ".*:"),
         Ano= str_extract(ano, ".*"),
         mes= str_trim(mes),
         mes= str_remove(mes, ".*:"),
         Mes= str_extract(mes, ".*"),
         idioma = str_trim(idioma),
         idioma= str_remove(idioma, ".*:"),
         Idioma= str_extract(idioma, ".*"),
         ciudad = str_trim(ciudad),
         ciudad= str_remove(ciudad, ".*:"),
         Ciudad = str_extract(ciudad, ".*"),
         ) %>% 
  select(-info_4,-ano,-mes,-idioma,-ciudad) %>% 
  separate(info_5,
           c("i_1","i_2","i_3","i_4"),
           sep = ",") %>%
  mutate(Disponibilidad= str_extract(i_1, ".*"),
         Disponibilidad= str_remove(Disponibilidad, ".*:"),
         Disponibilidad= str_trim(Disponibilidad),
         Num_pag= str_extract(i_2, ".*"),
         Num_pag= str_remove(Num_pag, ".*:"),
         Num_pag= str_trim(Num_pag),
         Num_contrato= str_extract(i_3, ".*"),
         Num_contrato= str_remove(Num_contrato, ".*:"),
         Num_contrato= str_trim(Num_contrato),
         Institucion_Presta_servicio= str_extract(i_4, ".*"),
         Institucion_Presta_servicio= str_remove(Institucion_Presta_servicio, ".*:"),
         Institucion_Presta_servicio= str_trim(Institucion_Presta_servicio)) %>% 
  select(-i_1,-i_2,-i_3,-i_4)

