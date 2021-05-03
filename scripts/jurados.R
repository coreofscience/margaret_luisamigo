# data cleaning (Jurado/Comisiones evaluaciones...)

grupo_df_Jurado <- 
  grupo_df %>%
  filter(categoria == "Jurado/Comisiones evaluadoras de trabajo de grado") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7"), 
           sep = "\r\n" ) %>% 
  select(-info_6) %>% 
  mutate(Nivel_Academico = str_remove(info_1, ":.*"),
         Nivel_Academico = str_remove(Nivel_Academico, ".*-"),
         Nivel_Academico = str_trim(Nivel_Academico),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
  mutate(Pais=str_trim(pais),
         ano=str_trim(anno),
         Idioma=str_remove(info_3,",.*"),
         Idioma=str_extract(Idioma,":.*"),
         Idioma=str_remove(Idioma,".*:"),
         Idioma=str_trim(Idioma),
         Medio_divulgacion=str_extract(info_3,"n:.*"),
         Medio_divulgacion=str_remove(info_3,".*:"),
         Medio_divulgacion=str_trim(Medio_divulgacion),
         Sitio_Web=str_remove(info_4,",.*"),
         Sitio_Web=str_remove(Sitio_Web,".*:"),
         Sitio_Web=str_trim(Sitio_Web),
         Nombre_del_Orientado=str_extract(info_4,",.*"),
         Nombre_del_Orientado=str_extract(Nombre_del_Orientado,":.*"),
         Nombre_del_Orientado=str_remove(Nombre_del_Orientado,":"),
         Nombre_del_Orientado=str_trim(Nombre_del_Orientado),
         Programa_Academico=str_remove(info_5,",.*"),
         Programa_Academico=str_remove(Programa_Academico,".*:"),
         Programa_Academico=str_trim(Programa_Academico),
         Institucion=str_extract(info_5,",.*"),
         Institucion=str_extract(Institucion,":.*"),
         Institucion=str_remove(Institucion,":"),
         Institucion=str_remove(Institucion,".$"),
         Institucion=str_trim(Institucion),
         Autores=str_remove(info_7,".*:"),
         Autores=str_trim(Autores)) %>% 
  select(-info_1,-info_3,-info_4,-info_5,-info_7,-pais,-anno)
