grupo_df_otros_libros_publicados<- 
  grupo_df %>%
  filter(categoria == "Otros Libros publicados") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
         Tipo_libro = str_remove(info_1, ":.*"),
         Tipo_libro = str_remove(Tipo_libro, ".*- "),
         Tipo_libro = str_trim(Tipo_libro),
         Titulo= str_extract(info_1, ":.*"),
         Titulo= str_remove(Titulo, "^:"),
         Titulo= str_trim(Titulo)) %>% 
  select(-info_1) %>% 
  mutate(info_2 = str_trim(info_2),
         Pais = str_remove(info_2,",.*"),
         Pais = str_trim(Pais),
         Año = str_extract(info_2, ",.*"),
         Año = str_remove(Año, ","),
         Año = str_trim(Año),
         Año = str_remove(Año, ",.*"),
         ISBN= str_extract(info_2, "ISBN.*"),
         ISBN= str_remove(ISBN, "vol.*"),
         ISBN= str_remove(ISBN, ".*:"),
         ISBN = str_trim(ISBN),
         Volumen = str_extract(info_2,"vol:.*"),
         Volumen = str_remove(Volumen, "págs:.*"),
         Volumen= str_remove(Volumen, "vol:"),
         Volumen=str_trim(Volumen),
         Paginas= str_extract(info_2,"págs:.*,"),
         Paginas= str_remove(Paginas, ",.*"),
         Paginas= str_remove(Paginas, "págs:"),
         Paginas=str_trim(Paginas),
         Editorial= str_extract(info_2,"Ed.*"),
         Editorial= str_remove(Editorial,"Ed."),
         Editorial=str_trim(Editorial)) %>% 
  select(-info_2,-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Autores=str_extract(info_4, ".*"),
         Autores=str_remove(Autores, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_4)