grupo_df_ediciones <- 
  grupo_df%>%
  filter(categoria == "Ediciones") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6"), 
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
         Pais= str_remove(info_2, ",.*"),
         Pais= str_trim(Pais),
         Año= str_extract(info_2, ",.*"),
         Año= str_remove(Año, "^,"),
         Año= str_remove(Año, ",$")) %>% 
  select(-info_2) %>% 
  mutate(info_3=str_trim(info_3),
         Editorial=str_remove(info_3, ",.*"),
         Editorial=str_remove(Editorial, ".*:"),
         Editorial=str_trim(Editorial),
         Idioma= str_remove(info_3, ".*:"),
         Idioma= str_remove(Idioma, ",$"),
         Idioma= str_trim(Idioma)) %>% 
  select(-info_3) %>% 
  mutate(info_4=str_trim(info_4),
         Paginas= str_remove(info_4, ".*:"),
         Paginas= str_trim(Paginas)) %>% 
  select(-info_4,-info_5) %>% 
  mutate(info_6=str_trim(info_6),
         Autores= str_remove(info_6, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_6) 




