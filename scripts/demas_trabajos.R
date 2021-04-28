grupo_df_demas_trabajos <- 
  grupo_df%>%
  filter(categoria == "Demás trabajos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5"), 
           sep = "\r\n" )%>% 
  mutate(info_1 = str_trim(info_1),
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
         Idioma= str_remove(info_3, ",.*"),
         Idioma= str_remove(Idioma, ".*:"),
         Idioma= str_trim(Idioma),
         Medio_divulgacion= str_extract(info_3, ",.*"),
         Medio_divulgacion= str_remove(Medio_divulgacion, "^,"),
         Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
         Medio_divulgacion= str_trim(Medio_divulgacion)) %>% 
  select(-info_3, -info_4) %>% 
  mutate(info_5=str_trim(info_5),
         Autores= str_remove(info_5, ".*:"),
         Autores= str_trim(Autores)) %>% 
  select(-info_5) 
  
  


