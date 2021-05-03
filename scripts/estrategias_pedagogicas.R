grupo_df_estrategias_pedagogicas <- 
  grupo_df %>%
  filter(categoria == "Estrategias Pedagógicas para el fomento a la CTI") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7",
             "info_8","info_9","info_10","info_11","info_12","info_13","info_14"
             ,"info_15","info_16","info_17","info_18","info_19","info_20","info_21",
             "info_22","info_23","info_24","info_25","info_26"),
           sep = "\r\n" ) %>% 
  unite( info_3,c(4:27),  sep = ",", remove = TRUE) %>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_remove(info_1, "desde.*"),
           Titulo = str_remove(Titulo, ".*- "),
           Titulo= str_remove(Titulo, ": $"),
           Titulo = str_trim(Titulo),
           Fecha_inicio= str_extract(info_1, "desde.*"),
           Fecha_inicio= str_remove(Fecha_inicio, "desde"),
           Fecha_inicio= str_remove(Fecha_inicio, "hasta"),
           Fecha_inicio= str_trim(Fecha_inicio)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Fecha_Fin= str_extract(info_2, ".*")) %>%
    select(-info_2) %>% 
    mutate(info_3= str_trim(info_3),
           Descripcion= str_remove(info_3, "Descripción:"),
           Descripcion= str_remove(Descripcion, "NA.*"),
           Descripcion= str_remove(Descripcion, ",$"),
           Descripcion= str_trim(Descripcion)) %>% 
    select(-info_3)
  
