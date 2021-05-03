trabajos_dirigidos_ucla <- function(grupos_df) {
  
  trabajosdirigidos = 
    grupo_df %>%
    filter(categoria == "Trabajos dirigidos/turorías") %>% 
    separate(producto,
             c("info_1", 
               "info_2", 
               "info_3", 
               "info_4", 
               "info_5", 
               "info_6", 
               "info_7",
               "info_8"), 
             sep = "\r\n" ) %>% 
    select(-info_7) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(desde = str_trim(info_2),
           hasta = str_trim(info_3),
           estudiante = str_trim(info_4),
           paginas = str_trim(info_5), 
           institucion = str_trim(info_6),
           autor = str_trim(info_8)) %>% 
    select(-info_2,
           -info_3,
           -info_4,
           -info_5,
           -info_6,
           -info_8) %>% 
    separate(hasta, 
             c("hasta",
               "tipo_orientacion"),
             sep = ", ")
  
  return(trabajosdirigidos)
  
}
