#Data cleaning "Participación Ciudadana en Proyectos de CTI"

grupo_df_participacion_cti <- 
  grupo_df %>%
  filter(categoria == "Participación Ciudadana en Proyectos de CTI") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"), 
           sep = "\r\n" ) %>% 
  unite("info_3", c("info_3", "info_4", "info_5","info_6", "info_7", "info_8", "info_9"),
        sep = "",remove = TRUE) %>%
  mutate(titulo = str_remove(info_1, ":.*"),
         titulo = str_remove(titulo, ".*\\d.-"),
         titulo = str_trim(titulo),
         desde = str_remove(info_1, ".*: desde"),
         desde = str_remove(desde, "hasta.*"),
         desde = str_trim(desde),
         hasta = str_trim(info_2),
         descripcion = str_remove(info_3, ".*:"),
         descripcion = str_remove(descripcion, "NANANA.*")) %>% 
  select(-info_1, -info_2, -info_3)