#Data cleaning "Softwares"

grupo_df_softwares <- 
  grupo_df %>%
  filter(categoria == "Softwares") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6", "info_7"), 
           sep = "\r\n" ) %>% 
  select(-info_6) %>%
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
  mutate(Pais = str_trim(pais),
         ano = str_trim(anno),
         disponibilidad = str_remove(info_3, ",.*"),
         disponibilidad = str_remove(disponibilidad, ".*:"),
         disponibilidad = str_remove(disponibilidad, "^:"),
         disponibilidad = str_trim(disponibilidad),
         sitio_web = str_remove(info_3, ".*web:"),
         sitio_web = str_trim(sitio_web),
         nombre_comercial = str_remove(info_4, ",.*"),
         nombre_comercial = str_remove(nombre_comercial, "Nombre comercial: "),
         nombre_comercial = str_trim(nombre_comercial),
         nombre_proyecto = str_remove(info_4, ".*Nombre del proyecto"),
         nombre_proyecto = str_remove(nombre_proyecto, "^:"),
         nombre_proyecto = str_trim(nombre_proyecto),
         institucion_financiadora = str_remove(info_5, ".*:"),
         institucion_financiadora = str_trim(institucion_financiadora),
         autores = str_remove(info_7, ".*Autores: ")) %>% 
  select(-info_1,-pais,-anno,-info_3,-info_4,-info_5,-info_7)