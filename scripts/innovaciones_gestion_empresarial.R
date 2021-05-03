#Data cleaning "Innovaciones generadas en la Gestión Empresarial"

grupo_df_innov_gestion_empresarial <- 
  grupo_df %>%
  filter(categoria == "Innovaciones generadas en la Gestión Empresarial") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>% 
  select(-info_4) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
  mutate(Pais = str_trim(pais),
         ano = str_trim(anno),
         disponibilidad = str_remove(info_3,",.*"),
         disponibilidad = str_remove(disponibilidad,".*:"),
         disponibilidad = str_trim(disponibilidad),
         info_3 = str_extract(info_3, ",.*"),
         info_3 = str_extract(info_3, ":.*"),
         info_3 = str_remove(info_3, "^:"),
         institucion_financiadora = str_trim(info_3),
         autores = str_remove(info_5,".*:"),
         autores = str_trim(autores)) %>% 
  select(-info_1,-pais,-anno,-info_3,-info_5)
