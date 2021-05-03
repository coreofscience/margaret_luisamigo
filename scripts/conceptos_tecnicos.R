#Data cleaning "COnceptos técnicos"

grupo_df_conceptos_tecnicos <- 
  grupo_df %>%
  filter(categoria == "Conceptos técnicos") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>%
  select(-info_2, -info_4) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*\\d."),
         tipo_producto = str_trim(tipo_producto),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo),
         ano_solicitud = str_remove(info_3, ", Mes.*"),
         ano_solicitud = str_remove(ano_solicitud, ".*: "),
         ano_solicitud = str_trim(ano_solicitud),
         mes_solicitud = str_remove(info_3, ", Fecha.*"),
         mes_solicitud = str_remove(mes_solicitud, ".*: "),
         fecha_envio = str_remove(info_3, ".*envío: "),
         institucion_solicitante = str_remove(info_5, ", Ciudad.*"),
         institucion_solicitante = str_remove(institucion_solicitante, ".*solicitante: "),
         ciudad = str_remove(info_5, ", Número.*"),
         ciudad = str_remove(ciudad, ".*Ciudad: "),
         numero_cosecutivo_concepto = str_remove(info_5, ".*concepto: ")) %>% 
  select(-info_1, -info_3, -info_5)