#Data cleaning "Estrategias de Comunicación del Conocimiento"

grupo_df_estreategias_comunicacion <- 
  grupo_df %>%
  filter(categoria == "Estrategias de Comunicación del Conocimiento") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6", 
             "info_7", "info_8", "info_9", "info_10", "info_11", "info_12", 
             "info_13", "info_14", "info_15","info_16", "info_17", "info_18"), 
           sep = "\r\n" ) %>%
  unite("info_3", c("info_3", "info_4", "info_5", "info_6", 
                    "info_7", "info_8", "info_9", "info_10", "info_11", "info_12", 
                    "info_13", "info_14", "info_15","info_16", "info_17", "info_18"),
        sep = " ",remove = TRUE) %>% 
  mutate(tipo_producto = str_remove(info_1, ": desde.*"),
         tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
         tipo_producto = str_trim(tipo_producto),
         desde = str_remove(info_1, ".*desde "),
         desde = str_remove(desde, " hasta.*"),
         hata = str_trim(info_2),
         info_3 = str_remove(info_3, "NA.*"),
         descripcion = str_remove(info_3, ".*Descripción: "),
         descripcion = str_trim(descripcion)) %>% 
  select(-info_1, -info_2, -info_3)