#Data cleaning "Documentos de trabajo"

grupo_df_documentos_trabajo <- 
  grupo_df %>%
  filter(categoria == "Documentos de trabajo") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", 
             "info_6", "info_7", "info_8", "info_9", "info_10", "info_11"), 
           sep = "\r\n" ) %>% 
  unite(info_4,"info_4",c("info_4","info_5", "info_6", "info_7", "info_8", 
                          "info_9", "info_10", "info_11"),sep = " ",remove = TRUE) %>%
  separate(info_4, c("info_4","info_5"), sep = "URL") %>% 
  separate(info_5, c("info_5", "info_6", "info_7"), sep = "DOI") %>%
  unite(info_6, "info_6", c("info_6", "info_7"),sep = "",remove = TRUE) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo),
         ano = str_remove(info_2, ",$"),
         ano = str_trim(ano),
         numero_paginas = str_remove(info_3, ".*:"),
         numero_paginas = str_remove(numero_paginas, ",$"),
         numero_paginas = str_trim(numero_paginas),
         instituciones_participantes = str_remove(info_4, ".*:"),
         instituciones_participantes = str_remove(instituciones_participantes,","),
         instituciones_participantes = str_trim(instituciones_participantes),
         instituciones_participantes = str_remove(instituciones_participantes,",$"),
         URL = str_remove(info_5, "^:"),
         URL = str_remove(URL, ","),
         DOI = str_remove(info_6, "Autores.*"),
         DOI = str_remove(DOI, ":"),
         DOI = str_remove(DOI, ":"),
         DOI = str_trim(DOI),
         autores = str_remove(info_6, ".*Autores: "),
         autores = str_remove(autores, " NA .*")) %>% 
  select(-info_1,-info_2,-info_3,-info_4,-info_5,-info_6)