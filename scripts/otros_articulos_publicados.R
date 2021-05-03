#Data cleaning "Otros articulos publicados"

  grupo_df_otros_articulos <- 
    grupo_df %>%
    filter(categoria == "Otros artículos publicados") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    unite(info_2,"info_2",c("info_2","info_3"),sep = "",remove = TRUE) %>% 
    unite(info_4,"info_4",c("info_4","info_5"),sep = "",remove = TRUE) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           info_2 = str_extract(info_2,",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = str_remove(info_2,"ISSN.*"),
           revista = str_trim(revista),
           info_2 = str_extract(info_2, "ISSN.*"),
           info_2 = str_trim(info_2),
           ISSN = str_remove(info_2,",.*"),
           ISSN = str_extract(ISSN,":.*"),
           ISSN = str_remove(ISSN, "^:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_2,",.*"),
           ano = str_remove(info_2,"vol.*"),
           ano = str_remove(ano,"^,"),
           ano = str_trim(ano),
           info_2 = str_extract(info_2,"vol.*"),
           vol = str_remove(info_2,"fasc.*"),
           vol = str_remove(vol,".*:"),
           vol = str_remove(vol,"^:"),
           vol = str_trim(vol),
           fasc = str_extract(info_2,".*p"),
           fasc = str_remove(fasc,"p$"),
           fasc = str_extract(fasc,"c.*"),
           fasc = str_extract(fasc,":.*"),
           fasc = str_remove(fasc,"^:"),
           fasc = str_trim(fasc),
           pags = str_extract(info_2,"gs.*"),
           pags = str_extract(pags,":.*"),
           pags = str_remove(pags,"^:"),
           pags = str_trim(pags),
           autores = str_remove(info_4, "Autores: "),
           autores = str_trim(autores)) %>% 
    select(-info_1,-info_2,-info_4) 
  