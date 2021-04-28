# data cleaning (Cap√≠tulos de libro publicados)

grupo_df_capitulos_libros_publicados <- 
  grupo_df %>%
  filter(categoria == "CapÌtulos de libro publicados") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4"), 
           sep = "\r\n" ) %>% 
  select(-info_3) %>% 
  mutate(tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo_capitulo = str_extract(info_1, ":.*"),
         titulo_capitulo = str_remove(titulo_capitulo, "^:"),
         titulo_capitulo = str_trim(titulo_capitulo),
         pais = str_remove(info_2, ", \\d.*"),
         pais = str_trim(pais),
         ano = str_remove(info_2, ", ISBN.*"),
         ano = str_extract(ano, ", .*"),
         ano = str_remove(ano, "^,"),
         ano = str_remove(ano, ",.*"),
         ano = str_trim(ano),
         titulo_libro = str_extract(info_2, "\\d, .*"),
         titulo_libro = str_remove(titulo_libro, ", ISBN.*"),
         titulo_libro = str_remove(titulo_libro, ".*, "),
         ISBN = str_remove(info_2, ".*ISBN: "),
         ISBN = str_remove(ISBN, ", Vol.*"), 
         vol = str_remove(info_2, ".*Vol."),
         vol = str_remove(vol, ", p·g.*"),
         pags = str_remove(info_2, ".*p·gs:"),
         pags = str_remove(pags, ",.*"),
         editorial = str_remove(info_2, ".*Ed. "),
         auotres = str_remove(info_4, ".*Autores: ")) %>% 
  select(-info_1, -info_2, -info_4)