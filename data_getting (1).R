library(tidyverse)
library(rvest)
library(here)

# data getting

grupos <- 
  read_csv("data/grupos.csv")

grupo_df <- 
  tibble(grupo = character(),
         producto = character(),
         categoria = character())

for (i in 1:length(grupos$url)) {
  
  grupo <- 
    read_html(grupos$url[i]) %>% 
    html_table()
  
  for (j in 14:71) {
    
    df_1 = 
      grupo %>% 
      tibble() %>% 
      slice(j) %>% 
      unlist %>% 
      tibble() %>% 
      rename(producto = ".") %>% 
      mutate(grupo = grupos$grupo[i])
    
    if (length(df_1$producto) > 1) {
      
      df_2 =
        df_1 %>% 
        filter(producto != "") %>% 
        mutate(categoria = df_1$producto[1]) %>% 
        filter(str_detect(producto, "^[0-9]\\.*")) %>% 
        select(grupo, producto, categoria)
      
    } else {
      
      df_2 = 
        df_1 %>% 
        mutate(categoria = df_1$producto[1],
               producto = "NO TIENE") %>% 
        select(grupo, producto, categoria) %>% 
        unique()
      
    }
    
    grupo_df <- 
      bind_rows(df_2,
                grupo_df) 
    
  }
}

rm(df_1, df_2, grupo, i, j)

# data cleaning

grupo_df_articulos_lost_1 <- 
  grupo_df %>%
  filter(categoria == "Artículos publicados") %>% 
  separate(producto,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>% 
  filter(!is.na(info_5)) %>% 
  slice(1) %>% 
  select(-info_4) %>% 
  mutate(info_2 = str_trim(info_2),
         info_4 = str_trim(info_3),
         tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  select(-info_1) %>% 
  mutate(pais_revista = str_remove(info_2, ",.*"),
         info_2 = str_extract(info_2, ",.*"),
         info_2 = str_remove(info_2, "^,"),
         info_2 = str_trim(info_2),
         revista = info_2,
         info_2 = str_extract(info_4, "ISSN.*"),
         info_2 = str_trim(info_4),
         ISSN = str_remove(info_4, ",.*"),
         info_2 = str_extract(info_4, ",.*"),
         info_2 = str_remove(info_4, "^,"),
         info_2 = str_trim(info_4),
         ano = str_remove(info_4, "\\s.*"),
         info_2 = str_extract(info_4, "\\s.*"),
         info_2 = str_trim(info_4),
         vol = str_remove(info_4, "\\s.*"),
         vol = str_remove(vol, "vol:"),
         info_2 = str_extract(info_4, "\\s.*"),
         info_2 = str_trim(info_4),
         fasc = str_remove(info_4, "págs.*"),
         fasc = str_remove(fasc, "fasc: "),
         info_2 = str_extract(info_4, "págs.*"),
         info_2 = str_trim(info_4),
         pags = str_remove(info_4, ", DOI.*"),
         pags = str_remove(pags, "págs: "),
         DOI = str_extract(info_4, "DOI.*"),
         DOI = str_remove(DOI, "DOI:")) %>% 
  select(-info_4,
         -info_2) %>% 
  mutate(autores = str_remove(info_5, "Autores: "),
         autores = str_remove(autores, ",$"),
         autores = str_trim(autores)) %>% 
  select(-info_5,
         -info_3)

grupo_df_articulos_lost_2 <- 
  grupo_df %>%
  filter(categoria == "Artículos publicados") %>% 
  separate(producto,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>% 
  filter(!is.na(info_5)) %>% 
  slice(2) %>% 
  select(-info_4) %>%
  mutate(info_2 = str_trim(info_2),
         info_5 = str_trim(info_5),
         tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  select(-info_1) %>% 
  mutate(pais_revista = str_remove(info_2, ",.*"),
         pais_revista = str_remove(pais_revista, ".*,"),
         revista = str_remove(info_3, "ISSN.*"),
         info_3 = str_extract(info_3, "ISSN.*"),
         info_3 = str_trim(info_3),
         ISSN = str_remove(info_3, ",.*"),
         info_3 = str_extract(info_3, ",.*"),
         info_3 = str_remove(info_3, "^,"),
         info_3 = str_trim(info_3),
         ano = str_remove(info_3, "\\s.*"),
         info_3 = str_extract(info_3, "\\s.*"),
         info_3 = str_trim(info_3),
         vol = str_remove(info_3, "\\s.*"),
         vol = str_remove(vol, "vol:"),
         info_3 = str_extract(info_3, "\\s.*"),
         info_3 = str_trim(info_3),
         fasc = str_remove(info_3, "págs.*"),
         fasc = str_remove(fasc, "fasc: "),
         info_3 = str_extract(info_3, "págs.*"),
         info_3 = str_trim(info_3),
         pags = str_remove(info_3, ", DOI.*"),
         pags = str_remove(pags, "págs: "),
         DOI = str_extract(info_3, "DOI.*"),
         DOI = str_remove(DOI, "DOI:")) %>% 
  select(-info_2,
         -info_3) %>% 
  mutate(autores = str_remove(info_5, "Autores: "),
         autores = str_remove(autores, ",$"),
         autores = str_trim(autores)) %>% 
  select(-info_5) %>% 
  mutate(DOI = str_remove(DOI, "http://dx.doi.org/"))

grupo_df_articulos <- 
  grupo_df %>%
  filter(categoria == "Artículos publicados") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5"), 
           sep = "\r\n" ) %>% 
  filter(is.na(info_5)) %>% 
  select(-info_3,
         -info_5) %>% 
  mutate(info_2 = str_trim(info_2),
         info_4 = str_trim(info_4),
         tipo_producto = str_remove(info_1, ":.*"),
         tipo_producto = str_remove(tipo_producto, ".*-"),
         tipo_producto = str_trim(tipo_producto),
         titulo = str_extract(info_1, ":.*"),
         titulo = str_remove(titulo, "^:"),
         titulo = str_trim(titulo)) %>% 
  select(-info_1) %>% 
  mutate(pais_revista = str_remove(info_2, ",.*"),
         info_2 = str_extract(info_2, ",.*"),
         info_2 = str_remove(info_2, "^,"),
         info_2 = str_trim(info_2),
         revista = str_remove(info_2, "ISSN.*"),
         info_2 = str_extract(info_2, "ISSN.*"),
         info_2 = str_trim(info_2),
         ISSN = str_remove(info_2, ",.*"),
         info_2 = str_extract(info_2, ",.*"),
         info_2 = str_remove(info_2, "^,"),
         info_2 = str_trim(info_2),
         ano = str_remove(info_2, "\\s.*"),
         info_2 = str_extract(info_2, "\\s.*"),
         info_2 = str_trim(info_2),
         vol = str_remove(info_2, "\\s.*"),
         vol = str_remove(vol, "vol:"),
         info_2 = str_extract(info_2, "\\s.*"),
         info_2 = str_trim(info_2),
         fasc = str_remove(info_2, "págs.*"),
         fasc = str_remove(fasc, "fasc: "),
         info_2 = str_extract(info_2, "págs.*"),
         info_2 = str_trim(info_2),
         pags = str_remove(info_2, ", DOI.*"),
         pags = str_remove(pags, "págs: "),
         DOI = str_extract(info_2, "DOI.*"),
         DOI = str_remove(DOI, "DOI:")) %>% 
  select(-info_2) %>% 
  mutate(autores = str_remove(info_4, "Autores: "),
         autores = str_remove(autores, ",$")) %>% 
  select(-info_4) %>% 
  bind_rows(grupo_df_articulos_lost_1,
            grupo_df_articulos_lost_2)

grupo_df_Proyectos <- 
  grupo_df %>%
  filter(categoria == "Proyectos") %>% 
  separate(producto,
           c("Nombre", "Pais", " año inicio", "año final", "Autores"), 
           sep = "\r\n" )
rm(grupo_df_articulos_lost_1,
   grupo_df_articulos_lost_2)
