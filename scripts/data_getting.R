
library(tidyverse)
library(rvest)
library(here)


# data getting

source(here("scripts",
            "data_grupos.R"))


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

grupo_df_articulos <- 
  grupo_df %>%
  filter(categoria == "Artículos publicados") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4"), 
           sep = "\r\n" ) %>% 
  select(-info_3) %>% 
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
  select(-info_4)


grupo_df_EventosCientificos <- 
  grupo_df %>%
  filter(categoria == "Eventos Científicos") %>% 
  separate(producto ,
           c("info_1", "info_2","info_3","info_4","info_5"), 
           sep = "\r\n" )%>% 
  mutate(info_2 = str_trim(info_2),
         info_4 = str_trim(info_4),
         Tipo_evento = str_remove(info_1, ":.*"),
         Tipo_evento = str_remove(Tipo_evento, ".*-"),
         Tipo_evento = str_trim(Tipo_evento),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  select(-info_1) %>% 
  mutate(Ciudad_evento= str_remove(info_2, ",.*"),
         info_2 = str_remove(info_2, ".*desde*"),
         Fecha_inicio = str_remove(info_2, "-$"),
         Fecha_Fin = str_remove(info_3, ".*hasta")) %>% 
  select(-info_2,-info_3) %>% 
  mutate(info_4 = str_remove(info_4, "Ámbito:"),
         Ámbito = str_remove(info_4, ",.*"),
         info_4= str_extract(info_4, "Tipos de participación:.*"),
         info_4= str_remove(info_4, ".*Tipos de participación:"),
         Tipo_participacion=str_remove(info_4,"Nombre de la institución.*"),
         info_4= str_extract(info_4, "Nombre de la institución.*"),
         Nombre_Institución= str_remove(info_4, ".*Nombre de la institución:")) %>% 
  select(-info_4) %>% 
  mutate(Tipo_vinculación = str_remove(info_5,"Nombre.*"),
         Tipo_vinculación = str_remove(Tipo_vinculación,"Ámbito.*")) %>% 
  select(-info_5)

  


