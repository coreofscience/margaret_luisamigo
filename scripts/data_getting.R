
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

# data cleaning (Jurado/Comisiones evaluaciones...)

grupo_df_Jurado <- 
  grupo_df %>%
  filter(categoria == "Jurado/Comisiones evaluadoras de trabajo de grado") %>% 
  separate(producto ,
           c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7"), 
           sep = "\r\n" ) %>% 
  select(-info_6) %>% 
  mutate(Nivel_Academico = str_remove(info_1, ":.*"),
         Nivel_Academico = str_remove(Nivel_Academico, ".*-"),
         Nivel_Academico = str_trim(Nivel_Academico),
         Titulo = str_extract(info_1, ":.*"),
         Titulo = str_remove(Titulo, "^:"),
         Titulo = str_trim(Titulo)) %>% 
  separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
  mutate(Pais=str_trim(pais),
         ano=str_trim(anno),
         Idioma=str_remove(info_3,",.*"),
         Idioma=str_extract(Idioma,":.*"),
         Idioma=str_remove(Idioma,".*:"),
         Idioma=str_trim(Idioma),
         Medio_divulgacion=str_extract(info_3,"n:.*"),
         Medio_divulgacion=str_remove(info_3,".*:"),
         Medio_divulgacion=str_trim(Medio_divulgacion),
         Sitio_Web=str_remove(info_4,",.*"),
         Sitio_Web=str_remove(Sitio_Web,".*:"),
         Sitio_Web=str_trim(Sitio_Web),
         Nombre_del_Orientado=str_extract(info_4,",.*"),
         Nombre_del_Orientado=str_extract(Nombre_del_Orientado,":.*"),
         Nombre_del_Orientado=str_remove(Nombre_del_Orientado,":"),
         Nombre_del_Orientado=str_trim(Nombre_del_Orientado),
         Programa_Academico=str_remove(info_5,",.*"),
         Programa_Academico=str_remove(Programa_Academico,".*:"),
         Programa_Academico=str_trim(Programa_Academico),
         Institucion=str_extract(info_5,",.*"),
         Institucion=str_extract(Institucion,":.*"),
         Institucion=str_remove(Institucion,":"),
         Institucion=str_remove(Institucion,".$"),
         Institucion=str_trim(Institucion),
         Autores=str_remove(info_7,".*:"),
         Autores=str_trim(Autores)) %>% 
  select(-info_1,-info_3,-info_4,-info_5,-info_7,-pais,-anno)
        






