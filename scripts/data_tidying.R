library(tidyverse)
library(here)
library(gt)
library(treemapify)
library(kableExtra)
library(stringi)
library(tidytext)
library(SnowballC)
library(widyr)
library(igraph)

# Finding similarities per group 

data_tidying_ucla <- function(produccion_grupos) {
  
  categorias <- data.frame(cat = c("articulos","capitulos","libros","softwares","innovaciones_gestion","trabajos_dirigidos"),
                           titulo = c("titulo","titulo_capitulo","Titulo","titulo","titulo","titulo"))
  
  for(j in categorias$cat){
  
  df <- tibble(id = numeric())
  
  grupo_df_articulos <- # create an id
    produccion_grupos[[2]][[j]] %>%
    mutate(id = 1:length(produccion_grupos[[2]][[j]][["grupo"]]))
  
  titulonuevo <- categorias |> filter(cat == j) |> select(titulo)
  
  grupos <- 
    grupo_df_articulos %>% 
    select(grupo) %>% 
    unique
  
  for (i in grupos$grupo) {
    
    df_1 <-  
      grupo_df_articulos %>% 
      filter(grupo == i) %>% 
      select(id, 
             titulonuevo$titulo)
    
    df_2 <- 
      df_1 %>% 
      unnest_tokens(output = "words",
                    input = titulonuevo$titulo,
                    token = "words") %>% 
      count(id, words) %>% 
      pairwise_similarity(item = id, 
                          feature = words, 
                          value = n)
    
    df_3 <- 
      df_2 %>% 
      filter(similarity >= 0.70) %>% 
      rename(Source = "item1",
             Target = "item2",
             weight = "similarity") %>% 
      graph_from_data_frame(directed = FALSE) %>% 
      simplify()
    
    df_4 <- 
      cbind(get.edgelist(df_3),
            E(df_3)$weight/2) %>% 
      as.data.frame() %>% 
      select(V2) %>% 
      rename(id = "V2") %>% 
      mutate(id = as.numeric(id))
    
    df_5 <- 
      df_1 %>% 
      anti_join(df_4) %>% 
      select(-titulonuevo$titulo)
    
    df <- 
      df %>% 
      bind_rows(df_5)
  }
  
  # Select unique values from grupo_df_articulos
  
  grupo_df_articulos_unicos <- 
    grupo_df_articulos %>% 
    inner_join(df)
  
  produccion_grupos[[2]][[j]] <- grupo_df_articulos_unicos
  
  }
  
  return(produccion_grupos)
  
}
