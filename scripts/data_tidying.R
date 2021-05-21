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
library(rvest)

titulo_limpio<-function(categoria){
  grupo_nuevo=categoria
  grupo_df_nuevo<-
    grupo_nuevo %>% 
    mutate(titulo_limpio = str_to_upper(titulo),
           titulo_limpio = str_replace_all(titulo_limpio, 
                                           "[[:punct:]]", " "),
           titulo_limpio = stri_trans_general(titulo_limpio, 
                                              id = "Latin-ASCII"),
           titulo_limpio = str_replace_all(titulo_limpio,
                                           "  ", 
                                           " "),
           titulo_limpio = str_trim(titulo_limpio),
           id = 1:length(grupo_nuevo$categoria))
  return(grupo_df_nuevo)
}

similar<-function(grupo_similar){
  similares=grupo_similar
  grupo_df_similarity <- 
    similares %>% 
    select(titulo_limpio,
           id) %>% 
    unnest_tokens(output = "words",
                  input = titulo_limpio, 
                  token = "words") %>% 
    count(id, words) %>% 
    pairwise_similarity(item = id, 
                        feature = words, 
                        value = n)
  return(grupo_df_similarity)
}

#Ciclo para limpiar el titulo de todas las categorias
grupo_df_titulo_limpio<-NULL
pos<-1
while (pos<=41) {
    categoria=produccion_grupos[[pos]]
    grupo_df_no_duplicados=list()
    grupo_df_no_duplicados<-list(titulo_limpio(categoria))
    grupo_df_titulo_limpio<-c(grupo_df_titulo_limpio,grupo_df_no_duplicados)
    pos<-pos+1
}

#Ciclo para los item
grupo_df_similarity<-NULL
y<-1
while (y<=36) {
  grupo_similar=grupo_df_titulo_limpio[[y]]
  grupo_df_=list()
  grupo_df_<-list(similar(grupo_similar))
  grupo_df_similarity<-c(grupo_df_similarity,grupo_df_)  #
  y<-y+1
}


'''
#source("data_getting.R")
grupo_df_articulos <-  
  read_csv(here("data", 
                "grupos_produccion.csv"))

# La razón de los duplicados es porque dos investigadores pueden escribir un artículo para un mismo grupo pero le va a contar dos veces. Ejemplo 

grupo_df_articulos_no_duplicados <- 
  grupo_df_articulos %>% 
  mutate(titulo_limpio = str_to_upper(titulo),
         titulo_limpio = str_replace_all(titulo_limpio, 
                                         "[[:punct:]]", " "),
         titulo_limpio = stri_trans_general(titulo_limpio, 
                                            id = "Latin-ASCII"),
         titulo_limpio = str_replace_all(titulo_limpio,
                                         "  ", 
                                         " "),
         titulo_limpio = str_trim(titulo_limpio),
         id = 1:length(grupo_df_articulos$categoria)) %>% 
  filter(!duplicated(titulo_limpio)) #quitar <------------

grupo_df_similarity <- 
  grupo_df_articulos_no_duplicados %>% 
  select(titulo_limpio,
         id) %>% 
  unnest_tokens(output = "words",
                input = titulo_limpio, 
                token = "words") %>% 
  count(id, words) %>% 
  pairwise_similarity(item = id, 
                      feature = words, 
                      value = n)
                      
'''

# Listado de artículos similares

produccion_similar_articulos <- 
  grupo_df_articulos_no_duplicados %>% 
  right_join(grupo_df_similarity %>% 
               filter(similarity >= 0.70), 
             by = c("id" = "item1")) %>% 
  left_join(grupo_df_articulos_no_duplicados, 
            by = c("item2" = "id"))

# Es necesario eliminar los registros que tengan distintos grupos 

produccion_similar_articulos_grupos <- 
  produccion_similar_articulos %>% 
  filter(grupo.x == grupo.y) %>% 
  filter(similarity >= 0.80) 
#filtrar por mismo grupo <----------------

# Necesitamos identificar los registros unicos duplicados (igraph)

graph_duplicados <- 
  produccion_similar_articulos_grupos %>% 
  select(id, item2, similarity) %>% 
  rename(Source = "id",
         Target = "item2",
         weight = "similarity") %>% 
  graph_from_data_frame(directed = FALSE) %>% 
  simplify()

duplicados_unicos <- 
  cbind(get.edgelist(graph_duplicados),
        E(graph_duplicados)$weight/2) %>% 
  as.data.frame()

duplicados_unicos %>% dim

# Eliminamos los registros duplicados mayores al 80% de similaridad que están en un mismo grupo
# En esta parte puede haber un error ya que pueden estar en diferentes revistas. 

grupo_df_articulos_no_duplicados_sin_sim <- 
  grupo_df_articulos_no_duplicados %>% 
  anti_join(duplicados_unicos %>% 
              select(V1) %>% 
              mutate(V1 = as.numeric(V1)),
            by = c("id" = "V1")) %>% 
  select(-titulo_limpio, 
         -id)