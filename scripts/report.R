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
library(openxlsx) 

df <- tibble(id = numeric())
df_eliminados <- tibble(item1 = numeric(), 
                        item2 = numeric(), 
                        similarity = numeric())

grupo_df_articulos <- # create an id 
  produccion_grupos[[2]][["articulos"]] %>% 
  mutate(id = 1:length(produccion_grupos[[2]][["articulos"]][["grupo"]]))

grupos <- 
  produccion_grupos[[2]][["articulos"]] %>% 
  select(grupo) %>% 
  unique

for (i in grupos$grupo) {
  
  df_1 <-  
    grupo_df_articulos %>% 
    filter(grupo == i) %>% 
    select(id, 
           titulo)
  
  df_2 <- 
    df_1 %>% 
    unnest_tokens(output = "words",
                  input = titulo,
                  token = "words") %>% 
    count(id, words) %>% 
    pairwise_similarity(item = id, 
                        feature = words, 
                        value = n) %>% 
    filter(similarity >= 0.70)
  
  df_3 <- 
    df_2 %>% 
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
    select(-titulo)
  
  df <- 
    df %>% 
    bind_rows(df_5)
  
  df_eliminados <-
    df_eliminados %>%
    bind_rows(df_2)
}

  df_eliminados <-
    df_eliminados %>% 
    right_join(grupo_df_articulos, by=c("item1"="id")) %>% 
    left_join(grupo_df_articulos, by=c("item2"="id"))
  
  df_eliminados_total<- df_eliminados[ ,c(1,4,5,6,7,8,9,10,11,12,13,
                                          14,15,16,3,2,17,18,19,20,
                                          21,22,23,24,25,26,27,28,29)]
  
  grupo_df_articulos_repetidos <- 
    grupo_df_articulos %>% 
    anti_join(df)
  
  grupo_df_articulos_unicos <- 
    grupo_df_articulos %>% 
    inner_join(df)

#-------------------------------------------------------------------------------  
 #eliminated with different groups
  df_ <- tibble(id = numeric())  
  df_eliminados_grupos <- tibble(item1 = numeric(), 
                          item2 = numeric(), 
                          similarity = numeric())
  
  df__1 <-  
    grupo_df_articulos_unicos %>% 
    select(id, 
           titulo)
    
  df__2 <- 
    df__1 %>% 
    unnest_tokens(output = "words",
                  input = titulo,
                  token = "words") %>% 
    count(id, words) %>% 
    pairwise_similarity(item = id, 
                        feature = words, 
                        value = n) %>% 
    filter(similarity >=0.70)
    
  df__3 <- 
    df__2 %>% 
    rename(Source = "item1",
           Target = "item2",
           weight = "similarity") %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    simplify()
    
  df__4 <- 
    cbind(get.edgelist(df__3),
          E(df__3)$weight/2) %>% 
    as.data.frame() %>% 
    select(V2) %>% 
    rename(id = "V2") %>% 
    mutate(id = as.numeric(id))
    
  df__5 <- 
    df__1 %>% 
    anti_join(df__4) %>% 
    select(-titulo)
    
  df_ <- 
    df_ %>% 
    bind_rows(df__5)
    
  df_similares_grupos <-
    df__2
    
  df_similares_grupos <-
    df_similares_grupos %>% 
    right_join(grupo_df_articulos, by=c("item1"="id")) %>% 
    left_join(grupo_df_articulos, by=c("item2"="id"))
    
  df_similares_total_grupos<- df_similares_grupos[ ,c(1,4,5,6,7,8,9,10,
                                          11,12,13,14,15,16,3,2,17,18,19,
                                          20,21,22,23,24,25,26,27,28,29)]

  grupo_df_articulos_repetidos_grupos <- 
    grupo_df_articulos_unicos %>% 
    anti_join(df_)
    
  grupo_df_articulos_unicos_grupos <- 
    grupo_df_articulos_unicos %>% 
    inner_join(df_)
  
  rm(df,df_,df_1,df_2,df_3,df_4,df_5,df__1,
     df__2,df__3,df__4,df__5,df_eliminados,
     df_eliminados_grupos,df_similares_grupos,
     grupo_df_articulos,grupo_df_articulos_repetidos,
     grupo_df_articulos_repetidos_grupos,
     grupo_df_articulos_unicos,grupo_df_articulos_unicos_grupos)
  
  #export excel with comparison of deleted products  
  #list_of_datasets <- list("eliminados_mismo_grupo" = df_eliminados_total, 
                           #"similares_entre_grupos" = df_similares_total_grupos)
  #write.xlsx(list_of_datasets, file = "comparacion_eliminados.xlsx")
  