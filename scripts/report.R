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


df <- tibble(id = numeric())
df_eliminados <- tibble(item1 = numeric(), 
                        item2 = numeric(), 
                        similarity = numeric())

grupo_df_articulos <- # create an id 
  produccion_grupos$articulos %>% 
  mutate(id = 1:length(produccion_grupos$articulos$grupo)) %>% 
  rename(item1 = "id")

grupos <- 
  grupo_df_articulos %>% 
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
  

df_eliminada<-
  merge(x=df_eliminados,y=grupo_df_articulos, by="item1") %>% 
  select(-item2,-similarity)
 
write.xlsx(df_eliminada, "lista_enlac.xlsx")

write.xlsx(df_eliminada, file="lista_enl.xlsx",
                sheetName="punto1",row.names=FALSE)
write.xlsx(df_eliminada2, file="lista_enl.xlsx", sheetName="Punto2",
           append=TRUE, row.names=FALSE)


wb = createWorkbook()

sheet = createSheet(wb, "Sheet 1")

addDataFrame(df_eliminada, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(df_eliminada2, sheet=sheet, startColumn=1, row.names=FALSE)


sheet = createSheet(wb, "Sheet 2")

saveWorkbook(wb, "My_File.xlsx")

  df_eliminadaA<-
    merge(x=df_eliminados,y=grupo_df_articulos, by="item1") 
  
  df_eliminada2<-
    merge(x=df_eliminadaA,y=df_eliminada, by="item2")
  
  df_eliminada3<-
    merge(x=df_eliminada2,y=grupo_df_articulos, by.x="item2", by.y="id")
  
  grupo_df_articulos_repetidos <- 
    grupo_df_articulos %>% 
    anti_join(df)
  
  grupo_df_articulos_unicos <- 
    grupo_df_articulos %>% 
    inner_join(df)
    
  contar_por_grupo_repetidos <- grupo_df_articulos_repetidos %>% 
    count(grupo, sort = TRUE, name = "Duplicados_grupos")
  
  contar_por_grupo_unicos <- grupo_df_articulos_unicos %>% 
    count(grupo, sort = TRUE, name = "sin_duplicados")

#-------------------------------------------------------------------------------  
  
  df_ <- tibble(id = numeric())  
  
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
                          value = n)
    
    df__3 <- 
      df__2 %>% 
      filter(similarity >= 0.70) %>% 
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
    
    grupo_df_articulos_repetidos_grupos <- 
      grupo_df_articulos_unicos %>% 
      anti_join(df_)
    
    grupo_df_articulos_unicos_grupos <- 
      grupo_df_articulos_unicos %>% 
      inner_join(df_)
    
    contar_grupos_repetidos <- grupo_df_articulos_repetidos_grupos %>% 
      count(grupo, sort = TRUE, name = "Duplicados_grupos")
    
    contar_grupos_unicos <- grupo_df_articulos_unicos_grupos %>% 
      count(grupo, sort = TRUE, name = "sin_duplicados")
    