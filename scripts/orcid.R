library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)
library(stringi)
library(widyr)
library(tidytext)
library(igraph)

getting_orcid <- function(shiny_data){
  
  
  orcid_wido <- read_csv("https://docs.google.com/spreadsheets/d/1L5C5P620MsOHVEAfv8MWWlP7V7P5XG1C/export?format=csv&gid=1998435805") |> 
    unique() |> 
    filter(!is.na(ORCID)) |> 
    mutate(INVESTIGADOR = str_to_upper(INVESTIGADOR),
           INVESTIGADOR = stri_trans_general(str = INVESTIGADOR,
                                           id = "Latin-ASCII"),
           ID = 1:length(INVESTIGADOR),
           ID = str_c("1-",ID)) |> 
    select(-2)
  
  orcid_plumx <- read_csv("https://docs.google.com/spreadsheets/d/1eBAvKT4CDCb1sc2RTeByOuNPy68Jgvlw/export?format=csv&gid=1331860699") |> 
    unique() |>
    select(3,6) |> 
    filter(!is.na(ORCID)) |> 
    mutate(INVESTIGADOR = str_to_upper(INVESTIGADOR),
           INVESTIGADOR = stri_trans_general(str = INVESTIGADOR,
                                             id = "Latin-ASCII"),
           ID = 1:length(INVESTIGADOR),
           ID = str_c("2-",ID))
  
  df_1 <- rbind(orcid_wido, orcid_plumx)
  
  df_2 <-
    df_1 |> 
    unnest_tokens(output = "words",
                  input = INVESTIGADOR,
                  token = "words") |> 
    count(ID, words) |> 
    pairwise_similarity(item = ID, 
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
    rename(ID = "V2")
  
  df_5 <- 
    df_1 %>% 
    anti_join(df_4)
  
  orcid <- df_5 |> select(1,2)
  
  #reseaecher <- data.frame(shiny_data[[3]])
  researcher <- read_csv(here("output","investigadores.csv"))
  researcher2 <- researcher |> select(1) |> 
    mutate(ORCID = "NA") |> 
    rename(INVESTIGADOR = 1)
  data <- rbind(orcid,researcher2) |> 
    mutate(ID = 1:length(INVESTIGADOR))
  
  df_2d <-
    data |> 
    unnest_tokens(output = "words",
                  input = INVESTIGADOR,
                  token = "words") |> 
    count(ID, words) |> 
    pairwise_similarity(item = ID, 
                        feature = words, 
                        value = n)
  df_3d <- 
    df_2d %>% 
    filter(similarity >= 0.70) %>% 
    rename(Source = "item1",
           Target = "item2",
           weight = "similarity") %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    simplify()
  
  df_4d <- 
    cbind(get.edgelist(df_3d),
          E(df_3d)$weight/2) %>% 
    as.data.frame() %>% 
    select(V2) %>% 
    rename(ID = "V2") |> 
    mutate(ID = as.numeric(ID))
  
  df_5d <- 
    data %>% 
    anti_join(df_4d)
}