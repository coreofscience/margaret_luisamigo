library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)
library(stringi)
library(widyr)
library(tidytext)
library(igraph)

getting_orcid_first_time <- function(shiny_data){
  
  
  orcid_wido <- read_csv("https://docs.google.com/spreadsheets/d/1L5C5P620MsOHVEAfv8MWWlP7V7P5XG1C/export?format=csv&gid=1998435805") |> 
    unique() |> 
    mutate(INVESTIGADOR = str_to_upper(INVESTIGADOR),
           INVESTIGADOR = stri_trans_general(str = INVESTIGADOR,
                                           id = "Latin-ASCII"),
           INVESTIGADOR = str_replace(INVESTIGADOR, "  ", " "),
           ID = 1:length(INVESTIGADOR),
           ID = str_c("1-",ID)) |> 
    select(-2)
  
  orcid_plumx <- read_csv("https://docs.google.com/spreadsheets/d/1eBAvKT4CDCb1sc2RTeByOuNPy68Jgvlw/export?format=csv&gid=1331860699") |> 
    unique() |>
    select(3,6) |> 
    mutate(INVESTIGADOR = str_to_upper(INVESTIGADOR),
           INVESTIGADOR = stri_trans_general(str = INVESTIGADOR,
                                             id = "Latin-ASCII"),
           INVESTIGADOR = str_replace(INVESTIGADOR, "  ", " "),
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
  
  df_5 <- 
    df_2 |>  
    filter(similarity >= 0.70)
  
  df_6 <- 
    df_5 |> full_join(orcid_wido, by = c("item1" = "ID")) |> 
    full_join(orcid_plumx, by = c("item2" = "ID")) |> 
    mutate(INVESTIGADOR.x = ifelse(is.na(INVESTIGADOR.x),INVESTIGADOR.y, INVESTIGADOR.x),
           ORCID.x = ifelse(is.na(ORCID.x),ORCID.y,ORCID.x)) |> 
    filter(!is.na(INVESTIGADOR.x)) |> 
    select(4,5) |> 
    filter(!duplicated(INVESTIGADOR.x)) |> 
    rename(INVESTIGADOR = 1,
           ORCID = 2) |> 
    mutate(ID = 1:length(INVESTIGADOR),
           ID = str_c("1-",ID))
  
  orcid <- df_6 |> select(1,3)
  
  researcher <- data.frame(shiny_data[[3]]) |> 
    mutate(ID = 1:length(integrantes),
           ID = str_c("2-",ID))
  
  researcher2 <- researcher |> select(integrantes, ID) |> 
    rename(INVESTIGADOR = 1)
  
  data <- rbind(orcid, researcher2)
  
  df_2d <-
    data |> 
    unnest_tokens(output = "words",
                  input = INVESTIGADOR,
                  token = "words") |> 
    count(ID, words) |> 
    pairwise_similarity(item = ID, 
                        feature = words, 
                        value = n)
  
  df_5d <- 
    df_2d |>  
    filter(similarity >= 0.70)
    
  new_data <- df_5d |> full_join(df_6, by = c("item1" = "ID")) |> 
    full_join(researcher, by = c("item2" = "ID")) |> 
    mutate(integrantes = ifelse(is.na(integrantes),INVESTIGADOR,integrantes)) |> 
    filter(!is.na(integrantes)) |> 
    select(5:24) |> 
    select(2,1,3:20) |> 
    arrange(grupo)
  
  #write.xlsx(new_data, "researchers.xlsx")
  
 return(new_data)
}

getting_orcid <- function(shiny_data){
 
  researcher <- data.frame(shiny_data[[3]])
  
  orcid <- read_csv("https://docs.google.com/spreadsheets/d/1gBaXHFp1NTUTeXodb4JyHqY-P-AWV5yN5-p4L1O09gk/export?format=csv&gid=1846176083") |> 
    select(1,2,3) |> 
    unique() |> 
    mutate(researcher = str_to_upper(researcher),
           researcher = stri_trans_general(str = researcher,
                                           id = "Latin-ASCII"))
  
  new_data <- left_join(researcher, orcid) |> 
    select(-researcher) |> 
    select(1:13,20,14:19)
  return(new_data)
   
}
