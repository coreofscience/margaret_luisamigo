library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)
library(stringi)
library(widyr)
library(tidytext)

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
df_5 <- 
  df_2 |>  
  filter(similarity >= 0.7)




