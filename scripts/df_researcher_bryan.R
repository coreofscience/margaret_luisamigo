library(tidyverse)
library(rvest)
library(here)
library(xml2)

df_researcher <- 
  produccion_grupos[[3]] |> 
  filter(str_detect(fin_vinculacion, "Actual")) |> 
  mutate(posgrade = map(.x = url, 
                        .f = safely(get_posgrade_from_cvlac))) |> 
  mutate(posgrade = map(posgrade, "result"))

articulos_unicos <- read_csv("data/articulos_unicos.csv")
View(articulos_unicos)
cantidad <- articulos_unicos |> 
  filter(ano >= 2016,ano <=2020) |> 
  select(autores) |> 
  separate_rows(autores, sep = ", ") |> 
  count(autores, sort = TRUE, name = "cantidad")

df_hindex <- getting_scholar_h_index(scholar_id)

df_hindex <- df_hindex |> 
  mutate(h_index = ifelse(is.na(h_index),0,h_index),
         researcher = str_to_upper(researcher),
         researcher = stri_trans_general(researcher, id = "Latin-ASCII")) |> 
  filter(!duplicated(researcher))

df_researcher <- cbind(df_researcher, df_info_researcher)

df_researcher_1 <- df_researcher |> 
  mutate(integrantes = str_to_upper(integrantes),
         integrantes = stri_trans_general(integrantes, id = "Latin-ASCII"))|> 
  left_join(cantidad,by = c("integrantes"="autores")) |> 
  left_join(df_hindex, by = c("integrantes"="researcher")) |> 
  mutate(h_index = ifelse(is.na(h_index),0,h_index),
         cantidad = ifelse(is.na(cantidad),0,cantidad),
         categoria = ifelse(categoria == "Investigador Junior (IJ)" |
                              categoria == "Investigador Asociado (I)" |
                              categoria == "Investigador Senior (IS)" ,
                            categoria, "sin clasificar"))