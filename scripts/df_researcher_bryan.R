library(tidyverse)
library(rvest)
library(here)
library(xml2)
df_researcher <- produccion_grupos[[3]] |> 
  filter(str_detect(fin_vinculacion, "Actual"))
data_researcher_all <- 
  apply(df_researcher, 1, function(x){
    URL <- read_html(x['url'])})
df_info_researcher <- tibble()
for (i in 1:length(data_researcher_all)) {
  
  tabla <- data_researcher_all[[i]] |> html_table() 
  
  if (length(tabla[]) > 2) {
    
    df_1 <- tabla[[2]] |> 
      select(2) |> 
      slice(2) |> 
      mutate(categoria = str_remove(X2, "con .*"),
             categoria = str_remove(categoria, "hasta.*"),
             categoria = str_trim(categoria)) |> 
      select(categoria)
    
    j=1
    while(j<=6){
      if (tabla[[j]]$X1[1]=="Formación Académica") {
        df_2 <- tabla[[j]] |>
          select(2) |> 
          slice(2) |> 
          separate(X2,c("info_1","info_2","info_3"), 
                   sep = "\r\n" ) |> 
          mutate(ultima_formacion_academica = str_trim(info_1)) |> 
          select(-info_1,-info_2,-info_3)
        j=7
      }
      else{
        j=j+1
      }
    }
  }else{
    df_2 <- data.frame(ultima_formacion_academica = "NA")
    try( df_1 <- tabla[[2]] |> 
           select(2) |> 
           slice(1) |> 
           mutate(categoria = str_remove(X2, "con .*"),
                  categoria = str_remove(categoria, "hasta.*"),
                  categoria = str_trim(categoria)) |> 
           select(categoria)
    )
    
  }
  df_1 <- cbind(df_1,df_2)
  
  df_info_researcher <- bind_rows(df_info_researcher,df_1)
}
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