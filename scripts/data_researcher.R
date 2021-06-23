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

df_researcher <- cbind(df_researcher, df_info_researcher)
