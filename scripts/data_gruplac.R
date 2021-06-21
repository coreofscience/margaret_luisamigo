library(tidyverse)
library(rvest)
library(here)
library(xml2)

#data_getting_ucla <- function(grupos) {
  
df_data = tibble(grupo = character(),)
integrantes = tibble(grupo = character(),)

for (i in 1:length(grupos$url)) {
  read <- read_html(grupos$url[i])
  

  df_l <- html_nodes(read,"a") %>% html_attr("href") %>% tibble() %>%
    mutate(grupo = grupos$grupo[i])
  
  integrantes <- bind_rows(integrantes, df_l)
  
  seleccionar <- c(TRUE,FALSE,FALSE,TRUE,TRUE)
  
  df_d <- read %>%
    html_table(1) %>% 
    tibble() %>% 
    head(5) %>%
    mutate(grupo = grupos$grupo[i])
  
  df_d <- df_d[seleccionar,]

  df_data <- bind_rows(df_data, df_d)
}
rm(i,df_d,df_l,read)
