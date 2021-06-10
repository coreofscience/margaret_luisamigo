library(tidyverse)
library(rvest)
library(here)

autores_df_articulos <- articulos_unicos %>% filter(ano >= 2016) %>% 
  separate_rows(autores, sep = ",") %>% 

  mutate(autores = str_trim(autores))

write.xlsx(autores_df_articulos , file = "Produccion_grupo_docente.xlsx")

