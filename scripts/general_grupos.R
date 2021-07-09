

# Creating dataframe 

df_1 <- 
  produccion_grupos[[1]] |> 
  select(grupo, 
         clasificacion,  
         web, 
         fecha_creacion, 
         lider, 
         email, 
         area_conocimiento_1)

articulos_unicos <- 
  read_csv(here("data",
                "articulos_unicos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020)

df_2 <- 
  articulos_unicos |> 
  select(grupo, categoria) |> 
  group_by(grupo) |> 
  count(categoria) |> 
  select(grupo, n) |> 
  rename(cantidad_articulos = "n")

df_3 <- 
  df_1 |> 
  left_join(df_2)

df_4 <- 
  produccion_grupos[[3]] |> 
  filter(str_detect(fin_vinculacion, "Actual")) |> 
  count(grupo) |> 
  rename(cantidad_investigadores = "n")

df_5 <- 
  df_3 |> 
  left_join(df_4) |> 
  left_join(grupos) |> 
  rename(gruplac = "url",
         universidad_link = "web") |> 
  select(grupo, clasificacion, 
         cantidad_articulos,
         cantidad_investigadores,
         gruplac,
         fecha_creacion,
         lider,
         email, 
         area_conocimiento_1) |> 
  mutate(gruplac = str_c("<a href=","\"",
                         gruplac,
                         "\"",
                         ">Link</a>")) 

write_csv(df_5, file = "general_grupos.csv")
