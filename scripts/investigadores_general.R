
investigadores_general <- 
  read_csv(here("output", 
                "df_researcher.csv"))

articulos_unicos_2016_2020 <- 
  read_csv(here("data",
                "articulos_unicos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  select(-id) |>
  separate_rows(autores, sep = ", ")

productos <- articulos_unicos_2016_2020 |> 
  rename(integrantes = "autores")

df <- investigadores_general |> 
  left_join(productos, by = c("grupo", "integrantes")) |> 
  count(integrantes, sort = TRUE, name = "cantidad")

inv_general <- investigadores_general |> 
  left_join(df, by = "integrantes") |> 
  select(-cantidad.x) |> 
  rename(cantidad = "cantidad.y") |> 
  select(1,2,3,4,5,6,7,8,9,12,10,11)
write_csv(inv_general, file = "investigadores.csv")
