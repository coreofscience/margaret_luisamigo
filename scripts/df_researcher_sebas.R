
df_researcher <- produccion_grupos[[3]] |> 
  filter(str_detect(fin_vinculacion, "Actual")) |> 
  mutate(posgrado)

df_researcher_dummy <- 
  df_researcher |>  
  mutate(posgrade = map(.x = url, 
                        .f = safely(get_posgrade_from_cvlac))) |> 
  mutate(posgrade_1 = map(posgrade, "result"),
         error = map(posgrade, "error"))

get_posgrade_from_cvlac <- function(cvlac_url) {
  
  cvlac_df = read_html(cvlac_url) |> 
    html_table()
  
  cvlac_posgrade_df = cvlac_df[[1]] |> 
    filter(str_detect(string = X1, 
                      pattern = "Formación Académica")) |> 
    select(X5, X7) |> 
    slice(1) |> 
    separate_rows(X5, sep = "\r\n") |>
    slice(1,4) |> 
    mutate(X5 = str_trim(X5)) |> 
    nest(data = X5) |> 
    rename("X5" = data) |> 
    separate_rows(X7, sep = "\r\n") |> 
    slice(1,4) |> 
    mutate(X7 = str_trim(X7)) |> 
    nest(data = X7) |> 
    rename("X7" = data) |> 
    unnest(cols = c(X5, X7)) |> 
    add_rownames() |> 
    gather(var, value, -rowname) |> 
    spread(rowname, value) |> 
    select(-var) |> 
    rename("posgrade" = 1,
           "duration" = 2) |> 
    separate(duration, sep = " - ", into = c("start", "end")) |> 
    filter(end != "de") |> 
    select(posgrade) |> 
    slice(1) |> 
    unlist()
  
  return(cvlac_posgrade_df)
  
}
  

# Olga cvlac

df_dummy_3 <- 
  read_html("https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000066346") |> 
  html_table()

df_olga <- 
  df_dummy_3[[1]]  |> 
  filter(str_detect(string = X1, 
                    pattern = "Formación Académica")) |> 
  select(X5, X7) |> 
  slice(1) |> 
  separate_rows(X5, sep = "\r\n") |>
  slice(1,4) |> 
  mutate(X5 = str_trim(X5)) |> 
  nest(data = X5) |> 
  rename("X5" = data) |> 
  separate_rows(X7, sep = "\r\n") |> 
  slice(1,4) |> 
  mutate(X7 = str_trim(X7)) |> 
  nest(data = X7) |> 
  rename("X7" = data) |> 
  unnest(cols = c(X5, X7)) |> 
  add_rownames() |> 
  gather(var, value, -rowname) |> 
  spread(rowname, value) |> 
  select(-var) |> 
  rename("posgrado" = 1,
         "duracion" = 2) |> 
  separate(duracion, sep = " - ", into = c("inicio", "fin")) |> 
  filter(fin != "de") |> 
  select(posgrado) |> 
  slice(1)

# Pedro cvlac

df_dummy_pedro <- 
  read_html("https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0001488843") |> 
  html_table()

df_pedro <- 
  df_dummy_pedro[[1]] |> 
  filter(str_detect(string = X1, 
                    pattern = "Formación Académica")) |> 
  select(X5, X7) |> 
  slice(1) |> 
  separate_rows(X5, sep = "\r\n") |>
  slice(1,4) |> 
  mutate(X5 = str_trim(X5)) |> 
  nest(data = X5) |> 
  rename("X5" = data) |> 
  separate_rows(X7, sep = "\r\n") |> 
  slice(1,4) |> 
  mutate(X7 = str_trim(X7)) |> 
  nest(data = X7) |> 
  rename("X7" = data) |> 
  unnest(cols = c(X5, X7)) |> 
  add_rownames() |> 
  gather(var, value, -rowname) |> 
  spread(rowname, value) |> 
  select(-var) |> 
  rename("posgrado" = 1,
         "duracion" = 2) |> 
  separate(duracion, sep = " - ", into = c("inicio", "fin")) |> 
  filter(fin != "de") |> 
  select(posgrado) |> 
  slice(1)
  

# Yamile cvlac

df_dummy_yamile <- 
  read_html("https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000045844") |> 
  html_table()

df_yamile <- 
  df_dummy_yamile[[1]] |> 
  filter(str_detect(string = X1, 
                    pattern = "Formación Académica")) |> 
  select(X5, X7) |> 
  slice(1) |> 
  separate_rows(X5, sep = "\r\n") |>
  slice(1,4) |> 
  mutate(X5 = str_trim(X5)) |> 
  nest(data = X5) |> 
  rename("X5" = data) |> 
  separate_rows(X7, sep = "\r\n") |> 
  slice(1,4) |> 
  mutate(X7 = str_trim(X7)) |> 
  nest(data = X7) |> 
  rename("X7" = data) |> 
  unnest(cols = c(X5, X7)) |> 
  add_rownames() |> 
  gather(var, value, -rowname) |> 
  spread(rowname, value) |> 
  select(-var) |> 
  rename("posgrado" = 1,
         "duracion" = 2) |> 
  separate(duracion, sep = " - ", into = c("inicio", "fin")) |> 
  filter(fin != "de") |> 
  select(posgrado) |> 
  slice(1)

# Sebastian cvlac

df_dummy_sebas <- 
  read_html("https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0001430153") |> 
  html_table()

df_sebas <- 
  df_dummy_sebas[[1]] |> 
  filter(str_detect(string = X1, 
                    pattern = "Formación Académica")) |> 
  select(X5, X7) |> 
  slice(1) |> 
  separate_rows(X5, sep = "\r\n") |>
  slice(1,4) |> 
  mutate(X5 = str_trim(X5)) |> 
  nest(data = X5) |> 
  rename("X5" = data) |> 
  separate_rows(X7, sep = "\r\n") |> 
  slice(1,4) |> 
  mutate(X7 = str_trim(X7)) |> 
  nest(data = X7) |> 
  rename("X7" = data) |> 
  unnest(cols = c(X5, X7)) |> 
  add_rownames() |> 
  gather(var, value, -rowname) |> 
  spread(rowname, value) |> 
  select(-var) |> 
  rename("posgrado" = 1,
         "duracion" = 2) |> 
  separate(duracion, sep = " - ", into = c("inicio", "fin")) |> 
  filter(fin != "de") |> 
  select(posgrado) |> 
  slice(1)

