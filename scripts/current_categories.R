
articulos_unicos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) |> 
  mutate(ano = as.character(ano))

scimago <- read_csv2(here("output",
                         "scimago2020.csv")) |> 
  separate_rows(Issn, sep = ", ")|>
  rename("ISSN" = Issn,
         "SJR_Q"= 7) |> 
  select(ISSN,SJR_Q) |> 
  mutate(i1 = substr(ISSN, 1,4),
         i2 = substr(ISSN, 5,8)) |>
  unite(ISSN, c("i1","i2"), sep = "-", remove = TRUE)

national_2021 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=758989915") |> 
  unite(ISSN, c("issn_impreso","issn_electronico", "issn_l"), sep = ",", remove = TRUE) |> 
  mutate(ano = "2021") |> 
  select(-VIGENCIA) |> 
  separate_rows(ISSN, sep = ",") |> 
  filter(!str_detect(ISSN, "NA")) |> 
  rename(revista = 1) |> 
  select(1,2,4,3)

international_j_2021 <- read_csv(here("output",
                                      "international_journals_2021.csv")) |> 
  separate_rows(ISSN, sep = "; ") |> 
  mutate(ano = as.character(ano))

nombres <- articulos_unicos_2016_2020 |> 
  select(ISSN, revista) |> 
  unique()

international <- left_join(articulos_unicos_2016_2020,international_j_2021, by="ISSN")
all_journals <- left_join(international, national_2021, by= "ISSN") |> 
  mutate(categoria = ifelse(is.na(categoria), categoria.y, categoria),
         revista = ifelse(is.na(revista), revista.y, revista))

revistas <- all_journals |> count(ISSN, categoria, revista) |> 
  left_join(nombres, by = "ISSN")|> 
  mutate(revista.x = ifelse(is.na(revista.x), revista.y, revista.x),
         revista.x = str_to_upper(revista.x)) |> 
  rename("revista" = revista.x,
         "cantidad" = n) |> 
  left_join(scimago, by = "ISSN") |> 
  select(revista, ISSN, categoria, SJR_Q,cantidad) |> 
  group_by(cantidad) |> arrange(desc(cantidad)) |> 
  mutate(categoria = ifelse(is.na(categoria), "Sin categoria", categoria),
         SJR_Q = ifelse(is.na(SJR_Q), "Sin categoria", SJR_Q))

revistas$porcentaje <- round(prop.table(revistas$cantidad),3)*100 

write_csv(revistas, here("output","current_journals.csv"))
