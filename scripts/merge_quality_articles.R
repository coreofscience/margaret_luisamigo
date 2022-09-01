merge_quality_articles_ucla <- function(articulos_unicos){
 
  ## scimago data
  scimago <- read.csv(here('data','scimago_categories.csv')) |> 
    select(-1)|> 
    mutate(ano = as.character(ano)) |> 
    mutate(i1 = substr(ISSN, 1,4),
           i2 = substr(ISSN, 5,8),
           ISSN = str_trim(ISSN)) |>
    unite(ISSN, c("i1","i2"), sep = "-", remove = TRUE)
  
  ## homologadas publindex
  publindex_h <- read.csv(here('data','homologadas_publindex.csv')) |> 
    select(-1)|> 
    mutate(ano = as.character(ano),
           ISSN = str_trim(ISSN)) |> 
    separate_rows(ISSN, sep = ',') |> 
    select(1,4,3,2) |> 
    rename(categoria_h = 2)
 
  ## National publindex
  publindex_n <- read.csv(here('data','national_publindex.csv')) |> 
    select(-1)|> 
    mutate(ano = as.character(ano),
           ISSN = str_trim(ISSN)) |> 
    rename(categoria_n = 2)
  
  scimago_data_merge <- scimago |>
    full_join(publindex_h, by=c("ISSN","ano")) |>
    mutate(categoria = ifelse(is.na(categoria),categoria_h,categoria)) |> 
    rename(revista = 1) |> 
    select(1,2,3,4)
  
  articulos <- articulos_unicos |> 
    mutate(ano = str_trim(ano),
           ISSN = str_trim(ISSN)) |> 
    left_join(scimago_data_merge, by = c("ISSN", "ano")) |> 
    left_join(publindex_h, by = c("ano", "ISSN")) |> 
    left_join(publindex_n, by = c("ISSN", "ano")) |> 
    mutate(categoria.y = ifelse(is.na(categoria.y), categoria_h, categoria.y),
           categoria_h = ifelse(is.na(categoria_h), categoria_n, categoria_h)) |> 
    select(1:7,18,16,8:14) |>
    rename("categoria" = categoria.x,
           "categoria_revista" = categoria_h,
           "revista" = revista.x,
           'SJR_Q' = categoria.y) |>
    mutate(SJR_Q = ifelse(ano < 1999, "Sin información", SJR_Q),
           SJR_Q = ifelse(is.na(SJR_Q), 'Sin categoría',SJR_Q),
           SJR_Q = ifelse(SJR_Q == '-', 'Sin categoría',SJR_Q),
           categoria_revista = ifelse(ano < 2003, "Sin información", categoria_revista),
           categoria_revista = ifelse(is.na(categoria_revista), "Sin categoría", categoria_revista)) |> 
    group_by(grupo) |> arrange(desc(grupo))|> 
    mutate(SJR_Q = case_when(SJR_Q =="A1" ~ "Q1",
                             SJR_Q =="A2" ~ "Q2",
                             SJR_Q =="B" ~ "Q3",
                             SJR_Q =="C" ~ "Q4",
                             TRUE ~ SJR_Q)) |> 
    unique()
  
  return(articulos)
}
