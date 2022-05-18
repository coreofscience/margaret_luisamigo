get_cvlac <- function(url) {
  cvlac_df <- read_html(url) |> 
    html_table()
  
  df_name <- 
    cvlac_df[[1]] |> 
    select(X1, X2) |> 
    filter(X1 == "Nombre") |>
    select(name = X2)
  
  df_category <- 
    cvlac_df[[1]] |> 
    select(X1, X2) |> 
    filter(X1 == "Categoría") |> 
    rename(Category = X2) |> 
    mutate(Category = str_extract(Category, 
                                  ".*con vigencia"),
           Category = str_remove(Category, 
                                 " con vigencia")) |> 
    select(Category)
  
  df_formation <- # We need to extract more data from this part. issue
    cvlac_df[[1]] |> 
    filter(str_detect(X1, "Formación Académica")) |> 
    slice(1) |> 
    purrr::discard(~all(is.na(.) | . == "")) |> 
    select(-X1, -X2, -X3) |> 
    t() |> 
    data.frame() |> 
    rename(formation = 1) |> 
    mutate(formation = str_extract(formation, 
                                   ".*"))
  
  df_frh <- # We need to extract more information issue
    cvlac_df[[1]] |> 
    filter(str_detect(X1, 
                      "Trabajos dirigidos/tutorías")) |> 
    select(-X1, -X2) |> 
    slice(1) |> 
    t() |> 
    data.frame() |> 
    rename(FRH = 1) |> 
    filter(str_detect(FRH, 
                      "Trabajos dirigidos")) |> 
    mutate(FRH = str_remove(FRH , 
                            "Trabajos dirigidos/Tutorías - Trabajo de grado de ")) |> 
    group_by(FRH) |> 
    count() |> 
    rename(total = n)
  
  return(list(name = df_name,
              category = df_category,
              formation = df_formation,
              frh = df_frh))
  
}
