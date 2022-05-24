library(rvest)
library(dplyr)
library(stringr)

get_cvlac <- function(url) {
  cvlac_df <- read_html(url) |> 
    html_table()
  
  df_name <- 
    cvlac_df[[1]] |> 
    dplyr::select(X1, X2) |> 
    dplyr::filter(X1 == "Nombre") |>
    dplyr::select(name = X2)
  
  df_category <- 
    cvlac_df[[1]] |> 
    dplyr::select(X1, X2) |> 
    dplyr::filter(X1 == "Categoría") |> 
    dplyr::rename(Category = X2) |> 
    dplyr::mutate(Category = stringr::str_extract(Category, 
                                                  ".*con vigencia"),
                  Category = stringr::str_remove(Category, 
                                                 " con vigencia")) |> 
    dplyr::select(Category)
  
  df_formation <- # We need to extract more data from this part. issue
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, "Formación Académica")) |> 
    dplyr::slice(1) |> 
    purrr::discard(~all(is.na(.) | . == "")) |> 
    dplyr::select(-X1, -X2, -X3) |> 
    t() |> 
    data.frame() |> 
    dplyr::rename(formation = 1) |> 
    dplyr::mutate(formation = stringr::str_extract(formation, 
                                                   ".*"))
  
  df_frh <- # We need to extract more information issue
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      "Trabajos dirigidos/tutorías")) |> 
    dplyr::select(-X1, -X2) |> 
    dplyr::slice(1) |> 
    t() |> 
    data.frame() |> 
    dplyr::rename(FRH = 1) |> 
    dplyr::filter(stringr::str_detect(FRH, 
                                      "Trabajos dirigidos")) |> 
    dplyr::mutate(FRH = stringr::str_remove(FRH , 
                                            "Trabajos dirigidos/Tutorías - Trabajo de grado de ")) |> 
    dplyr::group_by(FRH) |> 
    dplyr::count() |> 
    dplyr::rename(total = n)
  
  df_events <- 
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      'Eventos científicos')) |> 
    purrr::discard(~all(is.na(.) | . == "")) |> 
    dplyr::select(-X1, -X2) |> 
    dplyr::slice(1) |> 
    t() |> 
    data.frame() |> 
    dplyr::rename(eventos = 1) |> 
    dplyr::filter(stringr::str_detect(eventos, 
                                      'Nombre del evento')) |> 
    dplyr::mutate(eventos = stringr::str_extract(eventos, 
                                                 '^\\d+')) |> 
    dplyr::distinct() |> 
    dplyr::count() |> 
    dplyr::rename(total = n)
  
  df_papers <- 
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      '^Artículos')) |> 
    purrr::discard(~all(is.na(.) | . == '')) |> 
    dplyr::select(-X1, -X2) |> 
    dplyr::slice(1) |> 
    t() |> 
    data.frame() |> 
    dplyr::rename(articulos = 1) |> 
    dplyr::filter(!stringr::str_detect(articulos,
                                      'Producción bibliográfica')) |> 
    dplyr::mutate(fecha = stringr::str_extract(articulos,
                                               ',[0-9]{4},')) |> 
    dplyr::mutate(fecha = str_remove_all(fecha, ',')) |> 
    dplyr::count(fecha, sort = TRUE)
  
  
  return(list(name = df_name,
              category = df_category,
              formation = df_formation,
              frh = df_frh,
              events = df_events,
              papers = df_papers))
  
}
