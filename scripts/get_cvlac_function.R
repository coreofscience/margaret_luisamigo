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
  
  df_category_1 <- 
    cvlac_df[[1]] |> 
    dplyr::select(X1, X2) 
  
  if (length(df_category_1$X1) > 0 ) {
    
    df_category_2 <- 
      df_category_1 |> 
      dplyr::filter(X1 == "Categoría") |> 
      dplyr::rename(Category = X2) |> 
      dplyr::mutate(Category = stringr::str_extract(Category, 
                                                    ".*con vigencia"),
                    Category = stringr::str_remove(Category, 
                                                   " con vigencia")) |> 
      dplyr::select(Category)
  }
  
  if (length(df_category_2$Category) == 0) {
    
    df_category_2 <- tibble(Category = "Sin categoria")

  }
  
  df_formation_1 <- # We need to extract more data from this part. issue
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, "Formación Académica")) 
  
  if (length(df_formation_1$X1) > 0) { 
    df_formation_2 <- 
      df_formation_1 |> 
      dplyr::slice(1) |> 
      purrr::discard(~all(is.na(.) | . == "")) |> 
      dplyr::select(-X1, -X2, -X3) |> 
      t() |> 
      data.frame() |> 
      dplyr::rename(formation = 1) |> 
      dplyr::mutate(formation = stringr::str_extract(formation, 
                                                     ".*"))
    
  }
  
  df_frh_1 <- # We need to extract more information issue
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      "Trabajos dirigidos/tutorías")) 
  
  if (length(df_frh_1$X1) > 0) {
    
    df_frh_2 <- 
      df_frh_1 |> 
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
  }
  
  df_events_1 <- 
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      'Eventos científicos')) 
  
  
  if (length(df_events_1$X1) > 0) { 
    
    df_events_2 <- 
      df_events_1 |> 
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
  }
  
  df_papers_1 <- 
    cvlac_df[[1]] |> 
    dplyr::filter(stringr::str_detect(X1, 
                                      '^Artículos')) 
  if (length(df_papers_1$X1) > 0) {
    
    df_papers_2 <- 
      df_papers_1 |> 
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
      dplyr::count(fecha, sort = TRUE) |> 
      rename(total = n)
  } else {
    df_papers_2 <- tibble(articulos = 0)
  }
  
  
  
  
  return(list(name = df_name,
              category = df_category_2,
              formation = df_formation_2,
              frh = df_frh_2,
              events = df_events_2,
              papers = df_papers_2))
  
}
