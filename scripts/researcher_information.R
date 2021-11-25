researcher_information_ucla <- function(researchers, shiny_data){
  
  researchers_data <- researchers |> 
    mutate(id = 1:length(researchers$researcher),
           id = str_c("1-",id)) |> 
    rename(integrantes = 1) |> 
    select(integrantes, id)
  
  df_docentes_all <- data.frame(shiny_data[[3]])
  
  df_docentes <- df_docentes_all|> 
    mutate(id = 1:length(df_docentes_all$integrantes),
           id = str_c("2-",id)) |> 
    select(integrantes, id)
  
  df_1 <- rbind(researchers_data, df_docentes)
  
  df_2 <-
    df_1 |> 
    unnest_tokens(output = "words",
                  input = integrantes,
                  token = "words") |> 
    count(id, words) |> 
    pairwise_similarity(item = id, 
                        feature = words, 
                        value = n)
  
  df_5 <- 
    df_2 |>  
    filter(similarity >= 0.76)
  
  df_6 <- 
    df_5 |> left_join(researchers_data, by = c("item1" = "id")) |> 
    right_join(df_docentes, by = c("item2" = "id")) |> 
    filter(!duplicated(integrantes.y))
  
  grupo_researcher_cleaned <- 
    df_6 |> 
    full_join(researchers, by = c("integrantes.x"="researcher")) |> 
    full_join(df_docentes_all, by= c("integrantes.y"="integrantes")) |> 
    select(4:24)
  
  sin_grupo <- grupo_researcher_cleaned |> 
    filter(is.na(integrantes.y)) |> 
    select(-integrantes.y) |> 
    mutate(fin_vinculacion = "Sin información en GrupLAC") |> 
    rename(researcher = 1)
  
  grupo_researcher <- grupo_researcher_cleaned |> 
    filter(!is.na(integrantes.y)) |> 
    select(-integrantes.x) |> 
    mutate(unidad_academica = if_else(is.na(unidad_academica), "OTRO", unidad_academica),
           CENTRO = if_else(is.na(CENTRO), "OTRO", CENTRO),
           ZONA = if_else(is.na(ZONA), "OTRO", ZONA)) |> 
    rename(researcher = 1) |> 
    rbind(sin_grupo)
  
  return(grupo_researcher)
}
