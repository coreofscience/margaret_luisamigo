researcher_info <- function(researcher_data, researchers){
  
  
  researcher_data <- researcher_data |>
    rename(unidad_academica = 2) |> 
    group_by(researcher) |> 
    mutate(unidad_academica = paste0(unidad_academica, collapse = "; "),
           CENTRO = paste0(CENTRO, collapse = "; "),
           ZONA = paste0(ZONA, collapse = "; ")) |> 
    unique() 
   
  researcher_data <- as_tibble(researcher_data)
  
  researcher_data_all <- researcher_data |> 
     mutate(id = 1:length(researcher_data$researcher),
           id = str_c("1-",id)) |> 
    rename(integrantes = 1) |> 
    select(integrantes, id)
  
  df_docentes_all <- data.frame(researchers)
  
  df_docentes <- df_docentes_all|> 
    mutate(id = 1:length(df_docentes_all$researcher),
           id = str_c("2-",id)) |> 
    rename(integrantes = 1) |> 
    select(integrantes, id)
  
  df_1 <- rbind(researcher_data_all, df_docentes)
  
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
    filter(similarity >= 0.6)
  
  df_6 <- 
    df_5 |> left_join(researcher_data_all, by = c("item1" = "id")) |> 
    right_join(df_docentes, by = c("item2" = "id")) |> 
    filter(!duplicated(integrantes.y))
  
  grupo_researcher_cleaned <- 
    df_6 |> full_join(researcher_data, by = c("integrantes.x"="researcher")) |> 
    full_join(df_docentes_all, by= c("integrantes.y"="researcher")) |> 
    select(4,6,7,8,9,10) |> 
    rename(researcher = 1)
  
  return(grupo_researcher_cleaned)
}