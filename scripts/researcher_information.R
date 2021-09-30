researcher_information_ucla <- function(shiny_data){
  
  researcher_data_all <- read_csv("https://docs.google.com/spreadsheets/d/15piQ_UOC6TtSCc-aa5Erdl_L5A2ct695Bi9SS9b7tuI/export?format=csv&gid=0") |> 
    unique() 
  
  researcher_data <- researcher_data_all|> 
    mutate(id = 1:length(researcher_data_all$DOCENTE),
           id = str_c("1-",id)) |> 
    rename(integrantes = 1) |> 
    select(integrantes, id)
  
  df_docentes_all <- data.frame(shiny_data[[3]])
  
  df_docentes <- df_docentes_all|> 
    mutate(id = 1:length(df_docentes_all$integrantes),
           id = str_c("2-",id)) |> 
    select(integrantes, id)
  
  df_1 <- rbind(researcher_data, df_docentes)
  
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
    df_5 |> left_join(researcher_data, by = c("item1" = "id")) |> 
    right_join(df_docentes, by = c("item2" = "id")) |> 
    filter(!duplicated(integrantes.y))
  
  grupo_researcher_cleaned <- 
    df_6 |> left_join(researcher_data_all, by = c("integrantes.x"="DOCENTE")) |> 
    left_join(df_docentes_all, by= c("integrantes.y"="integrantes")) |> 
    select(5:23) |> 
    rename(integrantes = 1) |> 
    mutate(UNIDAD = ifelse(is.na(UNIDAD), "Otro", UNIDAD),
           PROGRAMA = ifelse(is.na(PROGRAMA), "Otro", PROGRAMA)) |> 
    rename(unidad = UNIDAD, 
           programa = PROGRAMA) |> 
    arrange(grupo)
  
  return(grupo_researcher_cleaned)
}
