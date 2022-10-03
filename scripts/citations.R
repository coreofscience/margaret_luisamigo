get_citations <- function(researchers, articulos){
  
  library(scholar)
  # n <- get_article_cite_history('RGUTAfEAAAAJ', 'Tyk-4Ss8FVUC')
  # 
  # coauthor_network <- get_coauthors('RGUTAfEAAAAJ&hl')
  # plot_coauthors(coauthor_network)
  # write.csv(coauthor_network, "coautores.csv", row.names = F)
  # r = researcher
  # 
  # profile <- get_profile('RGUTAfEAAAAJ&hl')
  # publications <- get_publications('RGUTAfEAAAAJ')
  # publications2 <- get_publications('opg3irUAAAAJ')
  
  citations = tibble()
  for(i in researchers$id_scholar){
    citation <- get_publications(i) 
    if(!is.na(citation)){
      citation = citation|> 
        select(title, cites, year, pubid)
      citations <- rbind(citations,citation)
      print("No es NA: ")
      print(i)
    }
    print("Es NA: ")
    print(i)
  }
  
  articulos <- articulos_unicos |> ungroup()
  
  art <- articulos |> select(titulo,id) |> 
    mutate(id = str_c("1-",id))
           
  citations <- citations |> 
    mutate(id = 1:length(citations$title),
           id = str_c("2-",id))
  cit <- citations |> 
    rename(titulo = 1)|> 
    select(titulo, id)
  
  df_1 <- rbind(art,cit)
  
  df_3 <- df_1 |> 
    unnest_tokens(word,
                  input = titulo) |> 
    anti_join(stop_words, by = "word") |> 
    count(id, word) |> 
    ungroup()
  
  df_4 <- df_3 |> 
    widyr::pairwise_similarity(item = id, 
                        feature = word, 
                        value = n)
  
  df_5 <- 
    df_2 |>  
    filter(similarity >= 0.80)
  
  df_6 <- 
    df_5 |> left_join(art, by = c("item1" = "id")) |> 
    right_join(cit, by = c("item2" = "id")) |> 
    filter(item1 != item2) |> 
    filter(!is.na(titulo.x))
  
  df_7 <- df_6 |> mutate(item1 = str_remove(item1,".*-")) |> 
    select(-similarity,-titulo.y) |> 
    mutate(item1 = as.character(item1)) |> 
    left_join(citations, by = c("item2" = "id")) |> 
    select(-item2, -title, -year, -pubid) |> 
    rename(id = 1,
           titulo = 2) |> 
    mutate(id = as.double(id))
  
  arti <- articulos |> full_join(df_7, by = c("id" = "id")) |> 
    rename(titulo = 4) |> 
    select(1:9,18,9:16) |> 
    filter(!duplicated(id))
  write_csv(arti,"articulos_citaciones.csv")
  
}
