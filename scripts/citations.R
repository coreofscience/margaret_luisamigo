get_citations <- function(){
  
  library(scholar)
  n <- get_article_cite_history('RGUTAfEAAAAJ', 'Tyk-4Ss8FVUC')
  
  coauthor_network <- get_coauthors('RGUTAfEAAAAJ&hl')
  plot_coauthors(coauthor_network)
  write.csv(coauthor_network, "coautores.csv", row.names = F)
  
  profile <- get_profile('RGUTAfEAAAAJ&hl')
  publications <- get_publications('RGUTAfEAAAAJ&hl')
  
  citations = tibble()
  row <- get_publications("opg3irUAAAAJ")
  for(i in researchers$id_scholar){
    row <- get_publications(i)
    Sys.sleep(3)
    citations <- rbind(citations, row)
  }
}