merge_quality_articles_ucla <- function(articulos_unicos){
  
  journal_2016 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=1992863285") |> 
    mutate(ano = "2016") |> 
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |> 
    separate_rows(ISSN, sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 2) |> 
    select(2,3,1,5,4)
  
  journal_2017_2018 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=219349269") |> 
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |> 
    mutate(ano = "2017,2018") |> 
    select(-VIGENCIA) |> 
    separate_rows(ISSN, sep = ",") |> 
    separate_rows(ano , sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 1) |> 
    select(1,2,4,5,3)
  
  journal_2019_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=758989915") |> 
    unite(ISSN, c("issn_impreso","issn_electronico", "issn_l"), sep = ",", remove = TRUE) |> 
    mutate(ano = "2019,2020") |> 
    select(-VIGENCIA) |> 
    separate_rows(ISSN, sep = ",") |> 
    separate_rows(ano , sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 1) |> 
    select(1,2,4,5,3)
  
  national_journals_2016_2020 <- rbind(journal_2016, journal_2017_2018, journal_2019_2020)
  
  international_journals <- read_csv(here("output",
                                          "journals_international_2016_2020.csv")) |> 
    separate_rows(ISSN, sep = "; ")
  
  articulos <- articulos_unicos |>
    left_join(international_journals, by =c("ISSN", "ano"))
  
  |> 
    
    
    select(1:7,16,8:14) |> 
    rename(categoria = 2,
           categoria_revista = 8)
}