merge_quality_articles_ucla <- function(articulos_unicos){
  
  journal_2016 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=1992863285") |> 
    mutate(ano = "2016",
           categoria = ifelse(categoria == "A1"|
                              categoria == "A2"|
                              categoria == "B"|
                              categoria == "C",categoria,"B")) |> 
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |> 
    separate_rows(ISSN, sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 2) |> 
    select(2,3,1,5,4)|> 
    mutate(ano = as.character(ano))
  
  journal_2017_2018 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=219349269") |> 
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |> 
    mutate(ano = "2017,2018,2019,2020",
           categoria = ifelse(categoria == "A1"|
                                categoria == "A2"|
                                categoria == "B"|
                                categoria == "C",categoria,"B")) |> 
    select(-VIGENCIA) |> 
    separate_rows(ISSN, sep = ",") |> 
    separate_rows(ano , sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 1) |> 
    select(1,2,4,5,3)|> 
    mutate(ano = as.character(ano))
  
  journal_2019_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=758989915") |> 
    unite(ISSN, c("issn_impreso","issn_electronico", "issn_l"), sep = ",", remove = TRUE) |> 
    mutate(ano = "2019,2020,2021",
           categoria = ifelse(categoria == "A1"|
                                categoria == "A2"|
                                categoria == "B"|
                                categoria == "C",categoria,"B")) |> 
    select(-VIGENCIA) |> 
    separate_rows(ISSN, sep = ",") |> 
    separate_rows(ano , sep = ",") |> 
    filter(!str_detect(ISSN, "NA")) |> 
    rename(revista = 1) |> 
    select(1,2,4,5,3)|> 
    mutate(ano = as.character(ano))
  
  journal_2017_2018 <- anti_join(journal_2017_2018, journal_2019_2020, by=c("ISSN","ano"))
  
  national_journals_2016_2020 <- rbind(journal_2016, journal_2017_2018, journal_2019_2020)
  
  international_journals <- read_csv(here("output",
                                          "journals_international_2016_2020.csv")) |> 
    separate_rows(ISSN, sep = "; ") |> 
    mutate(ano = as.character(ano))
  
  articulos <- articulos_unicos |>
    left_join(international_journals, by =c("ISSN", "ano")) |> 
    select(1:7,16,8:14) |> 
    rename(categoria = 2,
           categoria_revista = 8)
  
  articulos_df <- articulos |> 
    filter(is.na(categoria_revista)) |> 
    left_join(national_journals_2016_2020, by =c("ISSN", "ano"))|> 
    select(1:7,17,9:15) |> 
    rename(categoria = 2,
           revista = 6,
           categoria_revista = 8)
  
  articulos_national <- articulos |> filter(!is.na(categoria_revista)) 
  articulos_unicos <- rbind(articulos_national, articulos_df)
  return(articulos_unicos)
}
