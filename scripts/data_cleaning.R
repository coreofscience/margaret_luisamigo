source(here("scripts",
            "functions.R"))

data_cleaning_ucla <- function(grupos_df) {
  
  trabajos_dirigidos = trabajos_dirigidos_ucla(grupos_df)
  eventos_cientificos = eventos_cientificos_ucla(grupos_df)
  articulos = articulos_ucla(grupos_df)
  
  
  
  
  return(list(trabajos_dirigidos = trabajos_dirigidos,
              eventos_cientificos = eventos_cientificos))
  
}
