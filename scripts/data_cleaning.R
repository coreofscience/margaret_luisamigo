source(here("scripts",
            "functions.R"))

data_cleaning_ucla <- function(grupos_df) {
  
  trabajos_dirigidos = trabajos_dirigidos_ucla(grupos_df)
  eventos_cientificos = eventos_cientificos_ucla(grupos_df)
  articulos = articulos_ucla(grupos_df)
  proyectos = proyectos_ucla(grupos_df)
  capitulos = capitulos_ucla(grupos_df)
  jurado = jurado_ucla(grupos_df)
  cursos = cursos_ucla(grupos_df)
  otros_articulos = otros_articulos_ucla(grupos_df)
  
  
  return(list(trabajos_dirigidos = trabajos_dirigidos,
              eventos_cientificos = eventos_cientificos))
  
}
