data_analysis_descriptive_ucla <- function(produccion_actualizada) {
  
  produccion_actualizada[[1]] <- 
    make_general_grupos(produccion_actualizada) |> 
    left_join(grupos, by = "grupo")
  
  produccion_actualizada
  
}
