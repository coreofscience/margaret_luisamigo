source(here("scripts",
            "functions.R"))

data_cleaning_ucla <- function(grupo_df) {
  
  trabajos_dirigidos = trabajos_dirigidos_ucla(grupo_df)
  eventos_cientificos = eventos_cientificos_ucla(grupo_df)
  articulos = articulos_ucla(grupo_df)
  proyectos = proyectos_ucla(grupo_df)
  capitulos = capitulos_ucla(grupo_df)
  jurado = jurado_ucla(grupo_df)
  cursos = cursos_ucla(grupo_df)
  otros_articulos = otros_articulos_ucla(grupo_df)
  consultorias = consultorias_ucla(grupo_df)
  libros = libros_ucla(grupo_df)
  participacion_comites <- participacion_comites_ucla(grupo_df)
  demas_trabajos <- demas_trabajos_ucla(grupo_df)
  informes_investigacion <- informes_investigacion_ucla(grupo_df)
  innovaciones_gestion <- innovaciones_gestion_ucla(grupo_df)
  generacion_multimedia <- generacion_multimedia_ucla(grupo_df)
  otra_publicacion_divulgativa <- otra_publicacion_divulgativa_ucla(grupo_df)
  documentos_trabajo <- documentos_trabajo_ucla(grupo_df)
  ediciones <- ediciones_ucla(grupo_df)
  estrategias_pedagogicas <- estrategias_pedagogicas_ucla(grupo_df)
  redes_conocimiento <-  redes_conocimiento_ucla(grupo_df)
  generacion_contenido_virtual <- generacion_contenido_virtual_ucla(grupo_df)
  espacios_participacion <- espacios_participacion_ucla(grupo_df)
  softwares <- softwares_ucla(grupo_df)
  innovaciones_procesos <- innovaciones_procesos_ucla(grupo_df)
  otros_libros <- otros_libros_ucla(grupo_df)
  estrategias_comunicacion <- estrategias_comunicacion_ucla(grupo_df)
  generacion_contenido_impreso <- generacion_contenido_impreso_ucla(grupo_df)
  informes_tecnicos <- informes_tecnicos_ucla(grupo_df)
  participacion_ciudadana_cti <- participacion_ciudadana_cti_ucla(grupo_df)
  regulaciones_normas <- regulaciones_normas_ucla(grupo_df)
  generacion_contenido_audio <- generacion_contenido_audio_ucla(grupo_df)
  reglamentos_tecnicos<- reglamentos_tecnicos_ucla(grupo_df)
  otros_productos_tencologicos <- otros_productos_tencologicos_ucla(grupo_df)
  traducciones <- traducciones_ucla(grupo_df)
  signos_distintivos <- signos_distintivos_ucla(grupo_df)
  nuevos_registros_cientificos <- nuevos_registros_cientificos_ucla(grupo_df)
  actividades_evaluador <- actividades_evaluador_ucla(grupo_df)
  actividades_formacion <- actividades_formacion_ucla(grupo_df)
  apropiacion_social_conocimiento <- apropiacion_social_conocimiento_ucla(grupo_df)
  produccion_tecnica_tecnologica <- produccion_tecnica_tecnologica_ucla(grupo_df)
  conceptos_tecnicos <- conceptos_tecnicos_ucla(grupo_df)
  
  return(list(trabajos_dirigidos = trabajos_dirigidos,
              eventos_cientificos = eventos_cientificos,
              articulos = articulos,
              proyectos = proyectos,
              capitulos = capitulos, 
              jurado = jurado, 
              cursos = cursos,
              otros_articulos = otros_articulos,
              consultorias = consultorias,
              libros = libros, 
              participacion_comites = participacion_comites,
              demas_trabajos = demas_trabajos,
              informes_investigacion = informes_investigacion,
              innovaciones_gestion = innovaciones_gestion,
              generacion_multimedia = generacion_multimedia,
              otra_publicacion_divulgativa = otra_publicacion_divulgativa,
              documentos_trabajo = documentos_trabajo,
              ediciones = ediciones,
              estrategias_pedagogicas = estrategias_pedagogicas,
              redes_conocimiento =  redes_conocimiento,
              generacion_contenido_virtual = generacion_contenido_virtual,
              espacios_participacion = espacios_participacion,
              softwares = softwares,
              innovaciones_procesos = innovaciones_procesos,
              otros_libros = otros_libros,
              estrategias_comunicacion = estrategias_comunicacion,
              generacion_contenido_impreso = generacion_contenido_impreso,
              informes_tecnicos = informes_tecnicos,
              participacion_ciudadana_cti = participacion_ciudadana_cti,
              regulaciones_normas = regulaciones_normas,
              generacion_contenido_audio = generacion_contenido_audio,
              reglamentos_tecnicos=reglamentos_tecnicos,
              otros_productos_tencologicos = otros_productos_tencologicos,
              traducciones = traducciones,
              signos_distintivos = signos_distintivos,
              nuevos_registros_cientificos = nuevos_registros_cientificos,
              actividades_evaluador = actividades_evaluador,
              actividades_formacion = actividades_formacion,
              apropiacion_social_conocimiento = apropiacion_social_conocimiento,
              produccion_tecnica_tecnologica = produccion_tecnica_tecnologica,
              conceptos_tecnicos = conceptos_tecnicos))
  
}
