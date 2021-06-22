data_cleaning_main <- function(grupo_df) {
  
  grupos_main_cleaned <- 
    grupo_df[["grupo_main"]] |> 
    mutate(fecha_creacion = ym(fecha_creacion),
           departamento = str_remove(departamento_ciudad, 
                                    "-.*"),
           departamento = str_trim(departamento),
           ciudad = str_remove(departamento_ciudad, 
                                    ".*-"),
           ciudad = str_trim(ciudad),
           clasificacion = substr(clasificacion, 
                                  start = 1, 
                                  stop = 2),
           clasificacion = str_remove(clasificacion, 
                                      "\r"),
           area_conocimiento_1 = str_remove(area_conocimiento,
                                           "--.*"),
           area_conocimiento_0 = str_extract(area_conocimiento,
                                            "--.*"),
           area_conocimiento_0 = str_remove(area_conocimiento,
                                             "--"),
           area_conocimiento_2 = str_remove(area_conocimiento_0,
                                           "--.*"),
           area_conocimiento_3 = str_remove(area_conocimiento,
                                           ".*--")) |> 
    select(grupo,
           fecha_creacion,
           departamento,
           ciudad,
           lider,
           web,
           email,
           clasificacion,
           area_conocimiento_1,
           area_conocimiento_2,
           area_conocimiento_3)
  
  return(grupos_main_cleaned)
}

data_cleaning_product <- function(grupo_df) { 
  
  trabajos_dirigidos = trabajos_dirigidos_ucla(grupo_df[["grupo_product"]])
  eventos_cientificos = eventos_cientificos_ucla(grupo_df[["grupo_product"]])
  articulos = articulos_ucla(grupo_df[["grupo_product"]])
  proyectos = proyectos_ucla(grupo_df[["grupo_product"]])
  capitulos = capitulos_ucla(grupo_df[["grupo_product"]])
  jurado = jurado_ucla(grupo_df[["grupo_product"]])
  cursos = cursos_ucla(grupo_df[["grupo_product"]])
  otros_articulos = otros_articulos_ucla(grupo_df[["grupo_product"]])
  consultorias = consultorias_ucla(grupo_df[["grupo_product"]])
  libros = libros_ucla(grupo_df[["grupo_product"]])
  participacion_comites <- participacion_comites_ucla(grupo_df[["grupo_product"]])
  demas_trabajos <- demas_trabajos_ucla(grupo_df[["grupo_product"]])
  informes_investigacion <- informes_investigacion_ucla(grupo_df[["grupo_product"]])
  innovaciones_gestion <- innovaciones_gestion_ucla(grupo_df[["grupo_product"]])
  generacion_multimedia <- generacion_multimedia_ucla(grupo_df[["grupo_product"]])
  otra_publicacion_divulgativa <- otra_publicacion_divulgativa_ucla(grupo_df[["grupo_product"]])
  documentos_trabajo <- documentos_trabajo_ucla(grupo_df[["grupo_product"]])
  ediciones <- ediciones_ucla(grupo_df[["grupo_product"]])
  estrategias_pedagogicas <- estrategias_pedagogicas_ucla(grupo_df[["grupo_product"]])
  redes_conocimiento <-  redes_conocimiento_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_virtual <- generacion_contenido_virtual_ucla(grupo_df[["grupo_product"]])
  espacios_participacion <- espacios_participacion_ucla(grupo_df[["grupo_product"]])
  softwares <- softwares_ucla(grupo_df[["grupo_product"]])
  innovaciones_procesos <- innovaciones_procesos_ucla(grupo_df[["grupo_product"]])
  otros_libros <- otros_libros_ucla(grupo_df[["grupo_product"]])
  estrategias_comunicacion <- estrategias_comunicacion_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_impreso <- generacion_contenido_impreso_ucla(grupo_df[["grupo_product"]])
  informes_tecnicos <- informes_tecnicos_ucla(grupo_df[["grupo_product"]])
  participacion_ciudadana_cti <- participacion_ciudadana_cti_ucla(grupo_df[["grupo_product"]])
  regulaciones_normas <- regulaciones_normas_ucla(grupo_df[["grupo_product"]])
  actividades_evaluador <- actividades_evaluador_ucla(grupo_df[["grupo_product"]])
  actividades_formacion <- actividades_formacion_ucla(grupo_df[["grupo_product"]])
  apropiacion_social_conocimiento <- apropiacion_social_conocimiento_ucla(grupo_df[["grupo_product"]])
  produccion_tecnica_tecnologica <- produccion_tecnica_tecnologica_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_audio <- generacion_contenido_audio_ucla(grupo_df[["grupo_product"]])
  conceptos_tecnicos <- conceptos_tecnicos_ucla(grupo_df[["grupo_product"]])
  reglamentos_tecnicos<- reglamentos_tecnicos_ucla(grupo_df[["grupo_product"]])
  otros_productos_tencologicos <- otros_productos_tencologicos_ucla(grupo_df[["grupo_product"]])
  traducciones <- traducciones_ucla(grupo_df[["grupo_product"]])
  signos_distintivos <- signos_distintivos_ucla(grupo_df[["grupo_product"]])
  nuevos_registros_cientificos <- nuevos_registros_cientificos_ucla(grupo_df[["grupo_product"]])
  
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
              actividades_evaluador = actividades_evaluador,
              actividades_formacion = actividades_formacion,
              apropiacion_social_conocimiento = apropiacion_social_conocimiento,
              produccion_tecnica_tecnologica = produccion_tecnica_tecnologica,
              generacion_contenido_audio = generacion_contenido_audio,
              conceptos_tecnicos = conceptos_tecnicos,
              reglamentos_tecnicos=reglamentos_tecnicos,
              otros_productos_tencologicos = otros_productos_tencologicos,
              traducciones = traducciones,
              signos_distintivos = signos_distintivos,
              nuevos_registros_cientificos = nuevos_registros_cientificos))
  
}



data_getting_product <- function(data_grupos_all){
  
  grupo_df <- 
    tibble(grupo = character(),
           producto = character(),
           categoria = character())
  
  for (i in 1:length(grupos$url)) {
    
    grupo <- 
      data_grupos_all[[i]] |> 
      html_table()
    
    for (j in 14:71) {
      
      df_1 = 
        grupo %>% 
        tibble() %>% 
        slice(j) %>% 
        unlist %>% 
        tibble() %>% 
        rename(producto = ".") %>% 
        mutate(grupo = grupos$grupo[i])
      
      if (length(df_1$producto) > 1) {
        
        df_2 =
          df_1 %>% 
          filter(producto != "") %>% 
          mutate(categoria = df_1$producto[1]) %>% 
          filter(str_detect(producto, "^[0-9]\\.*")) %>% 
          select(grupo, producto, categoria)
        
      } else {
        
        df_2 = 
          df_1 %>% 
          mutate(categoria = df_1$producto[1],
                 producto = "NO TIENE") %>% 
          select(grupo, producto, categoria) %>% 
          unique()
        
      }
      
      grupo_df <- 
        bind_rows(df_2,
                  grupo_df) 
      
    }
  }
  
  rm(df_1, df_2, grupo, i, j)
  return(grupo_df)
}

data_getting_main <- function(data_grupos_all){
  
  df_group_main = tibble()
  
  for (i in 1:length(data_grupos_all)) {
    
    df_1 = 
      data_grupos_all[[i]] |> 
      html_table()
    
    df_2 = 
      df_1[[1]] |> 
      column_to_rownames("X1") |> 
      t() |>
      as.data.frame() |> 
      rename(fecha_creacion = 2,
             departamento_ciudad = 3,
             lider = 4,
             web = 6,
             email = 7,
             clasificacion = 8,
             area_conocimiento = 9) |> 
      select(fecha_creacion,
             departamento_ciudad,
             lider,
             web,
             email,
             clasificacion,
             area_conocimiento) |> 
      mutate(grupo = grupos$grupo[i])
    
    df_group_main <- bind_rows(df_group_main, df_2)
  }
  return(df_group_main)
}

data_getting_researcher <- function(data_grupos_all){
  
  df_researcher = tibble()
  
  for (i in 1:length(data_grupos_all)) {
    
    df_i <- html_nodes(data_grupos_all[[i]],"a") %>% 
      html_attr("href") %>% 
      tibble() %>%
      mutate(grupo = grupos$grupo[i]) |> 
      rename(url = 1) |> 
      slice(-1,-2)
    
    df_1 = 
      data_grupos_all[[i]] |> 
      html_table()
    
    df_2 = 
      df_1[[5]] |> 
      slice(-1,-2) |> 
      rename(nombre = X1,
             vinculacion = X2,
             horas_dedicacion = X3,
             inicio_fin_vinculacion = X4) |> 
      mutate(integrantes = str_remove(nombre, ".*-"),
             integrantes = str_trim(integrantes)) |> 
      select(integrantes,
             vinculacion, 
             horas_dedicacion, 
             inicio_fin_vinculacion,
             -nombre) |> 
      cbind(df_i)
    
    df_researcher <- bind_rows(df_researcher,df_2)  
  }
  return(df_researcher)
}

trabajos_dirigidos_ucla <- function(grupo_df) {
  
  trabajosdirigidos = 
    grupo_df %>%
    filter(categoria == "Trabajos dirigidos/turorías") %>% 
    separate(producto,
             c("info_1","info_2","info_3","info_4","info_5",
               "info_6","info_7","info_8","info_9","info_10",
               "info_11","info_12","info_13","info_14"), 
             sep = "\r\n" ) %>% 
    select(-info_4,-info_5,-info_6,-info_8,-info_12,-info_14) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(desde = str_trim(info_2),
           desde = str_remove(desde, "Desde"),
           desde = str_remove(desde, " hasta"),
           desde = str_trim(desde),
           hasta = str_trim(info_3),
           hasta = str_remove(hasta, ",.*"),
           tipo_orientacion =str_remove(info_3, ".*: "),
           estudiante = str_trim(info_7),
           estudiante = str_remove(estudiante, ",$"),
           programa_academico = str_remove(info_9, ".*:"),
           programa_academico = str_trim(programa_academico),
           paginas = str_remove(info_10, ".*páginas: "),
           paginas= str_remove(paginas, ",.*"),
           valoracion= str_extract(info_10, ",.*"),
           valoracion= str_remove(valoracion, "^,"),
           valoracion= str_remove(valoracion, ".*:"),
           valoracion= str_remove(valoracion, ","),
           valoracion= str_trim(valoracion),
           institucion = str_trim(info_11),
           institucion = str_remove(institucion, ".*Institución: "),
           tutor_coautor = str_trim(info_13),
           tutor_coautor = str_remove(tutor_coautor, ".*: ")) %>% 
    select(-info_2,
           -info_3,
           -info_7,
           -info_9,
           -info_10,
           -info_11,
           -info_13)
  
  return(trabajosdirigidos)
  
}

eventos_cientificos_ucla <- function(grupo_df) {
  
  grupo_df_EventosCientificos <- 
    grupo_df %>%
    filter(categoria == "Eventos Científicos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"), 
             sep = "\r\n" )%>% 
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_4),
           tipo_evento = str_remove(info_1, ":.*"),
           tipo_evento = str_remove(tipo_evento, ".*-"),
           tipo_evento = str_trim(tipo_evento),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(ciudad_evento= str_remove(info_2, ",.*"),
           info_2 = str_remove(info_2, ".*desde*"),
           fecha_inicio = str_remove(info_2, "-$"),
           fecha_fin = str_remove(info_3, ".*hasta")) %>% 
    select(-info_2,-info_3) %>% 
    mutate(info_4 = str_remove(info_4, "Ámbito:"),
           ambito = str_remove(info_4, ",.*"),
           info_4= str_extract(info_4, "Tipos de participación:.*"),
           info_4= str_remove(info_4, ".*Tipos de participación:"),
           tipo_participacion=str_remove(info_4,"Nombre de la institución.*"),
           info_4= str_extract(info_4, "Nombre de la institución.*"),
           nombre_Institución= str_remove(info_4, ".*Nombre de la institución:")) %>% 
    select(-info_4) %>% 
    mutate(tipo_vinculación = str_remove(info_5,"Nombre.*"),
           tipo_vinculación = str_remove(tipo_vinculación,"Ámbito.*"),
           tipo_vinculación = str_trim(tipo_vinculación)) %>% 
    select(-info_5)
  
  return(grupo_df_EventosCientificos)
}

articulos_ucla <- function(grupo_df) {
  
  grupo_df_articulos_lost_1 <- 
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>% 
    separate(producto,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    filter(!is.na(info_5)) %>% 
    slice(1) %>% 
    select(-info_4) %>% 
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_3),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(pais_revista = str_remove(info_2, ",.*"),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = info_2,
           info_2 = str_extract(info_4, "ISSN.*"),
           info_2 = str_trim(info_4),
           ISSN = str_remove(info_4, ",.*"),
           ISSN = str_remove(ISSN, "ISSN:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_4, ",.*"),
           info_2 = str_remove(info_4, "^,"),
           info_2 = str_trim(info_4),
           ano = str_remove(info_4, "\\s.*"),
           info_2 = str_extract(info_4, "\\s.*"),
           info_2 = str_trim(info_4),
           vol = str_remove(info_4, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_2 = str_extract(info_4, "\\s.*"),
           info_2 = str_trim(info_4),
           fasc = str_remove(info_4, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_2 = str_extract(info_4, "págs.*"),
           info_2 = str_trim(info_4),
           pags = str_remove(info_4, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_4, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>% 
    select(-info_4,
           -info_2) %>% 
    mutate(autores = str_remove(info_5, "Autores: "),
           autores = str_remove(autores, ",$"),
           autores = str_trim(autores)) %>% 
    select(-info_5,
           -info_3)
  
  grupo_df_articulos_lost_2 <- 
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>% 
    separate(producto,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    filter(!is.na(info_5)) %>% 
    slice(2) %>% 
    select(-info_4) %>%
    mutate(info_2 = str_trim(info_2),
           info_5 = str_trim(info_5),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(pais_revista = str_remove(info_2, ",.*"),
           pais_revista = str_remove(pais_revista, ".*,"),
           revista = str_remove(info_3, "ISSN.*"),
           info_3 = str_extract(info_3, "ISSN.*"),
           info_3 = str_trim(info_3),
           ISSN = str_remove(info_3, ",.*"),
           info_3 = str_extract(info_3, ",.*"),
           info_3 = str_remove(info_3, "^,"),
           info_3 = str_trim(info_3),
           ano = str_remove(info_3, "\\s.*"),
           info_3 = str_extract(info_3, "\\s.*"),
           info_3 = str_trim(info_3),
           vol = str_remove(info_3, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_3 = str_extract(info_3, "\\s.*"),
           info_3 = str_trim(info_3),
           fasc = str_remove(info_3, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_3 = str_extract(info_3, "págs.*"),
           info_3 = str_trim(info_3),
           pags = str_remove(info_3, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_3, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>% 
    select(-info_2,
           -info_3) %>% 
    mutate(autores = str_remove(info_5, "Autores: "),
           autores = str_remove(autores, ",$"),
           autores = str_trim(autores)) %>% 
    select(-info_5) %>% 
    mutate(DOI = str_remove(DOI, "http://dx.doi.org/"))
  
  grupo_df_articulos <- 
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    filter(is.na(info_5)) %>% 
    select(-info_3,
           -info_5) %>% 
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_4),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    select(-info_1) %>% 
    mutate(pais_revista = str_remove(info_2, ",.*"),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = str_remove(info_2, "ISSN.*"),
           info_2 = str_extract(info_2, "ISSN.*"),
           info_2 = str_trim(info_2),
           ISSN = str_remove(info_2, ",.*"),
           ISSN = str_remove(ISSN, "ISSN:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           ano = str_remove(info_2, "\\s.*"),
           info_2 = str_extract(info_2, "\\s.*"),
           info_2 = str_trim(info_2),
           vol = str_remove(info_2, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_2 = str_extract(info_2, "\\s.*"),
           info_2 = str_trim(info_2),
           fasc = str_remove(info_2, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_2 = str_extract(info_2, "págs.*"),
           info_2 = str_trim(info_2),
           pags = str_remove(info_2, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_2, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>% 
    select(-info_2) %>% 
    mutate(autores = str_remove(info_4, "Autores: "),
           autores = str_remove(autores, ",$")) %>% 
    select(-info_4)
  
  return(grupo_df_articulos)
}

proyectos_ucla <- function(grupo_df) {
  
  grupo_df_proyectos <- 
    grupo_df %>%
    filter(categoria == "Proyectos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_proyecto = str_remove(info_1, ":.*"),
           Tipo_proyecto = str_remove(Tipo_proyecto, ".*-"),
           Tipo_proyecto = str_trim(Tipo_proyecto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1, -info_2) %>% 
    mutate(info_3= str_trim(info_3),
           Fecha_inicio= str_remove(info_3, "-$"),
           Fecha_inicio= str_trim(Fecha_inicio)) %>% 
    select(-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Fecha_Fin=str_extract(info_4, ".*")) %>% 
    select(-info_4,-info_5)
}

capitulos_ucla <- function(grupo_df) {
  
  grupo_df_capitulos_libros_publicados = 
    grupo_df %>%
    filter(categoria == "Capítulos de libro publicados") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4"), 
             sep = "\r\n" ) %>% 
    select(-info_3) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo_capitulo = str_extract(info_1, ":.*"),
           titulo_capitulo = str_remove(titulo_capitulo, "^:"),
           titulo_capitulo = str_trim(titulo_capitulo),
           pais = str_remove(info_2, ", \\d.*"),
           pais = str_trim(pais),
           ano = str_remove(info_2, ", ISBN.*"),
           ano = str_extract(ano, ", .*"),
           ano = str_remove(ano, "^,"),
           ano = str_remove(ano, ",.*"),
           ano = str_trim(ano),
           titulo_libro = str_extract(info_2, "\\d, .*"),
           titulo_libro = str_remove(titulo_libro, ", ISBN.*"),
           titulo_libro = str_remove(titulo_libro, ".*, "),
           ISBN = str_remove(info_2, ".*ISBN: "),
           ISBN = str_remove(ISBN, ", Vol.*"), 
           vol = str_remove(info_2, ".*Vol."),
           vol = str_remove(vol, ", pág.*"),
           pags = str_remove(info_2, ".*pág.:"),
           pags = str_remove(pags, ",.*"),
           editorial = str_remove(info_2, ".*Ed. "),
           autores = str_remove(info_4, ".*Autores: ")) %>% 
    select(-info_1, -info_2, -info_4)
}

jurado_ucla <- function(grupo_df) {
  
  grupo_df_Jurado <- 
    grupo_df %>%
    filter(categoria == "Jurado/Comisiones evaluadoras de trabajo de grado") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7"), 
             sep = "\r\n" ) %>% 
    select(-info_6) %>% 
    mutate(Nivel_Academico = str_remove(info_1, ":.*"),
           Nivel_Academico = str_remove(Nivel_Academico, ".*-"),
           Nivel_Academico = str_trim(Nivel_Academico),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
    mutate(Pais=str_trim(pais),
           ano=str_trim(anno),
           Idioma=str_remove(info_3,",.*"),
           Idioma=str_extract(Idioma,":.*"),
           Idioma=str_remove(Idioma,".*:"),
           Idioma=str_trim(Idioma),
           Medio_divulgacion=str_extract(info_3,"n:.*"),
           Medio_divulgacion=str_remove(info_3,".*:"),
           Medio_divulgacion=str_trim(Medio_divulgacion),
           Sitio_Web=str_remove(info_4,",.*"),
           Sitio_Web=str_remove(Sitio_Web,".*:"),
           Sitio_Web=str_trim(Sitio_Web),
           Nombre_del_Orientado=str_extract(info_4,",.*"),
           Nombre_del_Orientado=str_extract(Nombre_del_Orientado,":.*"),
           Nombre_del_Orientado=str_remove(Nombre_del_Orientado,":"),
           Nombre_del_Orientado=str_trim(Nombre_del_Orientado),
           Programa_Academico=str_remove(info_5,",.*"),
           Programa_Academico=str_remove(Programa_Academico,".*:"),
           Programa_Academico=str_trim(Programa_Academico),
           Institucion=str_extract(info_5,",.*"),
           Institucion=str_extract(Institucion,":.*"),
           Institucion=str_remove(Institucion,":"),
           Institucion=str_remove(Institucion,".$"),
           Institucion=str_trim(Institucion),
           Autores=str_remove(info_7,".*:"),
           Autores=str_trim(Autores)) %>% 
    select(-info_1,-info_3,-info_4,-info_5,-info_7,-pais,-anno)
}

cursos_ucla <- function(grupo_df) {
  
  grupo_df_CursosCortaDuracion <- 
    grupo_df %>%
    filter(categoria == "Curso de Corta Duración Dictados") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_Curso = str_remove(info_1, ":.*"),
           Tipo_Curso = str_remove(Tipo_Curso, ".*-" ),
           Nombre_curso = str_remove(info_1, ".*:"),
           Nombre_curso = str_trim(Nombre_curso)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Ano = str_extract(info_2, ",.*"),
           Ano = str_remove(Ano, ","),
           Ano = str_trim(Ano),
           Ano = str_remove(Ano, ",")) %>% 
    select(-info_2) %>% 
    mutate(info_3=str_trim(info_3),
           Idioma=str_extract(info_3, ".*,"),
           Idioma=str_remove(Idioma, ","),
           Idioma=str_remove(Idioma, ".*: "),
           Medio_divulgacion= str_remove(info_3, ".*: ")) %>% 
    select(-info_3) %>% 
    mutate(info_4= str_trim(info_4),
           sitio_web= str_remove(info_4, ",.*"),
           sitio_web=str_remove(sitio_web, ".*:"),
           Participacion=str_remove(info_4, ".*, "),
           Participacion=str_remove(Participacion, ","),
           Participacion=str_remove(Participacion, "Participación como"),
           Participacion=str_trim(Participacion)
    ) %>% 
    select(-info_4) %>% 
    mutate(info_5= str_trim(info_5),
           Duracion_semanas= str_remove(info_5, ",.*"),
           Duracion_semanas=str_remove(Duracion_semanas, ".*:"),
           Duracion_semanas= str_trim(Duracion_semanas),
           Finalidad= str_remove(info_5,".*Finalidad:"),
           Finalidad= str_trim(Finalidad)) %>%
    select(-info_5) %>% 
    mutate(info_6= str_trim(info_6),
           lugar= str_remove(info_6, ",.*"),
           lugar= str_remove(lugar, ".*:"),
           lugar= str_trim(lugar),
           Institucion_Financiadora= str_extract(info_6, "Institución financiadora:.*"),
           Institucion_Financiadora= str_remove(Institucion_Financiadora, "Institución financiadora:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>% 
    select(-info_6,-info_7) %>% 
    mutate(info_8 = str_trim(info_8),
           Autores = str_remove(info_8,".*:")) %>% 
    select(-info_8)
}

otros_articulos_ucla <- function(grupo_df) {
  
  grupo_df_otros_articulos <- 
    grupo_df %>%
    filter(categoria == "Otros artículos publicados") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    unite(info_2,"info_2",c("info_2","info_3"),sep = "",remove = TRUE) %>% 
    unite(info_4,"info_4",c("info_4","info_5"),sep = "",remove = TRUE) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           info_2 = str_extract(info_2,",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = str_remove(info_2,"ISSN.*"),
           revista = str_trim(revista),
           info_2 = str_extract(info_2, "ISSN.*"),
           info_2 = str_trim(info_2),
           ISSN = str_remove(info_2,",.*"),
           ISSN = str_extract(ISSN,":.*"),
           ISSN = str_remove(ISSN, "^:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_2,",.*"),
           ano = str_remove(info_2,"vol.*"),
           ano = str_remove(ano,"^,"),
           ano = str_trim(ano),
           info_2 = str_extract(info_2,"vol.*"),
           vol = str_remove(info_2,"fasc.*"),
           vol = str_remove(vol,".*:"),
           vol = str_remove(vol,"^:"),
           vol = str_trim(vol),
           fasc = str_extract(info_2,".*p"),
           fasc = str_remove(fasc,"p$"),
           fasc = str_extract(fasc,"c.*"),
           fasc = str_extract(fasc,":.*"),
           fasc = str_remove(fasc,"^:"),
           fasc = str_trim(fasc),
           pags = str_extract(info_2,"gs.*"),
           pags = str_extract(pags,":.*"),
           pags = str_remove(pags,"^:"),
           pags = str_trim(pags),
           autores = str_remove(info_4, "Autores: "),
           autores = str_trim(autores)) %>% 
    select(-info_1,-info_2,-info_4) 
}

consultorias_ucla <- function(grupo_df) {
  
  grupo_df_consultorias_cientico_tecnologicas <- 
    grupo_df%>%
    filter(categoria == "Consultorías científico-tecnológicas") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_Consultoria = str_remove(info_1, ":.*"),
           Tipo_Consultoria = str_trim(Tipo_Consultoria),
           Tipo_Consultoria = str_remove(Tipo_Consultoria, ".*\\d."),
           Tipo_Consultoria = str_remove(Tipo_Consultoria, "."),
           Tipo_Consultoria = str_trim(Tipo_Consultoria),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1, -info_2) %>% 
    separate(info_3,
             c("i_1","i_2","i_3","i_4"),
             sep = ",") %>%
    mutate(i_1 = str_trim(i_1),
           i_1= str_remove(i_1, ".*:"),
           Ano_inicio= str_extract(i_1, ".*"),
           i_2= str_trim(i_2),
           i_2= str_remove(i_2, ".*:"),
           Mes_inicio= str_extract(i_2, ".*"),
           i_3 = str_trim(i_3),
           i_3= str_remove(i_3, ".*:"),
           Ano_fin= str_extract(i_3, ".*"),
           i_4= str_trim(i_4),
           i_4= str_remove(i_4, ".*:"),
           Mes_fin= str_extract(i_4, ".*")) %>% 
    select(-i_1,-i_2,-i_3,-i_4) %>% 
    mutate(info_5=str_trim(info_5),
           Idioma= str_remove(info_5, ",.*"),
           Idioma= str_remove(Idioma, ".*:"),
           Idioma= str_trim(Idioma),
           Ciudad= str_extract(info_5, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_remove(Ciudad, ",.*"),
           Ciudad= str_remove(Ciudad, ".*:"),
           Ciudad= str_trim(Ciudad),
           Disponibilidad= str_extract(info_5, "Disponibilidad.*"),
           Disponibilidad= str_remove(Disponibilidad, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Duracion= str_extract(info_5, "Duración.*"),
           Duracion= str_remove(Duracion, ".*:"),
           Duracion= str_remove(Duracion, ",$"),
           Duracion= str_trim(Duracion)
    ) %>% 
    select(-info_4,-info_5) %>% 
    mutate(info_6=str_trim(info_6),
           Num_contrato= str_remove(info_6, ",.*"),
           Num_contrato= str_remove(Num_contrato, ".*:"),
           Num_contrato= str_trim(Num_contrato),
           Institucion_Prestadora_servicio= str_extract(info_6, ",.*"),
           Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, "^,"),
           Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, ".*:"),
           Institucion_Prestadora_servicio= str_trim(Institucion_Prestadora_servicio)
    ) %>% 
    select(-info_6,-info_7)
}

libros_ucla <- function(grupo_df) {
  
  grupo_df_librosPublicados <- 
    grupo_df %>%
    filter(categoria == "Libros publicados") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_Libro = str_remove(info_1, ":.*"),
           Tipo_Libro = str_remove(Tipo_Libro, ".*-" ),
           Tipo_Libro = str_trim(Tipo_Libro),
           Titulo = str_remove(info_1, ".*investigación :"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Ano = str_extract(info_2, ",.*"),
           Ano = str_remove(Ano, ","),
           Ano = str_trim(Ano),
           Ano = str_remove(Ano, ",.*"),
           ISBN= str_extract(info_2, "ISBN.*"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ".*:"),
           ISBN = str_trim(ISBN),
           Volumen = str_extract(info_2,"vol:.*"),
           Volumen = str_remove(Volumen, "págs:.*"),
           Volumen= str_remove(Volumen, "vol:"),
           Volumen=str_trim(Volumen),
           Paginas= str_extract(info_2,"págs:.*,"),
           Paginas= str_remove(Paginas, ",.*"),
           Paginas= str_remove(Paginas, "págs:"),
           Paginas=str_trim(Paginas),
           Editorial= str_extract(info_2,"Ed.*"),
           Editorial= str_remove(Editorial,"Ed."),
           Editorial=str_trim(Editorial)) %>% 
    select(-info_2,-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_4)
  
}

participacion_comites_ucla <- function(grupo_df) {
  
  #Data cleaning "Participación en comités de evaluación"
  
  grupo_df_participacion_comites <- 
    grupo_df %>%
    filter(categoria == "Participación en comités de evaluación") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"), 
             sep = "\r\n" ) %>% 
    select(-info_5) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           sitio_web = str_remove(info_3,".*web"),
           sitio_web = str_remove(sitio_web,"^:"),
           sitio_web = str_trim(sitio_web),
           medio_divulgacion = str_remove(info_4,",.*"),
           medio_divulgacion = str_remove(medio_divulgacion,".*:"),
           medio_divulgacion = str_trim(medio_divulgacion),
           institucion = str_remove(info_4,".*,"),
           institucion = str_remove(institucion,".*:"),
           institucion = str_trim(institucion),
           autores = str_remove(info_6,".*:"),
           autores = str_trim(autores)) %>% 
    select(-info_1,-pais,-anno,-info_3,-info_4,-info_6)
  
  
}

demas_trabajos_ucla <- function(grupo_df) {
  
  grupo_df_demas_trabajos <- 
    grupo_df%>%
    filter(categoria == "Demás trabajos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2=str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Pais= str_trim(Pais),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ",$")) %>% 
    select(-info_2) %>% 
    mutate(info_3=str_trim(info_3),
           Idioma= str_remove(info_3, ",.*"),
           Idioma= str_remove(Idioma, ".*:"),
           Idioma= str_trim(Idioma),
           Medio_divulgacion= str_extract(info_3, ",.*"),
           Medio_divulgacion= str_remove(Medio_divulgacion, "^,"),
           Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
           Medio_divulgacion= str_trim(Medio_divulgacion)) %>% 
    select(-info_3, -info_4) %>% 
    mutate(info_5=str_trim(info_5),
           Autores= str_remove(info_5, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_5) 
  
}

informes_investigacion_ucla <- function(grupo_df) {
  
  grupo_df_informe_de_investigacion <- 
    grupo_df %>%
    filter(categoria == "Informes de investigación") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:" ),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Ano = str_remove(info_2, ",.*"),
           Ano = str_trim(Ano),
           Proyecto_de_investigacion = str_extract(info_2, ",.*" ),
           Proyecto_de_investigacion = str_extract(Proyecto_de_investigacion, ":.*"),
           Proyecto_de_investigacion = str_remove(Proyecto_de_investigacion, "^:"),
           Proyecto_de_investigacion = str_trim(Proyecto_de_investigacion)) %>% 
    select(-info_2,-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_4)
  
}

innovaciones_gestion_ucla <- function(grupo_df) {
  
  grupo_df_innov_gestion_empresarial <- 
    grupo_df %>%
    filter(categoria == "Innovaciones generadas en la Gestión Empresarial") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    select(-info_4) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           disponibilidad = str_remove(info_3,",.*"),
           disponibilidad = str_remove(disponibilidad,".*:"),
           disponibilidad = str_trim(disponibilidad),
           info_3 = str_extract(info_3, ",.*"),
           info_3 = str_extract(info_3, ":.*"),
           info_3 = str_remove(info_3, "^:"),
           institucion_financiadora = str_trim(info_3),
           autores = str_remove(info_5,".*:"),
           autores = str_trim(autores)) %>% 
    select(-info_1,-pais,-anno,-info_3,-info_5)
  
  
}

generacion_multimedia_ucla <- function(grupo_df) {
  
  grupo_df_generacion_contenido_multimedia <- 
    grupo_df%>%
    filter(categoria == "Generación de Contenido Multimedia") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_generacion = str_remove(info_1, ":.*"),
           Tipo_generacion = str_trim(Tipo_generacion),
           Tipo_generacion = str_remove(Tipo_generacion, ".*-"),
           Tipo_generacion = str_trim(Tipo_generacion),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Ano= str_remove(info_2, ",.*"),
           Ano= str_trim(Ano),
           Pais= str_extract(info_2, ",.*"),
           Pais= str_remove(Pais, "^,"),
           Pais= str_remove(Pais, ",$")) %>% 
    select(-info_2) %>% 
    mutate(info_3=str_trim(info_3),
           Idioma= str_remove(info_3, ".*:"),
           Idioma= str_trim(Idioma)) %>% 
    select(-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Medio_divulgacion= str_remove(info_4, ",.*"),
           Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
           Medio_divulgacion= str_trim(Medio_divulgacion),
           Sitio_web= str_extract(info_4, ",.*"),
           Sitio_web= str_remove(Sitio_web, "^,"),
           Sitio_web= str_remove(Sitio_web, ".*:"),
           Sitio_web= str_trim(Sitio_web)) %>% 
    select(-info_4) %>% 
    mutate(info_5=str_trim(info_5),
           Emisora= str_remove(info_5, ",.*"),
           Emisora= str_remove(Emisora, ".*:"),
           Emisora= str_trim(Emisora),
           Instituciones_participantes= str_extract(info_5, ",.*"),
           Instituciones_participantes= str_remove(Instituciones_participantes, "^,"),
           Instituciones_participantes= str_remove(Instituciones_participantes, ".*:"),
           Instituciones_participantes= str_trim(Instituciones_participantes)) %>% 
    select(-info_5,-info_6) %>% 
    mutate(info_7=str_trim(info_7),
           Autores= str_remove(info_7, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_7) 
  
}

otra_publicacion_divulgativa_ucla <- function(grupo_df) {
  
  grupo_df_otra_publicacion_divulgativa <- 
    grupo_df %>%
    filter(categoria == "Otra publicación divulgativa") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_Publicacion_divulgativa = str_remove(info_1, ":.*"),
           Tipo_Publicacion_divulgativa = str_remove(Tipo_Publicacion_divulgativa, ".*-" ),
           Tipo_Publicacion_divulgativa = str_trim( Tipo_Publicacion_divulgativa),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:" ),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           pais= str_remove(info_2, ",.*"),
           pais= str_trim(pais),
           ano=str_extract(info_2, ",.*"),
           ano = str_remove(ano, "^,"),
           info_2=str_extract(ano, ",.*"),
           ano= str_remove(ano, ",.*"),
           libro= str_remove(info_2, "^,"),
           libro= str_remove(libro, "vol.*"),
           libro= str_remove(libro, ",.$"),
           libro= str_extract(libro, ".*,"),
           libro= str_remove(libro, ",$"),
           libro=str_trim(libro),
           ISBN= str_remove(info_2, "^,"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ",.$"),
           ISBN= str_remove(ISBN, ".*,"),
           ISBN= str_trim(ISBN),
           volumen=str_extract(info_2, "vol.*"),
           volumen=str_remove(volumen, ",.*"),
           volumen=str_remove(volumen, "vol."),
           volumen=str_trim(volumen),
           Paginas=str_extract(info_2, "págs.*"),
           Paginas=str_remove(Paginas, ",.*"),
           Paginas=str_remove(Paginas, ".*:"),
           Paginas=str_trim(Paginas),
           Informacion=str_extract(info_2, "págs.*"),
           Informacion=str_extract(Informacion, ",.*"),
           Informacion=str_remove(Informacion, "^,"),
           info_2=str_extract(Informacion, ".*"),
           Informacion=str_extract(Informacion, ".*,"),
           Informacion=str_remove(Informacion, ",$"),
           Informacion=str_trim(Informacion),
           Informacion=str_remove(Informacion, "^-"),
           Informacion=str_trim(Informacion),
           Editorial=str_remove(info_2, ".*,"),
           Editorial=str_remove(Editorial, "Ed."),
           Editorial=str_trim(Editorial)
    ) %>% 
    select(-info_2,-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_4)
}

documentos_trabajo_ucla <- function(grupo_df) {
  
  grupo_df_documentos_trabajo <- 
    grupo_df %>%
    filter(categoria == "Documentos de trabajo") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", 
               "info_6", "info_7", "info_8", "info_9", "info_10", "info_11"), 
             sep = "\r\n" ) %>% 
    unite(info_4,"info_4",c("info_4","info_5", "info_6", "info_7", "info_8", 
                            "info_9", "info_10", "info_11"),sep = " ",remove = TRUE) %>%
    separate(info_4, c("info_4","info_5"), sep = "URL") %>% 
    separate(info_5, c("info_5", "info_6", "info_7"), sep = "DOI") %>%
    unite(info_6, "info_6", c("info_6", "info_7"),sep = "",remove = TRUE) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           ano = str_remove(info_2, ",$"),
           ano = str_trim(ano),
           numero_paginas = str_remove(info_3, ".*:"),
           numero_paginas = str_remove(numero_paginas, ",$"),
           numero_paginas = str_trim(numero_paginas),
           instituciones_participantes = str_remove(info_4, ".*:"),
           instituciones_participantes = str_remove(instituciones_participantes,","),
           instituciones_participantes = str_trim(instituciones_participantes),
           instituciones_participantes = str_remove(instituciones_participantes,",$"),
           URL = str_remove(info_5, "^:"),
           URL = str_remove(URL, ","),
           DOI = str_remove(info_6, "Autores.*"),
           DOI = str_remove(DOI, ":"),
           DOI = str_remove(DOI, ":"),
           DOI = str_trim(DOI),
           autores = str_remove(info_6, ".*Autores: "),
           autores = str_remove(autores, " NA .*")) %>% 
    select(-info_1,-info_2,-info_3,-info_4,-info_5,-info_6)
}

ediciones_ucla <- function(grupo_df) {
  
  grupo_df_ediciones <- 
    grupo_df%>%
    filter(categoria == "Ediciones") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Medio = str_remove(info_1, ":.*"),
           Medio = str_remove(Medio, ".*-"),
           Medio = str_trim(Medio),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Pais= str_trim(Pais),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ",$")) %>% 
    select(-info_2) %>% 
    mutate(info_3=str_trim(info_3),
           Editorial=str_remove(info_3, ",.*"),
           Editorial=str_remove(Editorial, ".*:"),
           Editorial=str_trim(Editorial),
           Idioma= str_remove(info_3, ".*:"),
           Idioma= str_remove(Idioma, ",$"),
           Idioma= str_trim(Idioma)) %>% 
    select(-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Paginas= str_remove(info_4, ".*:"),
           Paginas= str_trim(Paginas)) %>% 
    select(-info_4,-info_5) %>% 
    mutate(info_6=str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_6) 
  
}

estrategias_pedagogicas_ucla <- function(grupo_df) {
  
  grupo_df_estrategias_pedagogicas <- 
    grupo_df %>%
    filter(categoria == "Estrategias Pedagógicas para el fomento a la CTI") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7",
               "info_8","info_9","info_10","info_11","info_12","info_13","info_14"
               ,"info_15","info_16","info_17","info_18","info_19","info_20","info_21",
               "info_22","info_23","info_24","info_25","info_26"),
             sep = "\r\n" ) %>% 
    unite( info_3,c(4:27),  sep = ",", remove = TRUE) %>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_remove(info_1, "desde.*"),
           Titulo = str_remove(Titulo, ".*- "),
           Titulo= str_remove(Titulo, ": $"),
           Titulo = str_trim(Titulo),
           Fecha_inicio= str_extract(info_1, "desde.*"),
           Fecha_inicio= str_remove(Fecha_inicio, "desde"),
           Fecha_inicio= str_remove(Fecha_inicio, "hasta"),
           Fecha_inicio= str_trim(Fecha_inicio)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Fecha_Fin= str_extract(info_2, ".*")) %>%
    select(-info_2) %>% 
    mutate(info_3= str_trim(info_3),
           Descripcion= str_remove(info_3, "Descripción:"),
           Descripcion= str_remove(Descripcion, "NA.*"),
           Descripcion= str_remove(Descripcion, ",$"),
           Descripcion= str_trim(Descripcion)) %>% 
    select(-info_3)
}

redes_conocimiento_ucla <- function(grupo_df) {
  
  grupo_df_redes_conocimiento <- 
    grupo_df %>%
    filter(categoria == "Redes de Conocimiento Especializado") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3"), 
             sep = "\r\n" ) %>% 
    mutate(titulo = str_remove(info_1, ":.*"),
           titulo = str_remove(titulo, ".*\\d.-"),
           titulo = str_trim(titulo),
           tipo_red = str_remove(info_1, ".*:"),
           tipo_red = str_trim(tipo_red),
           pais_ciudad = str_remove(info_2, ", desde.*"),
           pais_ciudad = str_remove(pais_ciudad, ".*en "),
           desde = str_remove(info_2, ".*desde "),
           desde = str_trim(desde),
           desde = str_remove(desde, "-$"),
           desde = str_trim(desde),
           hasta = str_remove(info_3, " Nro.*"),
           hasta = str_remove(hasta, ".*hasta "),
           numero_participantes = str_remove(info_3, ".*:"),
           numero_participantes = str_trim(numero_participantes)) %>% 
    select(-info_1, -info_2, -info_3)
  
  
}

generacion_contenido_virtual_ucla <- function(grupo_df) {
  
  grupo_df_generacion_contenido_virtual <- 
    grupo_df%>%
    filter(categoria == "Generación de Contenido Virtual") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Medio = str_remove(info_1, ":.*"),
           Medio = str_remove(Medio, ".*-"),
           Medio = str_trim(Medio),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Fecha= str_remove(info_2, ",.*"),
           Fecha= str_trim(Fecha),
           Entidades_vinculadas= str_extract(info_2, ",.*"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, "^,"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, ".*:"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, ",$")) %>% 
    select(-info_2) %>% 
    mutate(info_3=str_trim(info_3),
           Sitio_web= str_remove(info_3, "Sitio web:"),
           Sitio_web= str_trim(Sitio_web))%>% 
    select(-info_3,-info_4) %>% 
    mutate(info_5=str_trim(info_5),
           Autores= str_remove(info_5, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_5) 
  
}

espacios_participacion_ucla <- function(grupo_df) {
  
  grupo_df_espacio_participacion_ciudadano<- 
    grupo_df %>%
    filter(categoria == "Espacios de Participación Ciudadana") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ".*en"),
           Titulo = str_remove(Titulo, " en$"),
           Titulo = str_remove(Titulo, ".*- "),
           Titulo = str_trim(Titulo),
           Ciudad = str_remove(info_1,".*en"),
           Ciudad = str_trim(Ciudad)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Fecha_inicio = str_remove(info_2,"hasta.*"),
           Fecha_inicio = str_remove(Fecha_inicio, "desde"),
           Fecha_inicio = str_remove(Fecha_inicio, "- $"),
           Fecha_inicio =str_trim(Fecha_inicio),
           Fecha_Fin = str_extract(info_2, "hasta.*"),
           Fecha_Fin = str_remove(Fecha_Fin, "hasta"),
           Fecha_Fin = str_trim(Fecha_Fin)) %>% 
    select(-info_2) %>% 
    mutate(info_3 = str_trim(info_3),
           N_participantes= str_extract(info_3, ".*,"),
           N_participantes= str_remove(N_participantes, ".*:"),
           N_participantes= str_remove(N_participantes, ",$"),
           N_participantes= str_trim(N_participantes),
           Pag_web= str_extract(info_3, ",.*"),
           Pag_web= str_remove(Pag_web, "^,"),
           Pag_web= str_remove(Pag_web, ".*:"),
           Pag_web= str_trim(Pag_web)) %>% 
    select(-info_3)
  
}

softwares_ucla <- function(grupo_df) {
  
  grupo_df_softwares <- 
    grupo_df %>%
    filter(categoria == "Softwares") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6", "info_7"), 
             sep = "\r\n" ) %>% 
    select(-info_6) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>% 
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>% 
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           disponibilidad = str_remove(info_3, ",.*"),
           disponibilidad = str_remove(disponibilidad, ".*:"),
           disponibilidad = str_remove(disponibilidad, "^:"),
           disponibilidad = str_trim(disponibilidad),
           sitio_web = str_remove(info_3, ".*web:"),
           sitio_web = str_trim(sitio_web),
           nombre_comercial = str_remove(info_4, ",.*"),
           nombre_comercial = str_remove(nombre_comercial, "Nombre comercial: "),
           nombre_comercial = str_trim(nombre_comercial),
           nombre_proyecto = str_remove(info_4, ".*Nombre del proyecto"),
           nombre_proyecto = str_remove(nombre_proyecto, "^:"),
           nombre_proyecto = str_trim(nombre_proyecto),
           institucion_financiadora = str_remove(info_5, ".*:"),
           institucion_financiadora = str_trim(institucion_financiadora),
           autores = str_remove(info_7, ".*Autores: ")) %>% 
    select(-info_1,-pais,-anno,-info_3,-info_4,-info_5,-info_7)
}

innovaciones_procesos_ucla <- function(grupo_df) {
  
  grupo_df_inn_procesos_procedimientos <- 
    grupo_df %>%
    filter(categoria == "Innovaciones en Procesos y Procedimientos") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    select(-info_4) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           disponibilidad = str_remove(info_3, ",.*"),
           disponibilidad = str_remove(disponibilidad, ".*: "),
           institucion_financadora = str_remove(info_3, ".*dora: "),
           autores = str_remove(info_5, ".*Autores: ")) %>% 
    select(-info_1, -info_2, -info_3, -info_5)
  
}

otros_libros_ucla <- function(grupo_df) {
  grupo_df_otros_libros_publicados<- 
    grupo_df %>%
    filter(categoria == "Otros Libros publicados") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Tipo_libro = str_remove(info_1, ":.*"),
           Tipo_libro = str_remove(Tipo_libro, ".*- "),
           Tipo_libro = str_trim(Tipo_libro),
           Titulo= str_extract(info_1, ":.*"),
           Titulo= str_remove(Titulo, "^:"),
           Titulo= str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Pais = str_trim(Pais),
           Ano = str_extract(info_2, ",.*"),
           Ano = str_remove(Ano, ","),
           Ano = str_trim(Ano),
           Ano = str_remove(Ano, ",.*"),
           ISBN= str_extract(info_2, "ISBN.*"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ".*:"),
           ISBN = str_trim(ISBN),
           Volumen = str_extract(info_2,"vol:.*"),
           Volumen = str_remove(Volumen, "págs:.*"),
           Volumen= str_remove(Volumen, "vol:"),
           Volumen=str_trim(Volumen),
           Paginas= str_extract(info_2,"págs:.*,"),
           Paginas= str_remove(Paginas, ",.*"),
           Paginas= str_remove(Paginas, "págs:"),
           Paginas=str_trim(Paginas),
           Editorial= str_extract(info_2,"Ed.*"),
           Editorial= str_remove(Editorial,"Ed."),
           Editorial=str_trim(Editorial)) %>% 
    select(-info_2,-info_3) %>% 
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_4)
}

estrategias_comunicacion_ucla <- function(grupo_df) {
  
  grupo_df_estreategias_comunicacion <- 
    grupo_df %>%
    filter(categoria == "Estrategias de Comunicación del Conocimiento") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6", 
               "info_7", "info_8", "info_9", "info_10", "info_11", "info_12", 
               "info_13", "info_14", "info_15","info_16", "info_17", "info_18"), 
             sep = "\r\n" ) %>%
    unite("info_3", c("info_3", "info_4", "info_5", "info_6", 
                      "info_7", "info_8", "info_9", "info_10", "info_11", "info_12", 
                      "info_13", "info_14", "info_15","info_16", "info_17", "info_18"),
          sep = " ",remove = TRUE) %>% 
    mutate(tipo_producto = str_remove(info_1, ": desde.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           desde = str_remove(info_1, ".*desde "),
           desde = str_remove(desde, " hasta.*"),
           hata = str_trim(info_2),
           info_3 = str_remove(info_3, "NA.*"),
           descripcion = str_remove(info_3, ".*Descripción: "),
           descripcion = str_trim(descripcion)) %>% 
    select(-info_1, -info_2, -info_3)
  
}

generacion_contenido_impreso_ucla <- function(grupo_df) {
  
  grupo_df_generacion_cont_impreso <- 
    grupo_df %>%
    filter(categoria == "Generación de Contenido Impreso") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"), 
             sep = "\r\n" ) %>%
    select(-info_5) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           fecha = str_remove(info_2, ", Ambito.*"),
           fecha = str_trim(fecha),
           ambito = str_remove(info_2, ".*: "),
           ambito = str_trim(ambito),
           ambito = str_remove(ambito, ","),
           medio_circulacion = str_remove(info_3, " Lugar.*"),
           medio_circulacion = str_remove(medio_circulacion, ".*: "),
           medio_circulacion = str_trim(medio_circulacion),
           lugar_publicacion = str_remove(info_3, ".*:"),
           lugar_publicacion = str_remove(lugar_publicacion, ","),
           lugar_publicacion = str_trim(lugar_publicacion),
           sitio_web = str_remove(info_4, ".*web: "),
           autores = str_remove(info_6, ".*Autores: "),
           autores = str_trim(autores)) %>% 
    select(-info_1, -info_2, -info_3, -info_4, -info_6)
  
}

informes_tecnicos_ucla <- function(grupo_df) {
  
  grupo_df_informes_tecnicos<- 
    grupo_df %>%
    filter(categoria == "Informes técnicos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1, -info_2) %>%
    separate(info_3,
             c("ano","mes","idioma","ciudad"),
             sep = ",") %>% 
    mutate(ano = str_trim(ano),
           ano= str_remove(ano, ".*:"),
           Ano= str_extract(ano, ".*"),
           mes= str_trim(mes),
           mes= str_remove(mes, ".*:"),
           Mes= str_extract(mes, ".*"),
           idioma = str_trim(idioma),
           idioma= str_remove(idioma, ".*:"),
           Idioma= str_extract(idioma, ".*"),
           ciudad = str_trim(ciudad),
           ciudad= str_remove(ciudad, ".*:"),
           Ciudad = str_extract(ciudad, ".*"),
    ) %>% 
    select(-info_4,-ano,-mes,-idioma,-ciudad) %>% 
    separate(info_5,
             c("i_1","i_2","i_3","i_4"),
             sep = ",") %>%
    mutate(Disponibilidad= str_extract(i_1, ".*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Num_pag= str_extract(i_2, ".*"),
           Num_pag= str_remove(Num_pag, ".*:"),
           Num_pag= str_trim(Num_pag),
           Num_contrato= str_extract(i_3, ".*"),
           Num_contrato= str_remove(Num_contrato, ".*:"),
           Num_contrato= str_trim(Num_contrato),
           Institucion_Presta_servicio= str_extract(i_4, ".*"),
           Institucion_Presta_servicio= str_remove(Institucion_Presta_servicio, ".*:"),
           Institucion_Presta_servicio= str_trim(Institucion_Presta_servicio)) %>% 
    select(-i_1,-i_2,-i_3,-i_4)
}

participacion_ciudadana_cti_ucla <- function(grupo_df) {
  
  grupo_df_participacion_cti <- 
    grupo_df %>%
    filter(categoria == "Participación Ciudadana en Proyectos de CTI") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"), 
             sep = "\r\n" ) %>% 
    unite("info_3", c("info_3", "info_4", "info_5","info_6", "info_7", "info_8", "info_9"),
          sep = "",remove = TRUE) %>%
    mutate(titulo = str_remove(info_1, ":.*"),
           titulo = str_remove(titulo, ".*\\d.-"),
           titulo = str_trim(titulo),
           desde = str_remove(info_1, ".*: desde"),
           desde = str_remove(desde, "hasta.*"),
           desde = str_trim(desde),
           hasta = str_trim(info_2),
           descripcion = str_remove(info_3, ".*:"),
           descripcion = str_remove(descripcion, "NANANA.*")) %>% 
    select(-info_1, -info_2, -info_3)
}

regulaciones_normas_ucla <- function(grupo_df) {
  
  grupo_df_regulaciones_normas <- 
    grupo_df %>%
    filter(categoria == "Regulaciones y Normas") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"), 
             sep = "\r\n" ) %>% 
    select(-info_5) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           ambito = str_remove(info_3, ", Fecha.*"),
           ambito = str_remove(ambito, ".*: "),
           fecha = str_remove(info_3, ".*publicación: "),
           fecha = str_trim(fecha),
           objeto = str_remove(info_4, ".*Objeto: "),
           objeto = str_trim(objeto),
           autores = str_remove(info_6, ".*Autores: ")) %>% 
    select(-info_1, -info_2, -info_3, -info_4, -info_6)
  
}

actividades_evaluador_ucla <- function(grupo_df) {
  
  grupo_df_actividades_evaluador<- 
    grupo_df %>%
    filter(categoria == "ACTIVIDADES COMO EVALUADOR")
  
}

actividades_formacion_ucla <- function(grupo_df) {
  
  grupo_df_actividades_formacion <- 
    grupo_df %>%
    filter(categoria == "ACTIVIDADES DE FORMACIÓN")
  
}

apropiacion_social_conocimiento_ucla <- function(grupo_df) {
  
  grupo_df_apropiacion_social <- 
    grupo_df %>%
    filter(categoria == "APROPIACIÓN SOCIAL Y CIRCULACIÓN DEL CONOCIMIENTO")
  
}

produccion_tecnica_tecnologica_ucla <- function(grupo_df) {
  
  grupo_df_produccion_tecnica_tecnologica<- 
    grupo_df %>%
    filter(categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA")
  
}

generacion_contenido_audio_ucla <- function(grupo_df) {
  
  grupo_df_generacion_audio <- 
    grupo_df %>%
    filter(categoria == "Generaciónes de contenido de audio") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>% 
    select(-info_2, -info_4) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d. "),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           ano = str_remove(info_3, ", Mes.*"),
           ano = str_remove(ano, ".*: "),
           mes = str_remove(info_3, ", Ciudad.*"),
           mes = str_remove(mes, ".*: "),
           ciudad = str_remove(info_3, ".*Ciudad: "),
           formato_archivo_digital = str_remove(info_5, ", Descripcion.*"),
           formato_archivo_digital = str_remove(formato_archivo_digital, ".*: "),
           descripcion_audio = str_remove(info_5, ".*audio: ")) %>% 
    select(-info_1, -info_3, -info_5)
  
}

conceptos_tecnicos_ucla <- function(grupo_df) {
  
  grupo_df_conceptos_tecnicos <- 
    grupo_df %>%
    filter(categoria == "Conceptos técnicos") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"), 
             sep = "\r\n" ) %>%
    select(-info_2, -info_4) %>% 
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d."),
           tipo_producto = str_trim(tipo_producto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo),
           ano_solicitud = str_remove(info_3, ", Mes.*"),
           ano_solicitud = str_remove(ano_solicitud, ".*: "),
           ano_solicitud = str_trim(ano_solicitud),
           mes_solicitud = str_remove(info_3, ", Fecha.*"),
           mes_solicitud = str_remove(mes_solicitud, ".*: "),
           fecha_envio = str_remove(info_3, ".*envío: "),
           institucion_solicitante = str_remove(info_5, ", Ciudad.*"),
           institucion_solicitante = str_remove(institucion_solicitante, ".*solicitante: "),
           ciudad = str_remove(info_5, ", Número.*"),
           ciudad = str_remove(ciudad, ".*Ciudad: "),
           numero_cosecutivo_concepto = str_remove(info_5, ".*concepto: ")) %>% 
    select(-info_1, -info_3, -info_5)
  
}

reglamentos_tecnicos_ucla <- function(grupo_df) {
  
  grupo_df_reglamentos_tecnicos <- 
    grupo_df %>%
    filter(categoria == "Reglamentos técnicos") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5","info_6"), 
             sep = "\r\n" ) %>%
    mutate(Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>% 
    mutate(info_2= str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ","),
           Ano= str_trim(Ano)) %>% 
    select(-info_2) %>% 
    mutate(info_3 = str_trim(info_3),
           Disponibilidad= str_remove(info_3, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Sitio_Web= str_extract(info_3, ",.*"),
           Sitio_Web= str_remove(info_3, "^,"),
           Sitio_Web= str_remove(Sitio_Web, ".*:"),
           Sitio_Web= str_trim(Sitio_Web)) %>% 
    select(-info_3) %>% 
    mutate(info_4= str_trim(info_4),
           Institucion_Financiadora= str_remove(info_4, ".*:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>% 
    select(-info_4,-info_5) %>% 
    mutate(info_6= str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_6)
}

otros_productos_tencologicos_ucla <- function(grupo_df) {
  
  grupo_df_otros_productos_tecnologicos<- 
    grupo_df %>%
    filter(categoria == "Otros productos tecnológicos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1) %>%
    mutate(info_2= str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ","),
           Ano= str_trim(Ano)) %>% 
    select(-info_2) %>% 
    mutate(info_3 = str_trim(info_3),
           Disponibilidad= str_remove(info_3, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Nombre_comercial= str_extract(info_3, ",.*"),
           Nombre_comercial= str_remove(info_3, "^,"),
           Nombre_comercial= str_remove(Nombre_comercial, ".*:"),
           Nombre_comercial= str_trim(Nombre_comercial)) %>% 
    select(-info_3) %>% 
    mutate(info_4= str_trim(info_4),
           Institucion_Financiadora= str_remove(info_4, ".*:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>% 
    select(-info_4,-info_5) %>% 
    mutate(info_6= str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>% 
    select(-info_6)
}

traducciones_ucla <- function(grupo_df) {
  
  grupo_df_traducciones <- 
    grupo_df %>%
    filter(categoria == "Traducciones") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"), 
             sep = "\r\n" ) %>% 
    select(-info_8) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_remove(info_1, ".*: "),
           ano = str_remove(info_2, ",.*"),
           ano = str_trim(ano),
           revista = str_remove(info_2, ".*: "),
           revista = str_remove(revista, " ISSN.*"),
           revista = str_trim(revista),
           ISSN = str_remove(info_3, ", Libro.*"),
           ISSN = str_trim(ISSN), 
           libro = str_remove(info_3, ".*: "),
           libro = str_remove(libro, " ISBN.*"),
           ISBN = str_remove(info_4, ", Medio.*"),
           ISBN = str_trim(ISBN),
           medio_divulgacion = str_remove(info_4, ".*ción: "),
           idioma_documento_original = str_remove(info_5, ",.*"),
           idioma_documento_original = str_remove(idioma_documento_original, ".*: "),
           idioma_traduccion = str_remove(info_5, ".*ción: "),
           edicion = str_remove(info_6, ", Serie.*"),
           edicion = str_remove(edicion, ".*: "),
           serie = str_remove(info_6, ".*Serie: "),
           serie = str_remove(serie, ","),
           serie = str_trim(serie),
           autor_documento_original = str_remove(info_7, ".*: "),
           autores = str_remove(info_9, ".*: ")) %>% 
    select(-info_1, -info_2, -info_3, -info_4, 
           -info_5, -info_6, -info_7, -info_9)
}

signos_distintivos_ucla <- function(grupo_df){
  
  grupo_df_signos_distintivos <- 
    grupo_df %>%
    filter(categoria == "Signos distintivos") %>% 
    separate(producto ,
             c("info_1", "info_2", "info_3"), 
             sep = "\r\n" ) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           numero_registro = str_remove(info_3, ", Nombre.*"),
           numero_registro = str_remove(numero_registro, ".*: "),
           nombre_titular = str_remove(info_3, ".*titular: ")) %>% 
    select(-info_1, -info_2, -info_3)
}

nuevos_registros_cientificos_ucla <- function(grupo_df) {
  
  grupo_df_nuevos_registros_cientificos<- 
    grupo_df %>%
    filter(categoria == "Nuevos registros científicos") %>% 
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8","info_9"), 
             sep = "\r\n" )%>% 
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>% 
    select(-info_1, -info_2) %>%
    mutate(info_3= str_trim(info_3),
           Ano= str_remove(info_3, ",.*"),
           Ano= str_remove(Ano, ".*:"),
           Ano= str_trim(Ano),
           Mes= str_extract(info_3, ",.*"),
           Mes= str_remove(Mes, "^,"),
           Mes= str_remove(Mes, ",.*"),
           Mes= str_remove(Mes, ".*:"),
           Mes= str_trim(Mes),
           Ciudad= str_extract(info_3, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_extract(Ciudad, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_remove(Ciudad, ".*:"),
           Ciudad= str_trim(Ciudad)) %>% 
    select(-info_3, -info_4) %>% 
    mutate(info_5= str_trim(info_5),
           Base_de_Datos= str_remove(info_5, ",.*"),
           Base_de_Datos= str_remove(Base_de_Datos, ".*:"),
           Base_de_Datos= str_trim(Base_de_Datos),
           Sitio_web= str_extract(info_5, ",.*"),
           Sitio_web= str_remove(Sitio_web, "^,"),
           Sitio_web= str_remove(Sitio_web, ",.*"),
           Sitio_web= str_remove(Sitio_web, ".*:"),
           Sitio_web= str_trim(Sitio_web),
           Institucion= str_extract(info_5, ",.*"),
           Institucion= str_remove(Institucion, "^,"),
           Institucion= str_extract(Institucion, ",.*"),
           Institucion= str_remove(Institucion, "^,"),
           Institucion= str_remove(Institucion, ".*:"),
           Institucion= str_trim(Institucion)) %>% 
    select(-info_5, -info_6) %>%
    mutate(info_7= str_trim(info_7),
           Instituccion_certificadora= str_remove(info_7, ",.*"),
           Instituccion_certificadora= str_remove(Instituccion_certificadora, ".*:"),
           Instituccion_certificadora= str_trim(Instituccion_certificadora),
           Descripcion_registro= str_extract(info_7, ",.*"),
           Descripcion_registro= str_remove(Descripcion_registro, "^,"),
           Descripcion_registro= str_extract(Descripcion_registro, ":.*"),
           Descripcion_registro= str_remove(Descripcion_registro, "^:"),
           Descripcion_registro= str_trim(Descripcion_registro)) %>% 
    select(-info_7, -info_8) %>%
    mutate(Descripcion=str_extract(info_9, ".*"),
           Descripcion= str_trim(Descripcion)) %>% 
    select(-info_9)
}
