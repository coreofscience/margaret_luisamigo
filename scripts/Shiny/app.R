library(shiny)
library(shinythemes)
library(datasets)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(readxl)
library(stringi)
library(shinydashboard)

articulos_unicos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  mutate(SJR_Q = ifelse(SJR_Q == '-', "Sin categoria",
         SJR_Q))

investigadores_general <- 
  read_csv(here("output",
                "investigadores.csv")) 

grupos_general <- 
  read_csv(here("output",
                "grupos_general.csv")) 

paises_general <- articulos_unicos_2016_2020 |> 
  count(pais_revista, sort = TRUE)
paises_general$porcentaje <- round(prop.table(paises_general$n),3)*100 

revistas_actuales <-
  read_csv(here("output", 
                "current_journals.csv")) 

articulos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) 

capitulos_2016_2020 <- 
  read_csv(here("output",
                "capitulos.csv")) 

libros_2016_2020 <- 
  read_csv(here("output",
                "libros.csv")) 

software_2016_2020 <- 
  read_csv(here("output",
                "softwares.csv")) 

trabajo_2016_2020 <- 
  read_csv(here("output",
                "trabajos_dirigidos.csv")) 

innovacion_2016_2020 <- 
  read_csv(here("output",
                "innovaciones_gestion.csv"))
#-----------------------------------------------------------------------------------------------------#
#dataframe filtros
#filtro grupo

grupos <- grupos_general |>
  select(grupo) |>
  unique()

general_grupos <- list(grupos$grupo)

#-----------------------------------------------------------------------------------------------------#
#Inicio
filterside <- selectInput("grupos_input","Grupos:", 
                          c('General'= FALSE, grupos$grupo),
                          selectize = FALSE)

#butonside <- actionButton("aplicar_input", "Aplicar")

sliderside <- sliderInput("fechas_input", "Años:", min = 1995, max = 2021, value = c(2016,2020), sep = "")

sidebar <- dashboardSidebar(
  filterside,
  sliderside,
  #butonside,
  sidebarMenu(
             menuItem("Datos", tabName = "general_datos", icon = icon("atlas")),
    
    menuItem("Producción cientifica", icon = icon("book"), tabName = ("produccion")),
    
    
    menuItem("Grupos en cifras", icon = icon("bar-chart-o"),
             menuSubItem("Clasificación grupos", tabName = "clasi_grupos"),
             menuSubItem("Clasificación investigadores", tabName = "clasi_inves"),
             menuSubItem("Categoría revistas", tabName = "cate_revista"),
             menuSubItem("Evolución temporal", tabName = "evolu_articulos"),
             menuSubItem("Formación investigadores", tabName = "forma_inves")
    ),
    menuItem("Rpubs", icon = icon("file-code-o"),
             href = "https://rpubs.com/srobledog/margaret"
    )
  ),
  mainPanel(
    textOutput("grupos_input")
  )
)

setup <- dashboardBody(
  tabItems( 
    tabItem(tabName = "general_datos",
            tabsetPanel(type = "tabs",
                        tabPanel("Grupos", fluidPage(br(),(DT::dataTableOutput('ex1'))
                        ),),
                        
                        tabPanel("Investigadores", fluidPage(br(),(DT::dataTableOutput('ex2'))
                        ),),
                        
                        tabPanel("Paises", fluidPage(br(),(DT::dataTableOutput('ex3'))
                        ),),
                        
                        tabPanel("Revistas", fluidPage(br(),(DT::dataTableOutput('ex4'))
                        )))),
    tabItem(tabName = "produccion",
            tabsetPanel(type = "tabs",
                        tabPanel("Articulos", fluidPage(br(),(DT::dataTableOutput('articulo'))
                        )),
                        tabPanel("Capitulos", fluidPage(br(),(DT::dataTableOutput('capitulo'))
                        )),
                        tabPanel("Libros", fluidPage(br(),(DT::dataTableOutput('libro'))
                        )),
                        tabPanel("Software", fluidPage(br(),(DT::dataTableOutput('software'))
                        )),
                        tabPanel("Innovaciones", fluidPage(br(),(DT::dataTableOutput('innovaciones'))
                        )),
                        tabPanel("Trabajos dirigidos/Tutorías",
                                 fluidPage(br(),(DT::dataTableOutput('trabajosd'))
                                 )))),
    tabItem(tabName = "clasi_grupos",
            fluidPage(plotlyOutput("graf1"))),
    
    tabItem(tabName = "clasi_inves",
            fluidPage(plotlyOutput("graf2"))),
    
    tabItem(tabName = "cate_revista",
            fluidPage(
              fluidRow(box(
              title = "PUBLINDEX",width = 8, status = "warning", solidHeader = TRUE,
              collapsible = TRUE, collapsed = T,
              plotlyOutput("graf3", height = 300)
            )),
            fluidRow(box(
              title = "SCIMAGO", width = 8, status = "warning", solidHeader = TRUE,
              collapsible = TRUE,collapsed = T,
              plotlyOutput("graf3_1", height = 300)
            ))
              )),
    
    tabItem(tabName = "evolu_articulos",
            fluidPage(plotlyOutput("graf4"))),
    
    tabItem(tabName = "forma_inves",
            fluidPage(plotlyOutput("graf5")))
  )
)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Margaret",
                  dropdownMenu(type = "notifications", icon = shiny::icon("code"),
                               badgeStatus = "info", headerText = "Desarrolladores",
                               tags$li(a(href = "https://github.com/srobledog",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Sebastian Robledo")),
                               tags$li(a(href = "https://github.com/bryanariasq02",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Bryan Arias")),
                               tags$li(a(href = "https://github.com/camilogs1",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Camilo García"))
                  )),
  sidebar,
  setup
)

server <- function(input, output) {
  
  filtro <- reactive(input$grupos_input)
  
  filtro_fecha_min <- reactive({input$fechas_input[1]})
  
  filtro_fecha_max <- reactive({input$fechas_input[2]})
  
  output$ex1 <- DT::renderDataTable({
    grupos_general <- grupos_general |> 
      select(grupo, clasificacion, sum_papers, departamento , url,
             fecha_creacion,lider, email, area_conocimiento_1) |> 
      mutate(url= str_c("<a href=",
                        url,
                        ">Link</a>"))
    if (filtro()==FALSE)
    {
      datatable(grupos_general, filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Clasificación", "Cantidad artículos",
                             "Departamento","GrupLAC", 
                             "Fecha Creación", "Líder", "Email", 
                             "Área de Conocimiento"),
                class = 'cell-border stripe')
    }
    else
    {
      grupos_general |> 
        filter(grupo == filtro()) |> 
        datatable(filter = 'top',extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Clasificación", "Cantidad artículos",
                               "Departamento","GrupLAC", 
                               "Fecha Creación", "Líder", "Email", 
                               "Área de Conocimiento"),
                  class = 'cell-border stripe')
    }
  })
  
  output$ex2 <- DT::renderDataTable({
    
    investigadores_general <- investigadores_general |> 
      mutate(url = str_c("<a href=","\"",
                         url,
                         "\"",
                         ">Link</a>"),
             scholar = str_c("<a href=","\"",
                             "https://scholar.google.com/citations?user=",
                             id_scholar,
                             "\"",
                             ">Scholar</a>")) |>
      select(-vinculacion,
             -fin_vinculacion) |> 
      rename(Investigador = integrantes,
             Horas = horas_dedicacion,
             CvLAC = url,
             Grupo = grupo,
             Inicio = inicio_vinculacion,
             Formacion = posgrade,
      ) |> 
      select(Investigador,
             Grupo,
             unidad,
             programa,
             articulos,
             capitulos,
             libros,
             softwares,
             trabajos_dirigidos,
             innovaciones,
             h_index,
             clasification,
             Formacion,
             Inicio,
             CvLAC,
             scholar) 
    if (filtro()==FALSE)
    {
      datatable(investigadores_general,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Investigador", "Grupo","Unidad","Programa" , "Artículos", "Capítulos", 
                             "Libros", "Softwares", "Trabajos Dirigidos", 
                             "Innovaciones", "H index", "Categoría",
                             "Formación","Inicio", "CvLAC", "Scholar"),
                class = 'cell-border stripe')
    }
    else 
    {
      investigadores_general |> 
        filter(str_detect(Grupo , filtro() )) |> 
        datatable(filter = 'top',extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Investigador", "Grupo","Unidad","Programa" , "Artículos", "Capítulos", 
                               "Libros", "Softwares", "Trabajos Dirigidos", 
                               "Innovaciones", "H index", "Categoría",
                               "Formación","Inicio", "CvLAC", "Scholar"),
                  class = 'cell-border stripe')
    }
  })
  
  output$ex3 <- DT::renderDataTable({
    
    paises_general <- paises_general |>  
      mutate(porcentaje = str_c(porcentaje," %"),
             pais_revista = if_else(is.na(pais_revista), "No registra", pais_revista)) |> 
      datatable(extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("País", "Cantidad", "Porcentaje"),
                class = 'cell-border stripe')
    
  })
  
  output$ex4 <- DT::renderDataTable({
    
    revistas_actuales <- revistas_actuales |> 
      mutate(porcentaje = str_c(porcentaje," %")) |>  
      datatable(filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE), 
                rownames = FALSE,
                colnames = c('Revista', 'ISSN', 'Categoría Publindex',
                             'Categoría Scimago','Cantidad', 'Porcentaje'),
                class = 'cell-border stripe')
  })
  
  output$articulo <- DT::renderDataTable({
    
    articulos_2016_2020 <- articulos_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      select(-id) |> 
      mutate(DOI = str_extract(DOI, "\\d.*")) |> 
      mutate(DOI =  str_c("<a href=","\"",
                          "https://doi.org/",
                          DOI,
                          "\"",
                          ">Enlace</a>")) 
    if (filtro()==FALSE)
    {
      datatable(articulos_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                class = ('cell-border stripe'),
                colnames = c("Grupo", "Categoría", "Tipo producto",
                             "Título", "País revista", "Revista", 
                             "ISSN","Categoría Publindex", "Categoría Scimago", "Año", "Volumen",
                             "Fasc","Paginas", "Enlace artículo", "Autores"))
    }
    else
    {
      articulos_2016_2020 |> 
        filter(grupo == filtro()) |> 
        datatable(filter = 'top' ,extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  class = ('cell-border stripe'),
                  colnames = c("Grupo", "Categoría", "Tipo producto",
                               "Título", "País revista", "Revista", 
                               "ISSN","Categoría Publindex", "Categoría Scimago", "Año", "Volumen",
                               "Fasc","Paginas", "Enlace artículo", "Autores"))
    }
  })
  
  output$capitulo <- DT::renderDataTable({
    
    capitulos_2016_2020 <- capitulos_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      select(-vol, -tipo_producto)
    if(filtro()==FALSE)
    {
      datatable(capitulos_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título capitulo", "País", "Año", 
                             "Titulo libro","ISBN", "Paginas", "Editorial",
                             "Autores"),
                class = 'cell-border stripe')
    }
    else
    {
      capitulos_2016_2020 |> 
        filter(grupo == filtro()) |> 
        datatable(filter = 'top', extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Categoría",
                               "Título capitulo", "País", "Año", 
                               "Titulo libro","ISBN", "Paginas", "Editorial",
                               "Autores"),
                  class = 'cell-border stripe')
    }
  })
  
  output$libro <- DT::renderDataTable({
    
    libros_2016_2020 <- libros_2016_2020 |> 
      filter(Ano >= filtro_fecha_min(),
             Ano <=filtro_fecha_max()) |> 
      select(-Tipo_producto)  
    if (filtro()==FALSE)
    {
      datatable(libros_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título libro", "País", "Año", 
                             "ISBN","Editorial", "Autores"),
                class = 'cell-border stripe')
    }
    else
    {
      libros_2016_2020 |> 
        filter(grupo == filtro()) |> 
        datatable(filter = 'top', extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Categoría",
                               "Título libro", "País", "Año", 
                               "ISBN","Editorial", "Autores"),
                  class = 'cell-border stripe')
    }
  })
  
  output$software <- DT::renderDataTable({
    
    software_2016_2020 <- software_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      select(-nombre_proyecto, -tipo_producto) |> 
      mutate(sitio_web= str_c("<a href=",
                              sitio_web,
                              ">Link</a>")) 
    if (filtro()==FALSE)
    {
      datatable(software_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título", "País", "Año", 
                             "Disponibilidad","Sitio web", "Nombre comercial", "Institución financiadora", 
                             "Autores"),
                class = 'cell-border stripe')
    }
    else
    {
      software_2016_2020 |> 
        filter(grupo==filtro()) |> 
        datatable(filter = 'top',extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Categoría",
                               "Título", "País", "Año", 
                               "Disponibilidad","Sitio web", "Nombre comercial", "Institución financiadora", 
                               "Autores"),
                  class = 'cell-border stripe')
    }
  })
  
  output$innovaciones <- DT::renderDataTable({
    
    innovacion_2016_2020 <- innovacion_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max())
    if (filtro()==FALSE)
    {
      datatable(innovacion_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría", "Tipo Producto",
                             "Título", "país", "Año", 
                             "Disponibilidad","Institución financiadora", "Autores"),
                class = 'cell-border stripe')
    }
    else
    {
      innovacion_2016_2020 |> 
        filter(grupo==filtro()) |> 
        datatable(filter = 'top', extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Categoría", "Tipo Producto",
                               "Título", "país", "Año", 
                               "Disponibilidad","Institución financiadora", "Autores"),
                  class = 'cell-border stripe')
    }
  })
  
  output$trabajosd <- DT::renderDataTable({
    
    trabajo_2016_2020 <- trabajo_2016_2020 |> 
      mutate(hasta = str_remove(hasta, ".* "),
             hasta = str_trim(hasta),
             desde = str_remove(desde, "\\d.* "),
             desde = str_trim(desde)) |> 
      filter(desde >= filtro_fecha_min(),
             hasta <= filtro_fecha_max()) 
    if (filtro()==FALSE)
    {
      datatable(trabajo_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría", "Tipo Producto",
                             "Título", "Desde", "Hasta", 
                             "Tipo de Orientación","Estudiante", "Programa académico",
                             "Páginas", "Valoración", "Institución", "Tutor Coautor"),
                class = 'cell-border stripe')
    }
    else
    {
      trabajo_2016_2020 |> 
        filter(grupo==filtro()) |> 
        datatable(filter = 'top', extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Grupo", "Categoría", "Tipo Producto",
                               "Título", "Desde", "Hasta", 
                               "Tipo de Orientación","Estudiante", "Programa académico",
                               "Páginas", "Valoración", "Institución", "Tutor Coautor"),
                  class = 'cell-border stripe')
    }
  })
  
  
  output$graf1 <- renderPlotly({
    
    datos_clasi <- grupos_general |> 
      count(grupo, clasificacion) |> 
      arrange(desc(clasificacion))
    
    if(filtro()==FALSE)
    {
      datos_clasi1 <- grupos_general |> 
        count(clasificacion) |> 
        arrange(desc(clasificacion)) |> 
        plot_ly(x = ~clasificacion, y = ~n, type = 'bar') |> 
          layout(title = 'Clasificación Grupos de investigación',
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))
    }
    else
    {
      datos_clasi |> 
        filter(grupo==filtro()) |> 
        plot_ly(x = ~clasificacion, y = ~n, type = 'bar') |> 
          layout(title = 'Clasificación Grupos de investigación',
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))
    }
    
  })
  
  
  output$graf2 <- renderPlotly ({
     datos_clasificacion <- investigadores_general |> 
       count(grupo ,clasification) |> 
       arrange(desc(clasification)) 
     
     if(filtro()==FALSE)
     {
       plot_ly(datos_clasificacion ,labels= ~clasification, values=~n, type = 'pie') |> 
         layout(title = 'Categorías investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
     else
     {
       datos_clasificacion |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~clasification, values=~n, type = 'pie') |> 
         layout(title = 'Categorías investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
    
  })
  
  output$graf3 <- renderPlotly({
     datos_revista <- articulos_unicos_2016_2020 |> count(grupo ,categoria_revista) |>
       arrange(desc(categoria_revista)) |> 
       mutate(categoria_revista = ifelse(is.na(categoria_revista),"N/A",categoria_revista))
    
     if(filtro()==FALSE)
     {
       plot_ly(datos_revista, labels= ~categoria_revista, values=~n, type = 'pie') |> 
         layout(title = 'Categorías revistas',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
     else
     {
       datos_revista |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~categoria_revista, values=~n, type = 'pie') |> 
          layout(title = 'Categorías revistas',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
    
  })
  
  output$graf3_1 <- renderPlotly({
    datos_revista <- articulos_unicos_2016_2020 |> count(grupo ,SJR_Q) |>
      arrange(desc(SJR_Q)) |> 
      mutate(SJR_Q = ifelse(is.na(SJR_Q),"N/A",SJR_Q))
    
    if(filtro()==FALSE)
    {
      plot_ly(datos_revista, labels= ~SJR_Q, values=~n, type = 'pie') |> 
        layout(title = 'Categorías revistas',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    else
    {
      datos_revista |> 
        filter(grupo==filtro()) |> 
        plot_ly(labels= ~SJR_Q, values=~n, type = 'pie') |> 
        layout(title = 'Categorías revistas',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
  })
  
  output$graf4 <- renderPlotly({
     datos_produccion <- articulos_unicos_2016_2020 |> 
       select(categoria, ano, grupo) |> 
       count(grupo ,ano, sort = FALSE, name = "producciones")
     
     if(filtro()==FALSE)
     {
       datos_produccion1 <- articulos_unicos_2016_2020 |> 
         select(categoria, ano, grupo) |> 
         count(ano, sort = FALSE, name = "producciones") |> 
       plot_ly(x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines') |> 
         layout(title = "Producción articulos",
                xaxis = list(title = "Año"),
                yaxis = list(title = "Producción"))
     }
     else
     {
       datos_produccion |> 
         filter(grupo==filtro()) |> 
         plot_ly(x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines') |> 
          layout(title = "Producción articulos",
                xaxis = list(title = "Año"),
                yaxis = list(title = "Producción"))
     }
    
  })
  
  output$graf5 <- renderPlotly({
     datos_formacion <- investigadores_general |> count(grupo, posgrade) |> 
       arrange(desc(posgrade)) 
     
     if(filtro()==FALSE)
     {
       plot_ly(datos_formacion, labels= ~posgrade, values=~n, type = 'pie') |> 
         layout(title = 'Formación investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
     else
     {
       datos_formacion |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~posgrade, values=~n, type = 'pie') |> 
         layout(title = 'Formación investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     }
    
  })
  
} 

shinyApp(ui = ui , server = server)