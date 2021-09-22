library(shiny)
library(shinythemes)
library(datasets)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(readxl)
library(crosstalk)
library(stringi)
library(shinydashboard)

articulos_unicos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  filter(ano >= 2016,
         ano <=2020)

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
                "softwares.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) 

trabajo_2016_2020 <- 
  read_csv(here("output",
                "trabajos_dirigidos.csv")) 

innovacion_2016_2020 <- 
  read_csv(here("output",
                "innovaciones_gestion.csv")) |> 
  filter(ano >= 2016,
         ano <=2020) 
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

butonside <- actionButton("aplicar_input", "Aplicar")

sidebar <- dashboardSidebar(
  filterside,
  butonside,
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
             href = "https://rpubs.com/srobledog/margaret_7"
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
                        tabPanel("Grupos", fluidPage((DT::dataTableOutput('ex1'))
                        ),),
                        
                        tabPanel("Investigadores", fluidPage((DT::dataTableOutput('ex2'))
                        ),),
                        
                        tabPanel("Paises", fluidPage((DT::dataTableOutput('ex3'))
                        ),),
                        
                        tabPanel("Revistas", fluidPage((DT::dataTableOutput('ex4'))
                        )))),
    tabItem(tabName = "produccion",
            tabsetPanel(type = "tabs",
                        tabPanel("Articulos", fluidPage((DT::dataTableOutput('articulo'))
                        )),
                        tabPanel("Capitulos", fluidPage((DT::dataTableOutput('capitulo'))
                        )),
                        tabPanel("Libros", fluidPage((DT::dataTableOutput('libro'))
                        )),
                        tabPanel("Software", fluidPage((DT::dataTableOutput('software'))
                        )),
                        tabPanel("Innovaciones", fluidPage((DT::dataTableOutput('innovaciones'))
                        )),
                        tabPanel("Trabajos dirigidos/Tutorías",
                                 fluidPage((DT::dataTableOutput('trabajosd'))
                                 )))),
    tabItem(tabName = "clasi_grupos",
            fluidPage(plotlyOutput("graf1"))),
    
    tabItem(tabName = "clasi_inves",
            fluidPage(plotlyOutput("graf2"))),
    
    tabItem(tabName = "cate_revista",
            fluidPage(plotlyOutput("graf3"))),
    
    tabItem(tabName = "evolu_articulos",
            fluidPage(plotlyOutput("graf4"))),
    
    tabItem(tabName = "forma_inves",
            fluidPage(plotlyOutput("graf5")))
  )
)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Margaret"),
  sidebar,
  setup,
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))))
)

server <- function(input, output) {
  
  filtro <- eventReactive(input$aplicar_input,
                          input$grupos_input,
                          ignoreNULL = FALSE,ignoreInit = FALSE)
  
  output$ex1 <- DT::renderDataTable({
    grupos_general <- grupos_general |> 
      select(grupo, clasificacion, sum_papers, departamento , url,
             fecha_creacion,lider, email, area_conocimiento_1) |> 
      mutate(url= str_c("<a href=",
                        url,
                        ">Link</a>"))
    if (filtro()==FALSE)
    {
      datatable(grupos_general, filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
             Produccion = count_papers,
             Horas = horas_dedicacion,
             CvLAC = url,
             Grupo = grupo,
             Inicio = inicio_vinculacion,
             Formacion = posgrade,
      ) |> 
      select(Investigador,
             Grupo,
             Produccion,
             h_index,
             clasification,
             Formacion,
             Inicio,
             CvLAC,
             scholar) 
    if (filtro()==FALSE)
    {
      datatable(investigadores_general,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Investigador", "Grupo", "Artículos", "H index",
                             "Categoría","Formación","Inicio", "CvLAC", "Scholar"),
                class = 'cell-border stripe')
    }
    else 
    {
      investigadores_general |> 
        filter(str_detect(grupo , filtro() )) |> 
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Investigador", "Grupo", "Artículos", "H index",
                               "Categoría","Formación","Inicio", "CvLAC", "Scholar"),
                  class = 'cell-border stripe')
    }
  })
  
  output$ex3 <- DT::renderDataTable({
    
    paises_general <- paises_general |>  
      mutate(porcentaje = str_c(porcentaje," %"),
             pais_revista = if_else(is.na(pais_revista), "No registra", pais_revista)) |> 
      datatable(options = list(pageLength = 15),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("País", "Cantidad", "Porcentaje"),
                class = 'cell-border stripe')
    
  })
  
  output$ex4 <- DT::renderDataTable({
    
    revistas_actuales <- revistas_actuales |> 
      mutate(porcentaje = str_c(porcentaje," %")) |>  
      datatable(filter = 'top',options = list(pageLength = 20, scrollX = TRUE), 
                rownames = FALSE,
                colnames = c('Revista', 'ISSN', 'Categoría Publindex',
                             'Categoría Scimago','Cantidad', 'Porcentaje'),
                class = 'cell-border stripe')
  })
  
  output$articulo <- DT::renderDataTable({
    
    articulos_2016_2020 <- articulos_2016_2020 |> 
      filter(ano >= 2016,
             ano <=2020) |> 
      select(-id) |> 
      mutate(DOI = str_extract(DOI, "\\d.*")) |> 
      mutate(DOI =  str_c("<a href=","\"",
                          "https://doi.org/",
                          DOI,
                          "\"",
                          ">Enlace</a>")) 
    if (filtro()==FALSE)
    {
      datatable(articulos_2016_2020 ,filter = 'top',options = list(pageLength = 5, scrollX = TRUE),
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
        datatable(filter = 'top',options = list(pageLength = 5, scrollX = TRUE),
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
      filter(ano >= 2016,
             ano <=2020) |> 
      select(-vol, -tipo_producto)
    if(filtro()==FALSE)
    {
      datatable(capitulos_2016_2020 ,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
      filter(Ano >= 2016,
             Ano <=2020) |> 
      select(-Tipo_producto)  
    if (filtro()==FALSE)
    {
      datatable(libros_2016_2020 ,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
      select(-nombre_proyecto, -tipo_producto) |> 
      mutate(sitio_web= str_c("<a href=",
                              sitio_web,
                              ">Link</a>")) 
    if (filtro()==FALSE)
    {
      datatable(software_2016_2020 ,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
    
    innovacion_2016_2020 <- innovacion_2016_2020  
    if (filtro()==FALSE)
    {
      datatable(innovacion_2016_2020 ,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
      filter(desde >= 2016,
             hasta <=2020) 
    if (filtro()==FALSE)
    {
      datatable(trabajo_2016_2020 ,filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
        datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
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
      plot_ly(datos_clasi, x = ~clasificacion, y = ~n, type = 'bar')
    }
    else
    {
      datos_clasi |> 
        filter(grupo==filtro()) |> 
        plot_ly(x = ~clasificacion, y = ~n, type = 'bar')
    }
    
  })
  
  
  output$graf2 <- renderPlotly ({
     datos_clasificacion <- investigadores_general |> 
       count(grupo ,clasification) |> 
       arrange(desc(clasification)) 
     
     if(filtro()==FALSE)
     {
       plot_ly(datos_clasificacion ,labels= ~clasification, values=~n, type = 'pie')
     }
     else
     {
       datos_clasificacion |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~clasification, values=~n, type = 'pie')
     }
    
  })
  
  output$graf3 <- renderPlotly({
     datos_revista <- articulos_unicos_2016_2020 |> count(grupo ,categoria_revista) |>
       arrange(desc(categoria_revista)) |> 
       mutate(categoria_revista = ifelse(is.na(categoria_revista),"N/A",categoria_revista))
    
     if(filtro()==FALSE)
     {
       plot_ly(datos_revista, labels= ~categoria_revista, values=~n, type = 'pie')
     }
     else
     {
       datos_revista |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~categoria_revista, values=~n, type = 'pie')
     }
    
  })
  
  output$graf4 <- renderPlotly({
     datos_produccion <- articulos_unicos_2016_2020 |> 
       select(categoria, ano, grupo) |> 
       count(grupo ,ano, sort = FALSE, name = "producciones")
     
     if(filtro()==FALSE)
     {
       plot_ly(datos_produccion, x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines')
     }
     else
     {
       datos_produccion |> 
         filter(grupo==filtro()) |> 
         plot_ly(x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines')
     }
    
  })
  
  output$graf5 <- renderPlotly({
     datos_formacion <- investigadores_general |> count(grupo, posgrade) |> 
       arrange(desc(posgrade)) 
     
     if(filtro()==FALSE)
     {
       plot_ly(datos_formacion, labels= ~posgrade, values=~n, type = 'pie')
     }
     else
     {
       datos_formacion |> 
         filter(grupo==filtro()) |> 
         plot_ly(labels= ~posgrade, values=~n, type = 'pie')
     }
    
  })
  
} 

shinyApp(ui = ui , server = server)