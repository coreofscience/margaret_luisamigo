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
library(writexl)

# source(here("Upload",
#             "import_data.R"))
# library(tidyverse)
# library(here)
# library(stringi)
# library(plotly)

# source(here("scripts",
#             "citations.R"))

articulos_unicos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |>
  mutate(SJR_Q = ifelse(SJR_Q == '-', "Sin categoria",
                        SJR_Q))

articulos_2016_2020 <- 
  read_csv(here("output",
                "articulos.csv")) |> 
  mutate(categoria_revista = ifelse(categoria_revista == "Sin categorÃ­a",
                                    "Sin Categoría", ifelse(categoria_revista == "Sin informaciÃ³n", 'Sin Información', categoria_revista)),
         SJR_Q = ifelse(SJR_Q == "Sin categorÃ­a",
                        "Sin Categoría", ifelse(SJR_Q == "Sin informaciÃ³n", 'Sin Información', SJR_Q)))

investigadores_general <- 
  read_csv(here("output",
                "investigadores.csv")) |> 
  filter(!is.na(grupo))

grupos_general <- 
  read_csv(here("output",
                "grupos_general.csv")) 

paises_general <- articulos_unicos_2016_2020 |> 
  count(pais_revista, sort = TRUE)
paises_general$porcentaje <- round(prop.table(paises_general$n),3)*100 

revistas_actuales <-
  read_csv(here("output", 
                "current_journals.csv")) 


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

actividades_evaluador <-
  read_csv(here("output",
                "actividades_evaluador.csv"))

actividades_formacion <-
  read_csv(here("output",
                "actividades_formacion.csv"))

apropiacion_social <-
  read_csv(here("output",
                "apropiacion_social_conocimiento.csv"))

conceptos_tecnicos <-
  read_csv(here("output",
                "conceptos_tecnicos.csv"))

consultorias <-
  read_csv(here("output",
                "consultorias.csv"))

cursos <-
  read_csv(here("output",
                "cursos.csv"))

demas_trabajos <-
  read_csv(here("output",
                "demas_trabajos.csv"))
#investigadores
df_researchers <-
  read.csv(here("output",
                "df_researcher.csv"))

divulgacion_publica_contenidos_transmedia <-
  read_csv(here("output",
                "divulgacion_publica_contenidos_transmedia.csv"))

documentos_trabajo <-
  read_csv(here("output",
                "documentos_trabajo.csv"))

ediciones <- 
  read_csv(here("output",
                "ediciones.csv"))

Eliminados_por_grupo <-
  read_csv(here("output",
                "Eliminados_por_grupo.csv"))

espacios_participacion <-
  read_csv(here("output",
                "espacios_participacion.csv"))

estrategias_comunicacion <- 
  read_csv(here("output",
                "estrategias_comunicacion.csv"))

estrategias_pedagogicas <-
  read_csv(here("output",
                "estrategias_pedagogicas.csv"))

eventos_cientificos <-
  read_csv(here("output",
                "eventos_cientificos.csv"))

generacion_contenido_audio <-
  read_csv(here("output",
                "generacion_contenido_audio.csv"))

generacion_contenido_impreso <-
  read_csv(here("output",
                "generacion_contenido_impreso.csv"))

generacion_contenido_virtual <-
  read_csv(here("output",
                "generacion_contenido_virtual.csv"))

generacion_multimedia <-
  read_csv(here("output",
                "generacion_multimedia.csv"))

informes_investigacion <-
  read_csv(here("output",
                "informes_investigacion.csv"))

informes_tecnicos <-
  read_csv(here("output",
                "informes_tecnicos.csv"))

innovaciones_gestion <-
  read_csv(here("output",
                "innovaciones_gestion.csv"))

innovaciones_procesos <-
  read_csv(here("output",
                "innovaciones_procesos.csv"))

international_journals_2021 <-
  read_csv(here("output",
                "international_journals_2021.csv"))

journals_2016_2020 <-
  read_csv(here("output",
                "journals_2016_2020.csv"))

journals_international_2016_2020 <-
  read_csv(here("output",
                "journals_international_2016_2020.csv"))

jurado <-
  read_csv(here("output",
                "jurado.csv"))

libros_divulgacion <-
  read_csv(here("output",
                "libros_divulgacion.csv"))

libros_formacion <-
  read_csv(here("output",
                "libros_formacion.csv"))

manuales_guias_especializadas <-
  read_csv(here("output",
                "manuales_guias_especializadas.csv"))

notas_cientificas <-
  read_csv(here("output",
                "notas_cientificas.csv"))

nuevos_registros_cientificos <-
  read_csv(here("output",
                "nuevos_registros_cientificos.csv"))

otra_publicacion_divulgativa <- 
  read_csv(here("output",
                "otra_publicacion_divulgativa.csv"))

otros_articulos <-
  read_csv(here("output",
                "otros_articulos.csv"))

otros_libros <-
  read_csv(here("output",
                "otros_libros.csv"))

otros_productos_tencologicos <-
  read_csv(here("output",
                "otros_productos_tencologicos.csv"))

participacion_ciudadana_cti <-
  read_csv(here("output",
                "participacion_ciudadana_cti.csv"))

participacion_comites <-
  read_csv(here("output",
                "participacion_comites.csv"))

produccion_tecnica_tecnologica <-
  read_csv(here("output",
                "produccion_tecnica_tecnologica.csv"))

Producciones_de_contenido_digital <-
  read_csv(here("output",
                "Producciones_de_contenido_digital.csv"))

producciones_digital_audiovisual <-
  read_csv(here("output",
                "Producciones_digital_audiovisual.csv"))

redes_conocimiento <-
  read_csv(here("output",
                "redes_conocimiento.csv"))

reglamentos_tecnicos <-
  read_csv(here("output",
                "reglamentos_tecnicos.csv"))

regulaciones_normas <-
  read_csv(here("output",
                "regulaciones_normas.csv"))

signos_distintivos <-
  read_csv(here("output",
                "signos_distintivos.csv"))

similares_entre_grupo <-
  read_csv(here("output",
                "Similares_entre_grupo.csv"))

traducciones <-
  read_csv(here("output",
                "traducciones.csv"))

## Historico categorias grupos
grupos_historicos <- read_csv("https://docs.google.com/spreadsheets/d/1mAFeuE-Eq6DTSiB6a04uo7jid_LcXt1m/export?format=csv&gid=551509450")

## Historico proyectos
proyectos_historicos_2017 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=267659160")

proyectos_historicos_2018 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=1864593933")

proyectos_historicos_2019 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=298346709")

proyectos_historicos_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=466825271")

proyectos_historicos_2021 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=1317016133")

proyectos_historicos_2022 <- read_csv("https://docs.google.com/spreadsheets/d/1Vq_jnUOiBE-_7AsLEIdZnQyOTOuQqWRf/export?format=csv&gid=1766552935") 

## Jovenes investigadores
jovenes_i <- read_csv("https://docs.google.com/spreadsheets/d/1hplBDfYY_eaJx3yU2b_A62JrcQ__UGcY/export?format=csv&gid=428920444")

jovenes_i <- na.omit(jovenes_i)

## Semilleros
estudiantes_semilleros <- read_csv("https://docs.google.com/spreadsheets/d/1VJB_5oj_YGIyVHe4VObsgPni72EPMVVd/export?format=csv&gid=858320991")

semilleros_historicos_2017 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=771049243")
semilleros_historicos_2018 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=1134238589")
semilleros_historicos_2019 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=1127688177")
semilleros_historicos_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=1152962755")
semilleros_historicos_2021 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=1291737582")
semilleros_historicos_2022 <- read_csv("https://docs.google.com/spreadsheets/d/1lyybfY6EZPjFdw7ZWF8p1L7kaS6dBajQ/export?format=csv&gid=92570379")

semilleros_historicos <- merge(semilleros_historicos_2017, semilleros_historicos_2018, all = TRUE)
semilleros_historicos <- merge(semilleros_historicos, semilleros_historicos_2019, all = TRUE)
semilleros_historicos <- merge(semilleros_historicos, semilleros_historicos_2020, all = TRUE)
semilleros_historicos <- merge(semilleros_historicos, semilleros_historicos_2021, all = TRUE)
semilleros_historicos <- merge(semilleros_historicos, semilleros_historicos_2022, all = TRUE)
rm(semilleros_historicos_2017,semilleros_historicos_2018,semilleros_historicos_2019,semilleros_historicos_2020,
   semilleros_historicos_2021,semilleros_historicos_2022)

## Trabajos de grado
tg_2016 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=1553280182") |>
  mutate('Ano' = 2016)
tg_2017 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=1462223049") |> 
  mutate('Ano' = 2017)
tg_2018 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=2137854493") |> 
  mutate('Ano' = 2018)
tg_2019 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=183846486") |> 
  mutate('Ano' = 2019)
tg_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=627163141") |> 
  mutate('Ano' = 2019)
tg_2021 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=1179613430") |> 
  mutate('Ano' = 2021)
tg_2022 <- read_csv("https://docs.google.com/spreadsheets/d/1dlci5TT6Raqj6IKPMz3CiCu_W5K_y8QJ/export?format=csv&gid=1963658115") |> 
  mutate('Ano' = 2022)

tg <- merge(tg_2016, tg_2017, all = TRUE)
tg <- merge(tg, tg_2018, all = TRUE)
tg <- merge(tg, tg_2019, all = TRUE)
tg <- merge(tg, tg_2020, all = TRUE)
tg <- merge(tg, tg_2021, all = TRUE)
tg <- merge(tg, tg_2022, all = TRUE)
rm(tg_2016, tg_2017, tg_2018, tg_2019, tg_2020, tg_2021, tg_2022)

## Eventos academicos
eva_estudiantes <- read_csv("https://docs.google.com/spreadsheets/d/14EGepMiiewyb2PJ3rvEhu2i1sSi5NIwj/export?format=csv&gid=1384748556")

eva_docentes <- read_csv("https://docs.google.com/spreadsheets/d/14EGepMiiewyb2PJ3rvEhu2i1sSi5NIwj/export?format=csv&gid=1055913532")

### Fin lectura
margaret <- list("grupos_general"=grupos_general,"investigadores"=investigadores_general,"articulos"=articulos_unicos_2016_2020,
                 "actividades_evaluador"=actividades_evaluador,"actividades_formacion"=actividades_formacion,
                 "apropiacion_social_conocimiento"=apropiacion_social,
                 "capitulos"=capitulos_2016_2020,"conceptos_tecnicos"=conceptos_tecnicos,"consultorias"=consultorias,
                 "cursos"=cursos,"demas_trabajos"=demas_trabajos,
                 "divulgacion_publica_contenidos_transmedia"=divulgacion_publica_contenidos_transmedia,
                 "documentos_trabajo"=documentos_trabajo,"ediciones"=ediciones,
                 "espacios_participacion"=espacios_participacion,"estrategias_comunicacion"=estrategias_comunicacion,
                 "estrategias_pedagogicas"=estrategias_pedagogicas,"eventos_cientificos"=eventos_cientificos,
                 "generacion_contenido_audio"=generacion_contenido_audio,
                 "generacion_contenido_impreso"=generacion_contenido_impreso,"generacion_contenido_virtual"=generacion_contenido_virtual,
                 "generacion_multimedia"=generacion_multimedia,"informes_investigacion"=informes_investigacion,"informes_tecnicos"=informes_tecnicos,
                 "innovaciones_gestion"=innovaciones_gestion,"innovaciones_procesos"=innovaciones_procesos,
                 "jurado"=jurado,"libros_divulgaciones"=libros_divulgacion,"libros_formacion"=libros_formacion,"libros"=libros_2016_2020,
                 "manuales_guias_especializadas"=manuales_guias_especializadas,"notas_cientificas"=notas_cientificas,
                 "nuevos_registros_cientificos"=nuevos_registros_cientificos,"otra_publicacion_divulgativa"=otra_publicacion_divulgativa,
                 "otros_articulos"=otros_articulos,"otros_libros"=otros_libros,"otros_productos_tecnologicos"=otros_productos_tencologicos,
                 "participacion_ciudadana_cti"=participacion_ciudadana_cti,"participacion_comites"=participacion_comites,
                 "produccion_tecnica_tecnologia"=produccion_tecnica_tecnologica,"producciones_de_contenido_digital"=Producciones_de_contenido_digital,
                 "producciones_digital_audiovisual"=producciones_digital_audiovisual,"redes_conocimiento"=redes_conocimiento,
                 "reglamentos_tecnicos"=reglamentos_tecnicos,"regulaciones_normas"=regulaciones_normas,"signos_distintivos"=signos_distintivos,
                 "software"=software_2016_2020,"trabajos_dirigidos"=trabajo_2016_2020,"traducciones"=traducciones,
                 "similares_entre_grupo"=similares_entre_grupo,"eliminados_por_grupo"=Eliminados_por_grupo)
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

sliderside <- sliderInput("fechas_input", "Años:", min = 2014, max = 2022, value = c(2016,2022), sep = "")

sidebar <- dashboardSidebar(
  filterside,
  sliderside,
  sidebarMenu(
             menuItem("Datos", tabName = "general_datos", icon = icon("atlas")),
    
    menuItem("Producción cientifica", icon = icon("book"), tabName = ("produccion")),
    
    
    menuItem("Grupos en cifras", icon = icon("bar-chart-o",verify_fa = FALSE),
             menuSubItem("Clasificación grupos", tabName = "clasi_grupos"),
             menuSubItem("Clasificación investigadores", tabName = "clasi_inves"),
             menuSubItem("Categoría revistas", tabName = "cate_revista"),
             menuSubItem("Evolución temporal", tabName = "evolu_articulos"),
             menuSubItem("Formación investigadores", tabName = "forma_inves")
    ),
    menuItem("Procesos Institucionales", icon = icon("folder-plus",verify_fa = FALSE),        
             menuSubItem("Grupos Reconocidos", icon = icon("boxes"), tabName = "grReconocidos"),
             menuItem("Histórico Proyectos", icon = icon("arrow-circle-left"),
                      menuSubItem("2017", icon = icon("book"), tabName = "jistProyectos_2017"),
                      menuSubItem("2018", icon = icon("book"), tabName = "jistProyectos_2018"),
                      menuSubItem("2019", icon = icon("book"), tabName = "jistProyectos_2019"),
                      menuSubItem("2020", icon = icon("book"), tabName = "jistProyectos_2020"),
                      menuSubItem("2021", icon = icon("book"), tabName = "jistProyectos_2021"),
                      menuSubItem("2022", icon = icon("book"), tabName = "jistProyectos_2022")),
             menuItem("Semilleros", icon = icon("microscope"),
             menuSubItem("Histórico Semilleros", icon = icon("history"), tabName = "histSemilleros"),
             menuSubItem("Estudiantes inscritos", icon = icon("users"), tabName = "estudiantesInscritos")),
             menuSubItem("Jovenes Investigadores", icon = icon("search"), tabName = "jovenesInvestigadores"),
             menuSubItem("Trabajos de grado: Auxiliares", icon = icon("graduation-cap"), tabName = "trabjosGrado"),
             menuItem("Eventos Académicos", icon = icon("calendar-alt"),
             menuSubItem("Estudiantes", icon = icon("users"), tabName = "eventos_estudiantes"),
             menuSubItem("Docentes", icon = icon("users"), tabName = "eventos_docentes"))
             
    ),
    #download
    menuItem("Descargar",icon = icon("fas fa-download"), downloadButton("download", "Download full results")),
    span(),
    tags$h5("Última actualización:", align = "center"),
    tags$h6("03 de Octubre 2022", align = "center")
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
    tabItem(tabName = "grReconocidos",
            tabPanel("Grupos Reconocidos"), fluidPage(br(),(DT::dataTableOutput('grupos_hist'))
            )),
    tabItem(tabName = "jistProyectos_2017",
            tabPanel("2017"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2017'))
            )),
    tabItem(tabName = "jistProyectos_2018",
            tabPanel("2018"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2018'))
            )),
    tabItem(tabName = "jistProyectos_2019",
            tabPanel("2019"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2019'))
            )),
    tabItem(tabName = "jistProyectos_2020",
            tabPanel("2020"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2020'))
            )),
    tabItem(tabName = "jistProyectos_2021",
            tabPanel("2021"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2021'))
            )),
    tabItem(tabName = "jistProyectos_2022",
            tabPanel("2022"), fluidPage(br(),(DT::dataTableOutput('hist_proyectos_2022'))
            )),
    tabItem(tabName = "histSemilleros",
            tabPanel("Histórico semilleros"), fluidPage(br(),(DT::dataTableOutput('histSemi'))
            )),
    tabItem(tabName = "estudiantesInscritos",
            tabPanel("Estudiantes inscritos"), fluidPage(br(),(DT::dataTableOutput('estudiantesInscri'))
            )),
    tabItem(tabName = "jovenesInvestigadores",
            tabPanel("Jovenes Investigadores"), fluidPage(br(),(DT::dataTableOutput('jovenesI'))
            )),
    tabItem(tabName = "trabjosGrado",
            tabPanel("Trabajos de grado:Auxiliares"), fluidPage(br(),(DT::dataTableOutput('tgA'))
            )),
    tabItem(tabName = "eventos_estudiantes",
            tabPanel("Estudiantes"), fluidPage(br(),(DT::dataTableOutput('eva_estudiantes'))
            )),
    tabItem(tabName = "eventos_docentes",
            tabPanel("Docentes"), fluidPage(br(),(DT::dataTableOutput('eva_docentes'))
            )),
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
  
  data <- margaret
  
  output$download <- downloadHandler(
    filename = "Margaret.xlsx",
    content = function(file) {
      write_xlsx(data, file)
    }
  )
  
  
  output$ex1 <- DT::renderDataTable(server = FALSE,{
    grupos_general <- grupos_general |> 
      select(grupo, clasificacion, sum_papers, departamento , url.x,
             fecha_creacion,lider, email, area_conocimiento_1) |> 
      mutate(url.x= str_c('<a href="',
                        url.x,
                        '" target="_blank">Link</a>'))
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
  
  output$ex2 <- DT::renderDataTable(server = FALSE,{
    
    investigadores_general <- investigadores_general |>
      mutate(url = str_c('<a href="',
                         url,
                         '" target="_blank">Link</a>'),
             scholar = str_c("<a href=","\"",
                             "https://scholar.google.com/citations?user=",
                             id_scholar,
                             '" target="_blank">Scholar</a>'),
             ORCID = str_c("<a href=","\"",
                           "https://orcid.org/",
                           ORCID,
                           '" target="_blank">ORCID</a>')) |>
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
             ORCID,
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
                             "Formación","Inicio", "CvLAC","ORCID", "Scholar"),
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
                               "Formación","Inicio", "CvLAC","ORCID", "Scholar"),
                  class = 'cell-border stripe')
    }
  })
  
  output$ex3 <- DT::renderDataTable(server = FALSE,{
    
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
  
  output$ex4 <- DT::renderDataTable(server = FALSE,{
    
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
  
  output$grupos_hist <- DT::renderDataTable(server = FALSE,{
    
    datatable(grupos_historicos, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2017 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2017, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2018 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2018, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2019 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2019, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2020 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2020, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2021 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2021, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$hist_proyectos_2022 <- DT::renderDataTable(server = FALSE,{
    
    datatable(proyectos_historicos_2022, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$histSemi <- DT::renderDataTable(server = FALSE,{
    
    semilleros_historicos <- semilleros_historicos |>
      filter(ANO >= filtro_fecha_min(),
             ANO <=filtro_fecha_max()) 
    datatable(semilleros_historicos, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$estudiantesInscri <- DT::renderDataTable(server = FALSE,{
    
    estudiantes_semilleros <- estudiantes_semilleros |>
      filter(ANO >= filtro_fecha_min(),
             ANO <=filtro_fecha_max())
    datatable(estudiantes_semilleros, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$jovenesI <- DT::renderDataTable(server = FALSE,{
    
    jovenes_i <- jovenes_i |> 
      filter(ANO >= filtro_fecha_min(),
             ANO <=filtro_fecha_max())  |> 
      select(-'CEDULA') 
    
    datatable(jovenes_i, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$tgA <- DT::renderDataTable(server = FALSE,{
    
    tg <- tg |>
      filter(Ano >= filtro_fecha_min(),
             Ano <=filtro_fecha_max()) 
    tg <- tg |> select(-'Cedula', -'N', -'Documento Identidad', -'Fecha de Ingreso', -'Modalidad')
    datatable(tg, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$eva_estudiantes <- DT::renderDataTable(server = FALSE,{
    
    eva_estudiantes <- eva_estudiantes |> 
      filter(Ano >= filtro_fecha_min(),
             Ano <=filtro_fecha_max()) 
    datatable(eva_estudiantes, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$eva_docentes <- DT::renderDataTable(server = FALSE,{
    
    eva_docentes <- eva_docentes |> 
      filter(Ano >= filtro_fecha_min(),
             Ano <=filtro_fecha_max()) 
    datatable(eva_docentes, filter = 'top', extensions = c('Scroller','Buttons'),
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
              class = 'cell-border stripe')
  })
  
  output$articulo <- DT::renderDataTable(server = FALSE,{
    
    articulos_2016_2020 <- articulos_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      select(-id) |> 
      mutate(DOI = str_extract(DOI, "\\d.*")) |> 
      mutate(DOI =  str_c("<a href=","\"",
                          "https://doi.org/",
                          DOI,
                          "\"",
                          '" target="_blank">Link</a>')) 
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
                               "ISSN","Categoría Publindex", "Categoría Scimago", "Citaciones", "Año", "Volumen",
                               "Fasc","Paginas", "Enlace artículo", "Autores"))
    }
  })
  
  output$capitulo <- DT::renderDataTable(server = FALSE,{
    
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
  
  output$libro <- DT::renderDataTable(server = FALSE,{
    
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
  
  output$software <- DT::renderDataTable(server = FALSE,{
    
    software_2016_2020 <- software_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      select(-nombre_proyecto, -tipo_producto) |> 
      mutate(sitio_web= str_c("<a href=",
                              sitio_web,
                              '" target="_blank">Link</a>')) 
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
  
  output$innovaciones <- DT::renderDataTable(server = FALSE,{
    
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
  
  output$trabajosd <- DT::renderDataTable(server = FALSE,{
    
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
      count(clasificacion)
    
    datos_clasi$clasificacion = factor(datos_clasi$clasificacion,
                                       levels = c("A1","A","B","C"))
    
    if(filtro()==FALSE)
    {
      datos_clasi |> 
        plot_ly(x = datos_clasi$clasificacion, y = datos_clasi$n, type = 'bar') |> 
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
     datos_revista <- articulos_unicos_2016_2020 |> 
       filter(ano >= filtro_fecha_min(),
              ano <=filtro_fecha_max()) |> 
       count(grupo ,categoria_revista) |>
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
    datos_revista <- articulos_unicos_2016_2020 |> 
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |> 
      count(grupo ,SJR_Q) |>
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
       select(ano, grupo) |> 
       count(grupo ,ano, sort = FALSE, name = "producciones")
     
     if(filtro()==FALSE)
     {
       datos_produccion1 <- articulos_unicos_2016_2020 |> 
         select(ano, grupo) |> 
         count(ano, sort = FALSE, name = "producciones") |> 
       plot_ly(x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines') |> 
         layout(title = "Producción artículos",
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