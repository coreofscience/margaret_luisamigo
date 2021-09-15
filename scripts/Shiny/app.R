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

#revistas
revistas_actuales <-
    read_csv(here("output", 
                  "current_journals.csv")) 
#fin revistas

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
#fin

#dataframe filtros
#filtro grupo
grupos <- articulos_unicos_2016_2020 |>
  select(grupo) |>
  unique()
#filtro temporal, ya que solo funciona con 2 graficas
data4 <- investigadores_general |> select(grupo , clasification) |> 
  group_by(grupo, clasification) |> 
  summarise(n = n())

data5 <- investigadores_general |> select(grupo , posgrade) |> 
  group_by(grupo, posgrade) |> 
  summarise(n = n()) |> 
  rename(m = n,
         formacion = posgrade)
libre <- tibble(grupo= c("N/A","N/A","N/A","N/A","N/A"),
                formacion = c("N/A","N/A","N/A","N/A","N/A"),
                m = c(0,0,0,0,0))

data <- rbind(data5, libre) |> 
  rename(clasification = 2)

data6 <- merge(data, data4, by = c("grupo", "clasification"), all = TRUE) 

datos_compartidos <- crosstalk::SharedData$new(data6)
#-----------------------------------------------------------------------------------------------------
#Inicio
sidebar <- dashboardSidebar(
  filter_select("clasification", "Grupo", datos_compartidos, ~grupo),
  
  # selectInput("grupos_input","Grupos:", 
  #             c(grupos$grupo))
  sidebarMenu(
    menuItem("General", tabName = "general", icon = icon("atlas")),
    menuItem("Producción cientifica", icon = icon("book"), tabName = "produccion"),
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
        tabItem(tabName = "general",
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
  dashboardHeader(title = "Margaret"),
  sidebar,
  setup
)

server <- function(input, output) {
  
    output$graf1 <- renderPlotly({
      
        data1 <- grupos_general |> 
            count(clasificacion) |> 
            arrange(desc(clasificacion))
        
        fig1 <- plot_ly(data1, x = ~clasificacion, y = ~n, type = 'bar')
        fig1 <- fig1 %>% layout(title = 'Clasificación grupos de investigación')
        
    })
    
    output$graf2 <- renderPlotly ({
        datos2 <- investigadores_general |> 
            count(clasification) |> 
            arrange(desc(clasification)) 
        
        fig2 <- datos_compartidos |> plot_ly(labels= ~clasification, values=~n, type = 'pie')
        fig2 <- fig2 %>% layout(title = 'Clasificación investigadores',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    output$graf3 <- renderPlotly({
        data3 <- articulos_unicos_2016_2020 |> count(categoria_revista) |>
            arrange(desc(categoria_revista)) |> 
            mutate(categoria_revista = ifelse(is.na(categoria_revista),"N/A",categoria_revista))
        
        fig3 <- plot_ly(data3, labels= ~categoria_revista, values=~n, type = 'pie')
        fig3 <- fig3 %>% layout(title = 'Categoría revistas',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    output$graf4 <- renderPlotly({
        data4 <- articulos_unicos_2016_2020 |> 
            select(categoria, ano, grupo) |> 
            count(ano, sort = FALSE, name = "producciones")
        
        fig4 <- plot_ly(data4, x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines')
        fig4 <- fig4 %>% layout(title = "Producción articulos")
        
    })
    
    output$graf5 <- renderPlotly({
        data5 <- investigadores_general |> count(posgrade) |> 
            arrange(desc(posgrade)) |> 
            rename(formacion = 1)
        
        fig5 <- datos_compartidos |>  plot_ly(labels= ~clasification, values=~n, type = 'pie')
        fig5 <- fig5 %>% layout(title = 'Formación investigadores',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    output$ex1 <- DT::renderDataTable({
        grupos_general <- grupos_general |> 
            select(grupo, clasificacion, sum_papers, departamento , url,
                   fecha_creacion,lider, email, area_conocimiento_1) |> 
            mutate(url= str_c("<a href=",
                              url,
                              ">Link</a>")) |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Clasificación", "Cantidad artículos",
                                   "Departamento","GrupLAC", 
                                   "Fecha Creación", "Líder", "Email", 
                                   "Área de Conocimiento"),
                      class = 'cell-border stripe')
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
                   Produccion = total_papers,
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
                   scholar) |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Investigador", "Grupo", "Artículos", "H index",
                                   "Categoría","Formación","Inicio", "CvLAC", "Scholar"),
                      class = 'cell-border stripe')
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
                                ">Enlace</a>")) |>  
            datatable(filter = 'top',options = list(pageLength = 5, scrollX = TRUE),
                      escape = FALSE,
                      class = ('cell-border stripe'),
                      colnames = c("Grupo", "Categoría", "Tipo producto",
                                   "Título", "País revista", "Revista", 
                                   "ISSN","Categoría Publindex", "Categoría Scimago", "Año", "Volumen",
                                   "Fasc","Paginas", "Enlace artículo", "Autores"))
        
    })
    
    output$capitulo <- DT::renderDataTable({
        
        capitulos_2016_2020 <- capitulos_2016_2020 |> 
            filter(ano >= 2016,
                   ano <=2020) |> 
            select(-vol, -tipo_producto) |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Categoría",
                                   "Título capitulo", "País", "Año", 
                                   "Titulo libro","ISBN", "Paginas", "Editorial",
                                   "Autores"),
                      class = 'cell-border stripe')
    })
    
    output$libro <- DT::renderDataTable({
        
        libros_2016_2020 <- libros_2016_2020 |> 
            filter(Ano >= 2016,
                   Ano <=2020) |> 
            select(-Tipo_producto) |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Categoría",
                                   "Título libro", "País", "Año", 
                                   "ISBN","Editorial", "Autores"),
                      class = 'cell-border stripe')
    })
    
    output$software <- DT::renderDataTable({
        
        software_2016_2020 <- software_2016_2020 |> 
            select(-nombre_proyecto, -tipo_producto) |> 
            mutate(sitio_web= str_c("<a href=",
                                    sitio_web,
                                    ">Link</a>")) |>
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Categoría",
                                   "Título", "País", "Año", 
                                   "Disponibilidad","Sitio web", "Nombre comercial", "Institución financiadora", 
                                   "Autores"),
                      class = 'cell-border stripe')
    })
    
    output$innovaciones <- DT::renderDataTable({
        
        innovacion_2016_2020 <- innovacion_2016_2020 |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Categoría", "Tipo Producto",
                                   "Título", "país", "Año", 
                                   "Disponibilidad","Institución financiadora", "Autores"),
                      class = 'cell-border stripe')
    })
    
    output$trabajosd <- DT::renderDataTable({
        
        trabajo_2016_2020 <- trabajo_2016_2020 |> 
            mutate(hasta = str_remove(hasta, ".* "),
                   hasta = str_trim(hasta),
                   desde = str_remove(desde, "\\d.* "),
                   desde = str_trim(desde)) |> 
            filter(desde >= 2016,
                   hasta <=2020) |> 
            datatable(filter = 'top', options = list(pageLength = 15, scrollX = TRUE),
                      escape = FALSE,
                      rownames = FALSE,
                      colnames = c("Grupo", "Categoría", "Tipo Producto",
                                   "Título", "Desde", "Hasta", 
                                   "Tipo de Orientación","Estudiante", "Programa académico",
                                   "Páginas", "Valoración", "Institución", "Tutor Coautor"),
                      class = 'cell-border stripe')
    })
    
} 

shinyApp(ui = ui , server = server)