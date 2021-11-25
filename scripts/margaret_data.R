library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)
library(stringi)


source(here("scripts",
            "data_getting.R"))
source(here("scripts",
            "data_cleaning.R"))
source(here("scripts",
            "data_tidying.R"))
source(here("scripts/data_analysis_descriptive.R"))
source(here("scripts",
            "merge_quality_articles.R"))
source(here("scripts",
            "researcher_information.R"))
source(here("scripts",
            "researcher_info.R"))

eval(parse(here("scripts/functions.R"), encoding = "UTF-8"))
# Data outside

grupos <- read_csv("https://docs.google.com/spreadsheets/d/1MT7BKbO7co8mtkuJWY6vQ1J1Vxnerkr998DmWU9hoPY/export?format=csv&gid=0") |> 
  mutate(grupo = str_to_upper(grupo),
         grupo = stri_trans_general(str = grupo,
                                    id = "Latin-ASCII"))

researcher_data <- read_csv("https://docs.google.com/spreadsheets/d/1MT7BKbO7co8mtkuJWY6vQ1J1Vxnerkr998DmWU9hoPY/export?format=csv&gid=1016145547") |> 
  mutate(APELLIDOS = str_trim(APELLIDOS),
         NOMBRES = str_trim(NOMBRES)) |> 
  unite(researcher,"NOMBRES",c("NOMBRES","APELLIDOS"),sep = " ",remove = TRUE) |>
  mutate(researcher = str_to_upper(researcher),
         researcher = stri_trans_general(str = researcher,
                                         id = "Latin-ASCII"),
         researcher = str_squish(researcher)) |> 
  unique() 
 
researchers <- read_csv("https://docs.google.com/spreadsheets/d/1MT7BKbO7co8mtkuJWY6vQ1J1Vxnerkr998DmWU9hoPY/export?format=csv&gid=688218271") |> 
  select(researcher, id_scholar) |> 
  filter(id_scholar != "NA") |> 
  unique() |> 
  mutate(researcher = str_to_upper(researcher),
         researcher = stri_trans_general(str = researcher,
                                         id = "Latin-ASCII"),
         researcher = str_squish(researcher)) |>  
  mutate(h_index = map(id_scholar, safely(get_profile))) |> 
  unnest_wider(h_index) |> 
  unnest_wider(result) |> 
  select(researcher, id_scholar, h_index) |> 
  mutate(h_index = if_else(is.na(h_index), 0, h_index))

researchers <- researcher_info(researcher_data, researchers)

#researchers <- as_tibble(researchers)

grupo_df <- data_getting_ucla(grupos)
produccion_grupos <- data_cleaning_ucla(grupo_df)

source(here("scripts",
            "report.R"))

articulos_unicos <- data_tidying_ucla(produccion_grupos)

articulos_unicos <- merge_quality_articles_ucla(articulos_unicos) 

produccion_actualizada <- produccion_grupos
produccion_actualizada[[2]][["articulos"]]<- articulos_unicos
produccion_actualizada[[2]][["Eliminados_por_grupo"]] <- df_eliminados_total
produccion_actualizada[[2]][["Similares_entre_grupo"]] <- df_similares_total_grupos

shiny_data <- data_analysis_descriptive_ucla(produccion_actualizada)

shiny_data[[3]] <- researcher_information_ucla(researchers, shiny_data)

shiny_data[[2]] <- merge_information(shiny_data)

export_csv(shiny_data)

# Current Journals categories for flex_dashboard
source(here("scripts",
            "current_categories.R"))


# This code save produccion_grupos in an excel file

wb <- createWorkbook()
lapply(seq_along(produccion_actualizada[[2]]), 
       function(i){
         addWorksheet(wb=wb, 
                      sheetName = names(produccion_actualizada[[2]][i]))
         writeData(wb, 
                   sheet = i, 
                   produccion_actualizada[[2]][[i]])
       })

#Save Workbook

saveWorkbook(wb, 
             here("output","grupos_produccion.xlsx"), 
             overwrite = TRUE)


