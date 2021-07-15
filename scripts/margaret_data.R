library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)

source(here("scripts",
            "data_grupos.R"))
source(here("scripts",
            "data_getting.R"))
source(here("scripts",
            "data_cleaning.R"))
source(here("scripts",
            "data_tidying.R"))

grupos <- read_csv("https://docs.google.com/spreadsheets/d/1gBaXHFp1NTUTeXodb4JyHqY-P-AWV5yN5-p4L1O09gk/export?format=csv&gid=0")
researchers <- read_csv("https://docs.google.com/spreadsheets/d/1gBaXHFp1NTUTeXodb4JyHqY-P-AWV5yN5-p4L1O09gk/export?format=csv&gid=347194763")

grupo_df <- data_getting_ucla(grupos)
produccion_grupos <- data_cleaning_ucla(grupo_df)

source(here("scripts",
            "report.R"))

articulos_unicos <- data_tidying_ucla(produccion_grupos)

produccion_actualizada <- produccion_grupos
produccion_actualizada[[2]][["articulos"]]<- articulos_unicos
produccion_actualizada[[2]][["Eliminados_por_grupo"]] <- df_eliminados_total
produccion_actualizada[[2]][["Similares_entre_grupo"]] <- df_similares_total_grupos

export_csv(produccion_actualizada)

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
