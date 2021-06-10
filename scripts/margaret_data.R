library(tidyverse)
library(rvest)
library(here)
library(openxlsx)

source(here("scripts",
            "data_grupos.R"))
source(here("scripts",
            "data_getting.R"))
source(here("scripts",
            "data_cleaning.R"))

grupo_df <- data_getting_ucla(grupos)
produccion_grupos <- data_cleaning_ucla(grupo_df)

source(here("scripts",
            "data_tidying.R"))
source(here("scripts",
            "report.R"))

articulos_unicos <- data_tidying_ucla(produccion_grupos)


produccion_actualizada <- produccion_grupos
produccion_actualizada[[3]]<- articulos_unicos
produccion_actualizada[[42]] <- df_eliminados_total
produccion_actualizada[[43]] <- df_similares_total_grupos
names(produccion_actualizada)[[42]]<- "Eliminados_por_grupo"
names(produccion_actualizada)[[43]]<- "Similares_entre_grupo"

# This code save produccion_grupos in an excel file

wb <- createWorkbook()
lapply(seq_along(produccion_actualizada), 
       function(i){
         addWorksheet(wb=wb, 
                      sheetName = names(produccion_actualizada[i]))
         writeData(wb, 
                   sheet = i, 
                   produccion_actualizada[[i]])
       })

#Save Workbook

saveWorkbook(wb, 
             "output/grupos_produccion.xlsx", 
             overwrite = TRUE)
