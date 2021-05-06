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


# This code save produccion_grupos in an excel file

wb <- createWorkbook()
lapply(seq_along(produccion_grupos), 
       function(i){
         addWorksheet(wb=wb, 
                      sheetName = names(produccion_grupos[i]))
         writeData(wb, 
                   sheet = i, 
                   produccion_grupos[[i]])
       })

#Save Workbook

saveWorkbook(wb, 
             "output/grupos_produccion.xlsx", 
             overwrite = TRUE)
