library(tidyverse)
library(rvest)
library(here)

source(here("scripts",
            "data_grupos.R"))
source(here("scripts",
            "data_getting.R"))

grupos_df <- data_getting_ucla(grupos)
