library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(readxl)

read_journals_2004_2016 <- 
  read_csv(here("data", 
                "Revistas_Indexadas_2004_2016.csv"))

read_journals_2017_2018 <- 
  read_csv(here("data", 
                "Revistas_Indexadas_2017_2018.csv"))

read_journals_2019_2020 <- 
  read_csv(here("data", 
                "Revistas_Indexadas_2019_2020.csv"))

journals_2016 <- read_journals_2004_2016|> 
  select(TITULO, TXT_CLASIFICACION, NRO_ANO, NRO_ISSN) |> 
  rename(nombre_revista = 1,
         clasificacion = 2,
         ano = 3,
         ISSN = 4) |> 
  filter(ano >=2016)

journals_2017_2018 <- read_journals_2017_2018|> 
  select(NME_REVISTA_IN, ID_CLAS_REV, NRO_ANO, ID_REVISTA) |> 
  rename(nombre_revista = 1,
         clasificacion = 2,
         ano = 3,
         ISSN = 4)

journals_2019_2020 <- read_journals_2019_2020 |> 
  select(NME_REVISTA_IN, ID_CLAS_REV, NRO_ANO, ID_REVISTA) |> 
  rename(nombre_revista = 1,
         clasificacion = 2,
         ano = 3,
         ISSN = 4)

journals_2016_2020 <- rbind(journals_2016, 
                            journals_2017_2018, 
                            journals_2019_2020)

rm(read_journals_2004_2016, read_journals_2017_2018, read_journals_2019_2020,
   journals_2016, journals_2017_2018, journals_2019_2020)

#revistas con homologacion internacional

p_h_2016 <- read_excel(here("data",
                            "PUBLINDEX_H_2016.xlsx"))|> 
  select(NOMBRE, CATEOGRIA, VIGENCIA, ISSN) |> 
  filter(!is.na(NOMBRE)) |> 
  mutate(VIGENCIA = "2016") |> 
  rename(revista_h = 1,
         categoria = 2,
         ano =3)

p_h_2017_1 <- read_excel(here("data",
                              "PUBLINDEX_H_2017.xlsx"))|> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  slice(-1)

p_h_2017_2 <- read_excel(here("data",
                              "PUBLINDEX_H_2017.xlsx"),
                         sheet = 2) |> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN)

p_h_2017 <- rbind(p_h_2017_1, p_h_2017_2) 

p_h_2018_1 <- read_excel(here("data",
                              "PUBLINDEX_H_2018.xlsx"))|> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5, 
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  slice(-1) |> 
  mutate(ano = "2018")

p_h_2018_2 <- read_excel(here("data",
                              "PUBLINDEX_H_2018.xlsx"),
                         sheet = 2) |> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  mutate(ano = "2018")

p_h_2018_3 <- read_excel(here("data",
                              "PUBLINDEX_H_2018.xlsx"),
                         sheet = 3) |> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  mutate(ano = "2018")

p_h_2018_4 <- read_excel(here("data",
                              "PUBLINDEX_H_2018.xlsx"),
                         sheet = 4) |> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  mutate(ano = "2018")

p_h_2018 <- rbind(p_h_2018_1, p_h_2018_2, p_h_2018_3, p_h_2018_4)

p_h_2019_2020 <- read_excel(here("data",
                                 "PUBLINDEX_H_2019_2020.xlsx")) |> 
  rename(revista_h = 2,
         categoria = 4,
         ano = 5,
         ISSN = 3) |> 
  filter(!is.na(revista_h)) |> 
  select(revista_h, categoria, ano, ISSN) |> 
  mutate(ano = "2019-2020") |> 
  slice(-1) |> 
  separate_rows(ano, sep = "-")

revistas_internacional <- rbind(p_h_2016, p_h_2017, p_h_2018, p_h_2019_2020)

write_csv(journals_2016_2020, here("output",
                                   "journals_2016_2020.csv"))

write_csv(revistas_internacional, here("output",
                                       "journals_international_2016_2020.csv"))
