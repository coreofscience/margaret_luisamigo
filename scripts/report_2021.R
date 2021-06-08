library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(readxl)

Reporte_proyectos_2021 <- read_excel('C:/Users/ASUS/Documents/7timo Semestre/Proyecto_margaret/margaret/scripts/Reporte_proyectos_2021.xls')

p1 <- data.frame(Reporte_proyectos_2021$PROGRAMA_1)
p2 <- data.frame(Reporte_proyectos_2021$PROGRAMA_2)
p3 <- data.frame(Reporte_proyectos_2021$PROGRAMA_3)
p4 <- data.frame(Reporte_proyectos_2021$PROGRAMA_4)
p5 <- data.frame(Reporte_proyectos_2021$PROGRAMA_5)

names(p2)<-names(p1)
names(p3)<-names(p1)
names(p4)<-names(p1)
names(p5)<-names(p1)

programas <- rbind(p1,p2,p3,p4,p5) 
colnames(programas)<-c("Programas")

programas <- programas %>% filter(!is.na(Programas)) %>% 
  count(Programas,sort = TRUE,name = "Cantidad")

f1 <- data.frame(Reporte_proyectos_2021$PROGRAMA_UNIDAD...53)
f2 <- data.frame(Reporte_proyectos_2021$PROGRAMA_UNIDAD...56)
f3 <- data.frame(Reporte_proyectos_2021$PROGRAMA_UNIDAD...59)
f4 <- data.frame(Reporte_proyectos_2021$PROGRAMA_UNIDAD...62)
f5 <- data.frame(Reporte_proyectos_2021$PROGRAMA_UNIDAD...65)

names(f2)<-names(f1)
names(f3)<-names(f1)
names(f4)<-names(f1)
names(f5)<-names(f1)

facultades <- rbind(f1,f2,f3,f4,f5) 
colnames(facultades)<-c("Facultades")

facultades <- facultades %>% filter(!is.na(Facultades)) %>% 
  count(Facultades,sort = TRUE,name = "Cantidad")

r1 <- data.frame(Reporte_proyectos_2021$PROGRAMA_REGION...54)
r2 <- data.frame(Reporte_proyectos_2021$PROGRAMA_REGION...57)
r3 <- data.frame(Reporte_proyectos_2021$PROGRAMA_REGION...60)
r4 <- data.frame(Reporte_proyectos_2021$PROGRAMA_REGION...63)
r5 <- data.frame(Reporte_proyectos_2021$PROGRAMA_REGION...66)

names(r2)<-names(r1)
names(r3)<-names(r1)
names(r4)<-names(r1)
names(r5)<-names(r1)

regiones <- rbind(r1,r2,r3,r4,r5) 
colnames(regiones)<-c("Centros_Regionales")

regiones <- regiones %>% filter(!is.na(Centros_Regionales)) %>% 
  count(Centros_Regionales,sort = TRUE,name = "Cantidad")

rm(f1,f2,f3,f4,f5,p1,p2,p3,p4,p5,r1,r2,r3,r4,r5)

convocatorias <- data.frame(Reporte_proyectos_2021$DESCRIPCION_CONVOCATORIA)

colnames(convocatorias)<-c("Convocatorias")

convocatorias <- convocatorias %>% filter(!is.na(Convocatorias)) %>% 
  count(Convocatorias, sort = TRUE, name = "Cantidad")

presupuesto_interno <- Reporte_proyectos_2021 %>% 
  summarise(Total_financiacion_interna=sum(Reporte_proyectos_2021$TOTAL_PRESUPUESTO_INTERNO))

presupuesto_externo <- Reporte_proyectos_2021 %>% 
  summarise(Total_financiacion_externa=sum(Reporte_proyectos_2021$PRESUPUESTO_EXTERNO)) 

total_financiacion <- merge(presupuesto_externo,presupuesto_interno)

datasets <- list("Programas_academicos" = programas, 
                         "Facultades" = facultades,
                         "Centros_regionales" = regiones,
                         "convocatorias" = convocatorias,
                         "financiaciones" = total_financiacion)
write.xlsx(datasets, file = "Reporte_2021.xlsx")
