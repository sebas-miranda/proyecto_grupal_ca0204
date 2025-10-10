#Llamamos las librerias
library(dplyr)
library(readr)
library(here)

#Leemos la base de datos
ruta.entrada <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")
datos <- read_csv(ruta.entrada)

#Analizamos la frecuencia de las variales mas importantes y contabilizables

#Delito
resumen_delito <-  datos %>% 
  count(delito, name = "frecuencia_de_delitos") %>% 
  arrange(desc(frecuencia_de_delitos))

#Subdelito
resumen_subdelito <-  datos %>% 
  count(subdelito, name = "frecuencia_de_subdelitos") %>% 
  arrange(desc(frecuencia_de_subdelitos))

#Victima
resumen_victima <-  datos %>% 
  count(victima, name = "frecuencia_de_victimas") %>% 
  arrange(desc(frecuencia_de_victimas))

#Subvictima
resumen_subvictima <-  datos %>% 
  count(subvictima, name = "frecuencia_de_subvictimas") %>% 
  arrange(desc(frecuencia_de_subvictimas))

#Edad
resumen_edad <-  datos %>% 
  count(edad, name = "frecuencia_rango_de_edades") %>% 
  arrange(desc(frecuencia_rango_de_edades))

#Sexo
resumen_sexo <-  datos %>% 
  count(sexo, name = "frecuencia_de_sexo") %>% 
  arrange(desc(frecuencia_de_sexo))

#Nacionalidad
resumen_nacionalidad <-  datos %>% 
  count(nacionalidad, name = "frecuencia_de_nacionalidades") %>% 
  arrange(desc(frecuencia_de_nacionalidades))

#Provincia
resumen_provincia <-  datos %>% 
  count(provincia, name = "frecuencia_de_provincias") %>% 
  arrange(desc(frecuencia_de_provincias))

#Para efectos de analisis, guardamos en varios csvs los datos:

write_csv(resumen_delito, here("data", "processed", "resumen_delito.csv"))
write_csv(resumen_subdelito, here("data", "processed", "resumen_subdelito.csv"))
write_csv(resumen_victima, here("data", "processed", "resumen_victima.csv"))
write_csv(resumen_subvictima, here("data", "processed", "resumen_subvictima.csv"))
write_csv(resumen_edad, here("data", "processed", "resumen_edad.csv"))
write_csv(resumen_sexo, here("data", "processed", "resumen_sexo.csv"))
write_csv(resumen_nacionalidad, here("data", "processed", "resumen_nacionalidad.csv"))
write_csv(resumen_provincia, here("data", "processed", "resumen_provincia.csv"))
