#Llamamos las librerias
library(dplyr)

#Leemos la base de datos
ruta.entrada <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")
datos <- read_csv(ruta.entrada)

#Escribimos NO APLICA en las columnas variables  
datos <-  datos %>% 
  mutate(
  victima = if_else(victima != "PERSONA", paste(victima, "[NO APLICA]"), victima ),
  subvictima = if_else(victima != "PERSONA", "NO APLICA", subvictima),
  sexo = if_else(victima != "PERSONA", "NO APLICA", sexo),
  edad = if_else(victima != "PERSONA", "NO APLICA", edad),
  nacionalidad = if_else(victima != "PERSONA", "NO APLICA", nacionalidad)
  )

#Escribimos la ruta a donde aplicaremos estos cambios
ruta.salida <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")
write_csv(datos, ruta.salida)
