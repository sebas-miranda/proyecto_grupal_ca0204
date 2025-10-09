library(readxl)
library(tidyverse)
library(here)

lista.datos <- list()

for(anio in 19:25){
  
  extension <- ifelse(anio %in% c(19:23), ".xlsx", ".xls")
  nombre.archivo <- paste0("Estadísticas Policiales 20", anio, extension)
  
  ruta <- here("data_raw", "estadisticaspoliciales", nombre.archivo)
  
  if (!file.exists(ruta)) {
    message("Falta", ruta, ". Saltando este año.")
    next
  }
  
  temp <- read_excel(ruta)
  names(temp) <- tolower(names(temp))
  temp$fecha <- as.Date(temp$fecha, format = "%Y-%m-%d")
  
  if ("genero" %in% names(temp) && !"sexo" %in% names(temp)) {
    temp <- rename(temp, sexo = genero)
  }
  
  lista.datos[[length(lista.datos) + 1]] <- temp
}

datos.2019.2025 <- bind_rows(lista.datos)

#Ordenamos los sucesos de manera cronológica
datos.2019.2025 <- datos.2019.2025 %>% 
  arrange(fecha)

ruta.salida <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")

write_csv(datos.2019.2025, ruta.salida)