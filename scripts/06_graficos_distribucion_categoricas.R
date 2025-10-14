#Llamamos las librerias necesarias para proceder con los graficos:
library(dplyr)
library(ggplot2)
library(readr)
library(here)
library(lubridate)

ruta.entrada <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")
datos <- read_csv(ruta.entrada)

#Ahora bien tenemos cargada la base de datos del proyecto, primero verificamos los tipos de variables que posee tal que:
str(datos)

#Note que de hecho la totalidad de las variables utilizadas son categoricas por lo que se procede a graficar

grafico_fecha_frecuencia <- datos %>% 
  mutate(anio = year(fecha)) %>%
  group_by(anio) %>% 
  summarise(frecuencia = n()) %>% 
  mutate(frecuencia_miles = frecuencia / 1000) %>% 
  ggplot(aes(x = factor(anio),y = frecuencia_miles,  fill = frecuencia_miles)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Año",
    y = "Frecuencia de delitos en miles",
    title = "Cantidad de delitos anuales en miles", 
    fill = "Delitos en miles"
  )
ggsave(
  filename = here("info", "graphics", "grafico_fecha_frecuencia.png"),
  plot = grafico_fecha_frecuencia, 
  width = 15, height = 10, dpi = 200
)