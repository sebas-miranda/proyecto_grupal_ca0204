#Llamamos las librerias
library(dplyr)
library(readr)
library(here)
library(ggplot2)
#Leemos cada tabla de frecuencia

ruta.1 <- here("data", "processed", "resumen_delito.csv")
resumen_delito <- read_csv(ruta.1)
delitos.frecuentes <- 

ruta.2 <- here("data", "processed", "resumen_subdelito.csv")
resumen_subdelito <- read_csv(ruta.2)

ruta.3 <- here("data", "processed", "resumen_victima.csv")
resumen_victima <- read_csv(ruta.3)

ruta.4 <- here("data", "processed", "resumen_subvictima.csv")
resumen_subvictima <- read_csv(ruta.4)

ruta.5 <- here("data", "processed", "resumen_edad.csv")
resumen_edad <- read_csv(ruta.5)

ruta.6 <- here("data", "processed", "resumen_sexo.csv")
resumen_sexo <- read_csv(ruta.6)

ruta.7 <- here("data", "processed", "resumen_nacionalidad.csv")
resumen_nacionalidad <- read_csv(ruta.7)

ruta.8 <- here("data", "processed", "resumen_provincia.csv")
resumen_provincia <- read_csv(ruta.8)

#Graficamos los datos


grafico_delito <- ggplot(resumen_delito,aes(x =delito, y = frecuencia_de_delitos, fil = delito)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribucion de la variable delito", 
    x = "Delito",
    x = "Frecuencia"
  ) +
  theme_minimal()


#guardamos los graficos

ggsave(
  filename = here("info", "graphics", "grafico_delito.png"),
  plot = grafico_delito, 
  width = 15, height = 10, dpi = 200
)




