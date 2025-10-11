#Llamamos las librerias
library(dplyr)
library(readr)
library(here)
library(ggplot2)
library(sf)
library(stringr)

#Leemos cada tabla de frecuencia

ruta.1 <- here("data", "processed", "resumen_delito.csv")
resumen_delito <- read_csv(ruta.1)

ruta.2 <- here("data", "processed", "resumen_victima.csv")
resumen_victima <- read_csv(ruta.2)

ruta.3 <- here("data", "processed", "resumen_edad.csv")
resumen_edad <- read_csv(ruta.3)

ruta.4 <- here("data", "processed", "resumen_sexo.csv")
resumen_sexo <- read_csv(ruta.4)

ruta.5 <- here("data", "processed", "resumen_nacionalidad.csv")
resumen_nacionalidad <- read_csv(ruta.5)

ruta.6 <- here("data", "processed", "resumen_provincia.csv")
resumen_provincia <- read_csv(ruta.6)

ruta.7 <- here("data", "processed", "resumen_canton.csv")
resumen_canton <- read_csv(ruta.7)
##############Graficamos los datos


#Grafico de delitos
grafico_delito <- resumen_delito %>% 
  arrange(desc(frecuencia_de_delitos)) %>% 
  mutate(delito = factor(delito, levels = delito)) %>% 
  ggplot(aes(x =delito, y = frecuencia_de_delitos, fil = delito)) +
  geom_bar(stat = "identity", fill = "skyblue", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "10 delitos ocurridos más frecuentres en Costa Rica entre 2019 y 2025", 
    x = "Delito",
    y = "Frecuencia"
  ) +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_delito.png"),
  plot = grafico_delito, 
  width = 15, height = 10, dpi = 200
)


######Grafico de victima
grafico_victima <- resumen_victima %>% 
  mutate(victima = factor(victima, levels = victima)) %>% 
  ggplot(aes(x = victima, y = frecuencia_de_victimas)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  coord_flip()+
  labs(title = "Victimas más frecuentes de un delito en Costa Rica entre 2019 y 2025") +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_victima.png"),
  plot = grafico_victima, 
  width = 15, height = 10, dpi = 200
)


######Grafico de edad
grafico_edad <- resumen_edad %>% 
  mutate(edad = factor(edad, levels = edad)) %>% 
  ggplot(aes(x = edad, y =frecuencia_rango_de_edades)) +
  geom_bar(stat= "identity", fill = "aquamarine") +
  coord_flip() +
  labs(title = "Rango de edad de la víctima de un delito en Costa Rica entre 2019 y 2025",
       x = "Rangos de edad",
       y = "Frecuencia") +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_edad.png"),
  plot = grafico_edad, 
  width = 15, height = 10, dpi = 200
)


#######Grafico de sexo
grafico_sexo <- resumen_sexo %>% 
  mutate(sexo = factor(sexo, levels = sexo)) %>% 
  ggplot(aes(x = sexo, y = frecuencia_de_sexo)) +
  geom_bar(stat = "identity", fill = "chocolate3" ) +
  coord_flip() +
  labs(title = "Variación del sexo de la víctima de un delito en Costa Rica entre 2019 y 2025",
       x = "Sexo",
       y = "Frecuencia") +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_sexo.png"),
  plot = grafico_sexo, 
  width = 15, height = 10, dpi = 200
)


#########Grafico de nacionalidad

grafico_nacionalidad <- resumen_nacionalidad %>% 
  mutate(nacionalidad = factor(nacionalidad, levels = nacionalidad)) %>% 
  ggplot(aes( x = nacionalidad, y = frecuencia_de_nacionalidades)) +
  geom_bar(stat = "identity", fill = "cornsilk" ) +
  coord_flip() +
  labs(title = "Nacionalidad de la víctima de un delito en Costa Rica entre 2019 y 2025",
       x = "Nacionalidad",
       y = "Frecuencia") +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_nacionalidad.png"),
  plot = grafico_nacionalidad, 
  width = 15, height = 10, dpi = 200
)


#########Grafico de provincia

grafico_provincia <- resumen_provincia %>% 
  mutate(provincia = factor(provincia, levels = provincia)) %>% 
  ggplot(aes( x = provincia, y = frecuencia_de_provincias))+
  geom_bar(stat = "identity", fill = "darkolivegreen4" ) +
  coord_flip() +
  labs(title = "Provincias de Costa Rica donde se reportan los delitos entre 2019 y 2025",
       x = "Provincia",
       y = "Frecuencia") +
  theme_minimal()
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_provincia.png"),
  plot = grafico_provincia, 
  width = 15, height = 10, dpi = 200
)

############Grafico de cantones

#Quitamos la columna de desconocido

resumen_canton <- resumen_canton %>% 
  filter(canton != "DESCONOCIDO")
  
#Leemos el shp que se descargo en la fuente del INEC

cantones <- st_read("data_raw/shapefiles/unidad_geoestadistica_cantonal_ugec_2024.shp")

#Hacemos todo mayuscula y sin tilde

cantones <- cantones  %>% 
  mutate(nomb_ugec = str_to_upper(nomb_ugec),
         nomb_ugec = str_replace_all(nomb_ugec, c(
           "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N"
         )))

#Agregamos las frecuencias a la base de cantones que importamos

cantones <- cantones %>% 
  left_join(resumen_canton, by = c("nomb_ugec"= "canton"))

###Graficamos


grafico_cantones <- ggplot(cantones) +
  geom_sf(aes(fill = frecuencia_de_cantones), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia de reportes de delito por canton en Costa Rica entre 2019 y 2025", fill = "Frecuencia")
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_cantones.png"),
  plot = grafico_cantones, 
  width = 15, height = 10, dpi = 200
)



