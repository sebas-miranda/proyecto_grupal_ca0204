library(dplyr)
library(ggplot2)
library(here)
library(tidyr)

ruta.entrada <- here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv")
datos <- read_csv(ruta.entrada)

#Df enfocaddo en relacionar los delitos por provincia
filtro <- datos %>% 
  filter(!provincia %in% c("DESCONOCIDO", "NO APLICA"),
         delito != "DESCONOCIDO")

delitos_mas_frecuentes <-  filtro %>% 
  group_by(delito) %>% 
  summarise(frecuencia.delito = n(), .groups = "drop") %>% 
  arrange(desc(frecuencia.delito)) %>% 
  slice_head(n = 10) %>% 
  pull(delito)

delito_provincia <- filtro %>% 
  filter(delito %in% delitos_mas_frecuentes) %>% 
  group_by(provincia, delito) %>% 
  summarise(frecuencia = n(), .groups = "drop") %>% 
  mutate(provincia = factor(provincia), delito = factor(delito))

#Relacion entre fechas y provincias
grafico_delito_provincia <- delito_provincia %>% 
  ggplot(aes(x = provincia, y = frecuencia, fill = delito)) +
  geom_bar(stat ="identity") +
  coord_flip() +
  labs(title = "Delitos por provincia en Costa Rica entre 2019 y 2025",
       x = "Provincia",
       y ="Frecuencia")
#Guardamos
ggsave(
  filename = here("info", "graphics", "grafico_delito_provincia.png"),
  plot = grafico_delito_provincia, 
  width = 15, height = 10, dpi = 200
)



#Df enfocado en relacionar el sexo con la edad

resumen_sexo_edad <- datos %>%
  filter(!edad %in% c("DESCONOCIDO", "NO APLICA"),
         !sexo %in% c("DESCONOCIDO", "NO APLICA")) %>% 
  group_by(sexo, edad) %>% 
  summarise(frecuencia = n(), .groups = "drop")

#Grafico
grafico_sexo_edad <- resumen_sexo_edad %>% 
  ggplot(aes(x = edad, y = frecuencia, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("HOMBRE" = "blue4", "MUJER" = "pink3")) +
  labs(title = "Distribucion de victimas de un delito en Costa Rica en funcion del sexo y la edad",
       x = "Rangos de edad",
       y = "Frecuencia") +
  theme_minimal()

# Guardar gráfico
ggsave(
  filename = here("info", "graphics", "grafico_sexo_edad.png"),
  plot = grafico_sexo_edad,
  width = 15,
  height = 10,
  dpi = 200
)
