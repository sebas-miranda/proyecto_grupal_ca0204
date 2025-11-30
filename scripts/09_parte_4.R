library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(here)
library(tidyverse)
library(sf)
library(stringr)

datos <- read_csv(here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv"))

### 4.1

sexo_victimas_anio <- datos %>% 
  filter(sexo %in% c("HOMBRE", "MUJER")) %>% 
  mutate(anio = year(fecha)) %>% 
  group_by(anio, sexo) %>% 
  summarise(conteo = n(), .groups = "drop") %>% 
  pivot_wider(names_from = sexo,
              values_from = conteo,
              values_fill = 0) %>% 
  rename(
    hombres = HOMBRE,
    mujeres = MUJER
  )
long_sexo <- sexo_victimas_anio %>% 
  pivot_longer(cols = c(hombres, mujeres),
               names_to = "sexo",
               values_to = "conteo")
grafico_victimas_año_sexo <- ggplot(long_sexo, aes(x = anio, y = conteo, color = sexo)) +
  geom_line(size = 1.4) +
  geom_point(size = 2) +
  geom_text(
    data = subset(long_sexo, anio == 2019),
    aes(label = conteo),
    nudge_x = 0.30,   # mueve el número a la derecha
    nudge_y = 0,      
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(long_sexo, anio != 2019),
    aes(label = conteo),
    nudge_x = 0.10,  
    nudge_y = 0,
    vjust = -2,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~sexo, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2019:2025) +
  scale_color_manual(values = c("hombres" = "#1f77b4", 
                                "mujeres" = "#e15759")) +
  labs(
    title = "Evolución anual de víctimas por sexo 2019–2025",
    x = "Año",
    y = "Cantidad de víctimas"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )
ggsave(
  filename = here("info", "graphics", "grafico_victimas_año_sexo.pdf"),
  plot = grafico_victimas_año_sexo, 
  width = 15, height =10,dpi=200
)


### 4.2

tabla_edad_sexo<- datos  %>%
  filter(sexo %in% c("HOMBRE", "MUJER")) %>%
  mutate(año = year(fecha)) %>%
  group_by(edad, sexo) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = sexo,
    values_from = conteo,
    values_fill = 0
  ) %>%
  rename(
    hombres = HOMBRE,
    mujeres = MUJER
  )

tabla_edad_sexo <- tabla_edad_sexo %>%
  rowwise() %>%
  mutate(
    total = hombres + mujeres,
    pct_hombres = round(hombres / total * 100, 2),
    pct_mujeres = round(mujeres / total * 100, 2)
  ) %>%
  ungroup()

tabla_edad_sexo <- tabla_edad_sexo %>%
  mutate(
    pct_hombres_col = round(hombres / sum(hombres) * 100, 2),
    pct_mujeres_col = round(mujeres / sum(mujeres) * 100, 2)
  )

tabla_edad_sexo <- as.data.frame(tabla_edad_sexo)

tabla_edad_sexo$total <- tabla_edad_sexo$hombres + tabla_edad_sexo$mujeres
tabla_edad_sexo$pct_hombres <- round(tabla_edad_sexo$hombres / tabla_edad_sexo$total * 100, 2)
tabla_edad_sexo$pct_mujeres <- round(tabla_edad_sexo$mujeres / tabla_edad_sexo$total * 100, 2)

tabla_edad_sexo$pct_hombres_col <- round(tabla_edad_sexo$hombres / sum(tabla_edad_sexo$hombres) * 100, 2)
tabla_edad_sexo$pct_mujeres_col <- round(tabla_edad_sexo$mujeres / sum(tabla_edad_sexo$mujeres) * 100, 2)

#pct_(sexo): Es el porcentaje de ese sexo dentro de ese grupo de edad
#pct_(sexo)_col: Porcentaje de todos los de ese sexo que pertenecen a ese grupo de edad.

# Gráfico: Frecuencia de delitos según edad y sexo
tabla_larga <- tabla_edad_sexo %>%
  pivot_longer(cols = c("hombres", "mujeres"), names_to = "sexo", values_to = "conteo") %>% 
  filter(edad!= "DESCONOCIDO")

grafico_delito_edad_sexo <- ggplot(tabla_larga, aes(x = edad, y = conteo, fill = sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = conteo),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("hombres" = "steelblue", "mujeres" = "pink")) +
  labs(title = "Frecuencia de delitos según edad y sexo",
       x = "Grupo de edad", y = "Número de víctimas") +
  theme_minimal()

ggsave(
  filename = here("info", "graphics", "grafico_delito_edad_sexo.pdf"),
  plot = grafico_delito_edad_sexo, 
  width = 15, height =10,dpi=200
)

#Grafico: Distribución porcentual de sexo por grupo de edad
tabla_edad_sexo2 <- tabla_edad_sexo %>%
  filter(edad != "DESCONOCIDO") %>%
  mutate(
    total = hombres + mujeres,
    pct_hombres = hombres / total,
    pct_mujeres = mujeres / total
  ) %>%
  select(edad, pct_hombres, pct_mujeres)

tabla_pct <- tabla_edad_sexo2 %>%
  pivot_longer(cols = starts_with("pct"),
               names_to = "sexo",
               values_to = "porcentaje") %>%
  mutate(
    sexo = ifelse(sexo == "pct_hombres", "Hombre", "Mujer")
  )

grafico_distribucion_sexo_edad <- ggplot(
  tabla_pct, 
  aes(x = edad, y = porcentaje, fill = sexo)
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            vjust = -0.4,
            size = 5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Hombre" = "steelblue", "Mujer" = "pink")) +
  labs(
    title = "Distribución porcentual de victimas por sexo de acuerdo al grupo de edad",
    x = "Grupo de edad", 
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 15)

ggsave(
  filename = here("info", "graphics", "grafico_distribucion_sexo_edad.pdf"),
  plot = grafico_distribucion_sexo_edad, 
  width = 15, height =10,dpi=200
)

### GRAFICOS DE VICTIMAS POR PROVINCIA

sexo_prov <- datos %>%
  filter(sexo %in% c("HOMBRE", "MUJER"),
         provincia != "DESCONOCIDO") %>%
  mutate(año = year(fecha)) %>%
  group_by(provincia, año, sexo) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = sexo,
    values_from = conteo,
    values_fill = 0
  ) %>%
  rename(
    hombres = HOMBRE,
    mujeres = MUJER
  )
long_prov <- sexo_prov %>%
  pivot_longer(
    cols = c(hombres, mujeres),
    names_to = "sexo",
    values_to = "conteo"
  )

graficar_provincia <- function(data, prov){
  
  datos <- data %>% filter(provincia == prov)
  
  ggplot(datos, aes(x = año, y = conteo, color = sexo, group = sexo)) +
    geom_line(size = 1.4) +
    geom_point(size = 2) +
    
    geom_text(
      data = subset(datos, año == 2019),
      aes(label = conteo),
      nudge_x = 0.30,   # a la derecha
      nudge_y = 0,
      size = 4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_text(
      data = subset(datos, año != 2019),
      aes(label = conteo),
      nudge_x = 0.10,   # un poquito a la derecha
      vjust = -2,       # hacia arriba
      size = 4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    facet_wrap(~sexo, ncol = 1, scales = "free_y") +
    
    scale_x_continuous(breaks = 2019:2025) +
    
    scale_color_manual(values = c(
      "hombres" = "#1f77b4",
      "mujeres" = "#e15759"
    )) +
    
    labs(
      title = paste("Evolución anual de víctimas por sexo –", prov),
      x = "Año",
      y = "Cantidad de víctimas"
    ) +
    
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

provincias <- unique(long_prov$provincia)

graficos_prov <- lapply(provincias, function(p){
  g <- graficar_provincia(long_prov, p)
  ggsave(
    filename = here("info", "graphics", paste0("victimas_sexo_", p, ".pdf")),
    plot = g,
    width = 14, height = 10, dpi = 300
  )
})

### 4.3

df1 <- datos  %>%
  filter(sexo %in% c("HOMBRE", "MUJER")) %>%
  mutate(año = year(fecha)) %>%
  group_by(canton, sexo) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = sexo,
    values_from = conteo,
    values_fill = 0
  ) %>%
  rename(
    hombres = HOMBRE,
    mujeres = MUJER
  )

df1 <- df1 %>% 
  filter(canton != "DESCONOCIDO")

cantones <- st_read(here("data_raw", "shapefiles", "unidad_geoestadistica_cantonal_ugec_2024.shp"))
unique(cantones$nomb_ugec)
cantones <- cantones  %>% 
  mutate(nomb_ugec = str_to_upper(nomb_ugec),
         nomb_ugec = str_replace_all(nomb_ugec, c(
           "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N"
         )))

cantones <- cantones %>% 
  left_join(df1, by = c("nomb_ugec"= "canton"))

cantones_poly <- cantones %>% 
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  st_cast("POLYGON", warn = FALSE)

bbox_cont <- st_as_sfc(
  st_bbox(c(xmin = -86, xmax = -82, ymin = 8, ymax = 11.3), 
          crs = st_crs(cantones_poly))
)
cantones_filtrado <- cantones_poly[st_intersects(cantones_poly, bbox_cont, sparse = FALSE), ]


grafico_mujeres <- ggplot(cantones_filtrado) +
  geom_sf(aes(fill = mujeres), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia de delitos: Mujeres por canton (2019-2025)", 
       fill = "Mujeres") +
  theme_minimal()
ggsave(
 filename = here("info", "graphics", "grafico_cantones_mujeres.pdf"),
  plot = grafico_mujeres,
  width = 15, height = 10, dpi = 450
)

grafico_hombres <- ggplot(cantones_filtrado) +
  geom_sf(aes(fill = hombres), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia de delitos: Hombres por canton (2019-2025)", 
       fill = "Hombres") +
  theme_minimal()
grafico_hombres
ggsave(
  filename = here("info", "graphics", "grafico_cantones_hombres.pdf"),
  plot = grafico_hombres,
  width = 15, height = 10, dpi=450
)

### EXTRA: Top 5 cantones con mas victimas de acuerdo al sexo

top5_hombres <- datos %>%
  filter(sexo == "HOMBRE", canton != "DESCONOCIDO") %>%
  group_by(canton) %>%
  summarise(total_hombres = n()) %>%
  arrange(desc(total_hombres)) %>%
  slice(1:5)

grafico_top5_hombres <- ggplot(top5_hombres,
                               aes(x = reorder(canton, total_hombres),
                                   y = total_hombres)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = total_hombres),
            vjust = -0.4,hjust = -0.2 ,size = 4.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Los 5 cantones con más víctimas masculinas (2019-2025)",
    x = "Cantón",
    y = "Número de víctimas"
  ) +
  theme_minimal(base_size = 16)
ggsave(
  filename = here("info", "graphics", "grafico_top_hombres.pdf"),
  plot = grafico_top5_hombres,
  width = 15, height = 10, dpi=450
)

top5_hombres <- datos %>%
  filter(sexo == "HOMBRE", canton != "DESCONOCIDO") %>%
  group_by(canton) %>%
  summarise(total_hombres = n()) %>%
  arrange(desc(total_hombres)) %>%
  slice(1:5)

grafico_top5_hombres <- ggplot(top5_hombres,
                               aes(x = reorder(canton, total_hombres),
                                   y = total_hombres)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = total_hombres),
            vjust = -0.4,hjust = -0.2 ,size = 4.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Los 5 cantones con más víctimas masculinas (2019-2025)",
    x = "Cantón",
    y = "Número de víctimas"
  ) +
  theme_minimal(base_size = 16)
ggsave(
  filename = here("info", "graphics", "grafico_top_hombres.pdf"),
  plot = grafico_top5_hombres,
  width = 15, height = 10, dpi=450
)