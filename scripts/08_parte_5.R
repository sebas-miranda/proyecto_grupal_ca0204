library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(here)
library(tidyverse)

datos <- read_csv(here("data", "processed", "Estadísticas Policiales 2019 a Julio 2025.csv"))

#Frecuencia anual de delitos por sexo en cada canton

frecuencia_anual <- datos  %>%
  filter(sexo %in% c("HOMBRE", "MUJER")) %>%
  mutate(año = year(fecha)) %>%
  group_by(canton, año, sexo) %>%
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

ruta_poblacion <-here("data", "processed", "poblacion_total.csv")
poblacion <- read_csv(ruta_poblacion) %>%
  select(canton, Poblacion = poblacion_total)

tasas_sexo <- frecuencia_anual %>%
  inner_join(poblacion, by = "canton") %>%
  mutate(
    tasa_hombres_100k = (hombres / Poblacion) * 100000,
    tasa_mujeres_100k = (mujeres / Poblacion) * 100000
  ) %>%
  select(canton, año, tasa_hombres_100k, tasa_mujeres_100k) %>%
  arrange(canton, año)
tasas_sexo
write_csv(
  tasas_sexo,
  here("data", "processed", "tasa_delitos_sexo_canton_anio.csv")
)

#Frecuencia anual de delitos por sexo en el país
frecuencia_anual_pais <- datos %>%
  filter(sexo %in% c("HOMBRE", "MUJER")) %>%
  mutate(año = year(fecha)) %>%
  filter(año >= 2019 & año <= 2023) %>%
  group_by(año, sexo) %>%
  summarise(total = n(), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = total, values_fill = 0) %>%
  rename(hombres = HOMBRE, mujeres = MUJER)

poblacion <- read_csv(here("data", "processed", "poblacion_total.csv"))
poblacion_total <- sum(poblacion$poblacion_total, na.rm = TRUE)

tasas_nacionales <- frecuencia_anual_pais %>%
  mutate(
    tasa_final_hombres_100k = (hombres / poblacion_total) * 100000,
    tasa_final_mujeres_100k = (mujeres / poblacion_total) * 100000
  ) %>%
  select(año, tasa_final_hombres_100k, tasa_final_mujeres_100k) %>%
  arrange(año)

idg_d <- read_csv(here("data", "processed", "IDG-D.csv")) %>%  
  pivot_longer(-canton, names_to = "año", values_to = "IDG_D") %>%
  mutate(año = as.integer(año)) %>% 
  group_by(año) %>%
  summarise(IDG_D_promedio = mean(IDG_D, na.rm = TRUE), .groups = "drop")

#IDH
idh <- read_csv(here("data", "processed", "IDH.csv")) %>%
  pivot_longer(-canton, names_to = "año", values_to = "IDH") %>%
  mutate(año = as.integer(año)) %>% 
  group_by(año) %>%
  summarise(IDH_promedio = mean(IDH, na.rm = TRUE), .groups = "drop")

#IDH-D
idh_d <- read_csv(here("data", "processed", "IDH-D.csv")) %>%
  pivot_longer(-canton, names_to = "año", values_to = "IDH_D") %>%
  mutate(año = as.integer(año))  %>% 
  group_by(año) %>%
  summarise(IDH_D_promedio = mean(IDH_D, na.rm = TRUE), .groups = "drop")

#Escolaridad
esc_mujeres <- read_csv(here("data", "processed", "Al_menos_secundaria_Mujeres.csv")) %>%
  pivot_longer(-canton, names_to = "año", values_to = "esc_mujeres") %>%
  mutate(año = as.integer(año)) %>% 
  group_by(año) %>%
  summarise(esc_mujeres = mean(esc_mujeres, na.rm = TRUE), .groups = "drop")

esc_hombres <- read_csv(here("data", "processed", "Al_menos_secundaria_Hombres.csv")) %>%
  pivot_longer(-canton, names_to = "año", values_to = "esc_hombres") %>%
  mutate(año = as.integer(año)) %>%
  group_by(año) %>%
  summarise(esc_hombres = mean(esc_hombres, na.rm = TRUE), .groups = "drop")


#Correlaciones
df_correlacion <- tasas_nacionales %>%
  inner_join(idg_d, by = "año") %>%
  inner_join(idh, by = "año") %>%
  inner_join(idh_d, by = "año") %>%
  inner_join(esc_mujeres, by = "año") %>%
  inner_join(esc_hombres, by = "año")

cor_pearson <- df_correlacion %>%
  select(tasa_final_hombres_100k, tasa_final_mujeres_100k, IDG_D_promedio, IDH_promedio, IDH_D_promedio, esc_mujeres, esc_hombres) %>%
  cor(method = "pearson", use = "pairwise.complete.obs")

#Guardar
write.csv(cor_pearson, here("data", "processed", "cor_pearson.csv"), row.names = FALSE, fileEncodin="UTF-8")
