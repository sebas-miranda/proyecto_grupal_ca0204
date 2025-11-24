library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(here)

##Desigualdad de Genero
salida <- here("data", "processed")

hojas <- c("Al menos secundaria Mujeres",
           "Al menos secundaria Hombres",
           "IDG-D")

limpiar_canton <- function(x) {
  x %>%
    str_replace("^[0-9]+:\\s*", "") %>%
    str_to_upper() %>%
    iconv(to = "ASCII//TRANSLIT")
}

for (h in hojas) {
  
  df_1 <- read_excel(here("data_raw", "indices", "Indice de Desigualdad de Género.xlsx"), sheet = h) %>%
    rename(Canton = 1) %>%
    mutate(Canton = limpiar_canton(Canton)) %>%
    select(Canton, `2019`, `2020`, `2021`, `2022`, `2023`)
  nombre_csv <- paste0(str_replace_all(h, " ", "_"), ".csv")
  ruta_csv <- file.path(salida, nombre_csv)
  
  write_csv(df_1, ruta_csv)
}

#IDH

ruta_excel_2 <- here("data_raw", "indices", "Indice de Desarrollo_ Humano.xls")

df_2<- read_excel(ruta_excel_2, sheet = "IDH") %>%
  rename(Canton = 1) %>%
  mutate(Canton = limpiar_canton(Canton)) %>%
  select(Canton, `2019`, `2020`, `2021`, `2022`, `2023`)

ruta_csv <- file.path(salida, "IDH.csv")
write_csv(df_2, ruta_csv)

#IDH Desigualdad


ruta_excel_3 <- here("data_raw", "indices", "Indice_Desarrollo_Humano_Ajustado_Desigualdad.xlsx")

df_3 <- read_excel(ruta_excel_3, sheet = "IDH-D") %>%
  rename(Canton = 1) %>%
  mutate(Canton = limpiar_canton(Canton)) %>%
  select(Canton, `2019`, `2020`, `2021`, `2022`, `2023`)

ruta_csv <- file.path(salida, "IDH-D.csv")
write_csv(df_3, ruta_csv)

#Poblacion cantonal

ruta_excel_4 <- here("data_raw", "indices", "Poblacion_Canton.xlsx")

df_4 <- read_excel(ruta_excel_4) %>%
  mutate(Canton = limpiar_canton(Cantón)) %>%
  select(Canton, Poblacion_Total = `Población Total`)

# Guardar CSV
ruta_csv <- file.path(salida, "POBLACION_TOTAL.csv")
write_csv(df_4,ruta_csv)


