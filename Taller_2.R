rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

# ── IMPORTAR ──────────────────────────────────────────────────────────────────
fuerzat_mayo  <- read_delim("D:/Git/Mayo/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("D:/Git/Junio/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("D:/Git/Julio/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("D:/Git/Mayo/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("D:/Git/Junio/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("D:/Git/Julio/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("D:/Git/Mayo/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("D:/Git/Junio/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("D:/Git/Julio/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("D:/Git/Mayo/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_junio <- read_delim("D:/Git/Junio/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_julio <- read_delim("D:/Git/Julio/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)



## Prueba Hipotesis para diferencia de proporciones dos muestras
#------------
# Planteamiento
# Se desea analizar si la proporción de personas con nivel de educación alta cambió entre mayo y julio.
#H₀: La proporción de personas con educación alta en mayo es igual a la de julio

# 1) Preparar datos 

# MAYO
caract_gral_mayo <- caract_gral_mayo %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1                 ~ "Sin educación",
    P3042 %in% c(2, 3)        ~ "Primaria",
    P3042 %in% c(4, 5, 6, 7)  ~ "Secundaria",
    P3042 == 8                 ~ "Técnico",
    P3042 == 9                 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    TRUE ~ NA_character_
  ))

# JULIO
caract_gral_julio <- caract_gral_julio %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1                 ~ "Sin educación",
    P3042 %in% c(2, 3)        ~ "Primaria",
    P3042 %in% c(4, 5, 6, 7)  ~ "Secundaria",
    P3042 == 8                 ~ "Técnico",
    P3042 == 9                 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    TRUE ~ NA_character_
  ))

# MAYO
caract_gral_mayo <- caract_gral_mayo %>%
  mutate(edu_alta = if_else(
    nivel_educacion %in% c("Técnico", "Tecnólogo", "Profesional"),
    1, 0
  ))

# JULIO
caract_gral_julio <- caract_gral_julio %>%
  mutate(edu_alta = if_else(
    nivel_educacion %in% c("Técnico", "Tecnólogo", "Profesional"),
    1, 0
  ))
# MAYO
x_mayo <- sum(caract_gral_mayo$edu_alta == 1, na.rm = TRUE)
n_mayo <- sum(!is.na(caract_gral_mayo$edu_alta))

# JULIO
x_julio <- sum(caract_gral_julio$edu_alta == 1, na.rm = TRUE)
n_julio <- sum(!is.na(caract_gral_julio$edu_alta))

# MAYO
edu_mayo <- caract_gral_mayo %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1 ~ "Sin educación",
    P3042 %in% c(2,3) ~ "Primaria",
    P3042 %in% c(4,5,6,7) ~ "Secundaria",
    P3042 == 8 ~ "Técnico",
    P3042 == 9 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    TRUE ~ NA_character_
  )) %>%
  mutate(edu_alta = if_else(
    nivel_educacion %in% c("Técnico","Tecnólogo","Profesional"), 1, 0
  )) %>%
  pull(edu_alta) %>%
  na.omit()

# JULIO
edu_julio <- caract_gral_julio %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1 ~ "Sin educación",
    P3042 %in% c(2,3) ~ "Primaria",
    P3042 %in% c(4,5,6,7) ~ "Secundaria",
    P3042 == 8 ~ "Técnico",
    P3042 == 9 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    TRUE ~ NA_character_
  )) %>%
  mutate(edu_alta = if_else(
    nivel_educacion %in% c("Técnico","Tecnólogo","Profesional"), 1, 0
  )) %>%
  pull(edu_alta) %>%
  na.omit()
#Evaluamos 
resultado <- mi_prop_test_2vectores_binarios(
  edu_mayo,
  edu_julio,
  alphas = c(0.030, 0.070, 0.025, 0.045, 0.001)
)

resultado$decisiones
resultado

p_mayo  <- x_mayo / n_mayo
p_julio <- x_julio / n_julio

p_mayo
p_julio
