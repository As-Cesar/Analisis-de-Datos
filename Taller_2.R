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

resultado

## Prueba Hipotesis para muestras dependientes
#------------
#Planteamiento
#Se desea analizar si la tasa de desempleo cambió entre mayo y julio, 
#considerando cada departamento como una unidad de análisis.
#H₀: La media de la tasa de desempleo es igual en mayo y julio para los mismos departamentos.

# Preparar datos
# No ocupados por departamento
no_ocup_dep_mayo <- no_ocup_mayo %>%
  group_by(DPTO) %>%
  summarise(no_ocupados = n())

# Fuerza de trabajo por departamento
fuerza_dep_mayo <- fuerzat_mayo %>%
  group_by(DPTO) %>%
  summarise(fuerza = n())

# Tasa de desempleo
tasa_mayo <- left_join(no_ocup_dep_mayo, fuerza_dep_mayo, by = "DPTO") %>%
  mutate(tasa_mayo = no_ocupados / fuerza)
# No ocupados
no_ocup_dep_julio <- no_ocup_julio %>%
  group_by(DPTO) %>%
  summarise(no_ocupados = n())

# Fuerza de trabajo
fuerza_dep_julio <- fuerzat_julio %>%
  group_by(DPTO) %>%
  summarise(fuerza = n())

# Tasa
tasa_julio <- left_join(no_ocup_dep_julio, fuerza_dep_julio, by = "DPTO") %>%
  mutate(tasa_julio = no_ocupados / fuerza)

datos_dep <- inner_join(tasa_mayo, tasa_julio, by = "DPTO")

x_mayo <- datos_dep$tasa_mayo
x_julio <- datos_dep$tasa_julio

length(x_mayo)
length(x_julio)
#Aplicar prueba
resultado <- mi_t_test_2muestras(
  x_mayo,
  x_julio,
  tipo = "dependientes",
  alternativa = "bilateral"
)
resultado
## Prueba de bondad de ajuste
#------------
# Planteamiento
# Se desea analizar si la distribución de los niveles educativos
# de las personas encuestadas sigue una distribución uniforme.
# H0: Los niveles de educación se distribuyen uniformemente
# (todas las categorías tienen la misma proporción)

# H1: Los niveles de educación NO se distribuyen uniformemente


# 1) Preparar datos

nivel_mayo <- caract_gral_mayo %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1 ~ "Sin educación",
    P3042 %in% c(2,3) ~ "Primaria",
    P3042 %in% c(4,5,6,7) ~ "Secundaria",
    P3042 == 8 ~ "Técnico",
    P3042 == 9 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    TRUE ~ NA_character_
  )) %>%
  pull(nivel_educacion) %>%
  na.omit()
resultado_chi <- mi_chi2_test(
  x = nivel_mayo,
  tipo = "bondad"
)
resultado_chi
#------------
# Prueba de independencia (Chi-cuadrado)
#------------

# Planteamiento:
# Se desea analizar si existe relación entre el nivel educativo 
# y la condición de ocupación (ocupado o desocupado) en la población.

# Hipótesis nula (H0):
# El nivel educativo y la condición de ocupación son independientes,
# es decir, la distribución de ocupados y desocupados es la misma
# para todos los niveles educativos.

# Hipótesis alternativa (H1):
# El nivel educativo y la condición de ocupación NO son independientes,
# es decir, existe una relación entre ambas variables.


# 1) Preparar datos

datos_indep <- bind_rows(
  ocupados_mayo %>%
    select(DIRECTORIO, SECUENCIA_P, ORDEN) %>%
    mutate(condicion_ocupacion = "Ocupado"),
  
  no_ocup_mayo %>%
    filter(DSI == 1) %>%
    select(DIRECTORIO, SECUENCIA_P, ORDEN) %>%
    mutate(condicion_ocupacion = "Desocupado")
) %>%
  inner_join(
    caract_gral_mayo %>%
      select(DIRECTORIO, SECUENCIA_P, ORDEN, nivel_educacion),
    by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  ) %>%
  na.omit()
# 2) aplicar prueba
resultado_indep <- mi_chi2_test(
  x = datos_indep %>% select(nivel_educacion, condicion_ocupacion),
  tipo = "independencia"
)

resultado_indep

#------------
# Prueba de signos
#------------

# Planteamiento:
# Se desea analizar si la tasa de desempleo cambió entre mayo y julio,
# considerando cada departamento como unidad de análisis.

# Hipótesis nula (H0):
# La mediana de las diferencias entre la tasa de desempleo de mayo y julio es igual a 0,
# es decir, no hay cambio en la tasa de desempleo.

# Hipótesis alternativa (H1):
# La mediana de las diferencias entre la tasa de desempleo de mayo y julio es diferente de 0,
# es decir, sí hay cambio en la tasa de desempleo.

# 1) Preparar datos
# MAYO
no_ocup_dep_mayo <- no_ocup_mayo %>%
  group_by(DPTO) %>%
  summarise(no_ocupados = sum(DSI == 1, na.rm = TRUE))

fuerza_dep_mayo <- fuerzat_mayo %>%
  group_by(DPTO) %>%
  summarise(fuerza = n())

tasa_mayo <- left_join(no_ocup_dep_mayo, fuerza_dep_mayo, by = "DPTO") %>%
  mutate(tasa_mayo = no_ocupados / fuerza)

# JULIO
no_ocup_dep_julio <- no_ocup_julio %>%
  group_by(DPTO) %>%
  summarise(no_ocupados = sum(DSI == 1, na.rm = TRUE))

fuerza_dep_julio <- fuerzat_julio %>%
  group_by(DPTO) %>%
  summarise(fuerza = n())

tasa_julio <- left_join(no_ocup_dep_julio, fuerza_dep_julio, by = "DPTO") %>%
  mutate(tasa_julio = no_ocupados / fuerza)

datos_signos <- inner_join(tasa_mayo, tasa_julio, by = "DPTO")

d <- datos_signos$tasa_mayo - datos_signos$tasa_julio

# eliminar empates (d = 0)
d <- d[d != 0]

resultado_signos <- mi_prueba_de_signos(
  x_mayo,
  x_julio,
  usar_rankings = TRUE
)
resultado_signos
