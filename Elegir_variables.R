rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── IMPORTAR ──────────────────────────────────────────────────────────────────
fuerzat_mayo  <- read_delim("/home/arley/Estadistica_2/Datos/Mayo_2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("/home/arley/Estadistica_2/Datos/Junio_2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("/home/arley/Estadistica_2/Datos/Julio_2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("/home/arley/Estadistica_2/Datos/Mayo_2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("/home/arley/Estadistica_2/Datos/Junio_2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("/home/arley/Estadistica_2/Datos/Julio_2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("/home/arley/Estadistica_2/Datos/Mayo_2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("/home/arley/Estadistica_2/Datos/Junio_2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("/home/arley/Estadistica_2/Datos/Julio_2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("/home/arley/Estadistica_2/Datos/Mayo_2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_junio <- read_delim("/home/arley/Estadistica_2/Datos/Junio_2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_julio <- read_delim("/home/arley/Estadistica_2/Datos/Julio_2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
# =========================================================
# CREACIÓN DE LA BASE FINAL PARA EL ANÁLISIS
# ========================================================

# ---------------------------------------------------------
# UNIÓN DE BASES
# ---------------------------------------------------------
# Se une la base de ocupados con características generales
# usando DIRECTORIO y ORDEN como identificadores únicos.

base_final <- ocupados_mayo %>%
  left_join(
    caract_gral_mayo %>%
      select(
        DIRECTORIO,
        ORDEN,
        P6040,   # Edad
        P3042    # Nivel educativo
      ),
    by = c("DIRECTORIO", "ORDEN")
  )

# =========================================================
# CREACIÓN DE VARIABLES CONTINUAS
# =========================================================

# ---------------------------------------------------------
# 1. VARIABLE: CAPITAL HUMANO
# ---------------------------------------------------------
# Se transforma el nivel educativo en años aproximados
# de educación para convertirlo en una variable continua.
#
# Interpretación:
# <= 5  -> educación básica
# <= 8  -> educación media
# >= 9  -> educación superior

base_final <- base_final %>%
  mutate(
    educacion_num = case_when(
      P3042 <= 5 ~ 5,
      P3042 <= 8 ~ 10,
      P3042 >= 9 ~ 16,
      TRUE ~ NA_real_
    )
  )

# ---------------------------------------------------------
# Capital humano:
# combina educación y edad productiva.
#
# Ambas variables se estandarizan para evitar diferencias
# de escala.

base_final <- base_final %>%
  mutate(
    capital_humano =
      (
        scale(educacion_num)[,1] +
          scale(P6040)[,1]
      ) / 2
  )

# =========================================================
# VARIABLE: INTENSIDAD LABORAL
# =========================================================
# Representa el nivel de participación e intensidad
# en el mercado laboral.
#
# Se construye utilizando:
# - Horas trabajadas
# - Ingreso laboral
#
# Ambas variables son continuas y se estandarizan
# para eliminar diferencias de escala.

base_final <- base_final %>%
  mutate(
    intensidad_laboral =
      (
        scale(P6800)[,1] +
          scale(log1p(INGLABO))[,1]
      ) / 2
  )

# =========================================================
# 3. VARIABLE: BIENESTAR ECONÓMICO
# =========================================================
# Representa el nivel económico del individuo.
#
# Se construye usando:
# - Ingreso laboral principal
# - Otros ingresos laborales
#
# Variables:
# INGLABO -> ingreso laboral
# P550    -> otros ingresos

base_final <- base_final %>%
  mutate(
    bienestar_economico =
      scale(log1p(INGLABO + P550))[,1]
  )

# =========================================================
# BASE FINAL PARA CORRELACIÓN
# =========================================================
# Se seleccionan únicamente las variables construidas
# y se eliminan valores faltantes.

base_cor <- base_final %>%
  select(
    capital_humano,
    intensidad_laboral,
    bienestar_economico
  ) %>%
  na.omit()

# =========================================================
# MATRIZ DE CORRELACIÓN
# =========================================================

cor(base_cor)

# =========================================================
# PRUEBAS DE CORRELACIÓN
# =========================================================

cor.test(
  base_cor$capital_humano,
  base_cor$intensidad_laboral
)

cor.test(
  base_cor$capital_humano,
  base_cor$bienestar_economico
)

cor.test(
  base_cor$intensidad_laboral,
  base_cor$bienestar_economico
)



base_final %>%
  ggplot(aes(x = intensidad_laboral)) +
  geom_histogram()


base_final %>%
  ggplot(aes(x = bienestar_economico)) +
  geom_histogram()

base_final %>% 
  ggplot(aes(x = capital_humano)) +
  geom_histogram()

base_final %>% 
  ggplot(aes(y = intensidad_laboral, bienestar_economico)) +
  geom_point()

base_final %>% 
  ggplot(aes(y = capital_humano, intensidad_laboral)) +
  geom_point()

base_final %>% 
  ggplot(aes(y = capital_humano, x = bienestar_economico)) +
  geom_point()

base_final <- base_final %>% 
  filter(
    capital_humano != 0,
    bienestar_economico != 0,
    intensidad_laboral != 0,
  )
base_final <- base_final %>% 
  mutate(
    educacion_cat = case_when(
      P3042 <= 5 ~ 'Educación básica',
      P3042 <= 8 ~ 'Educación media',
      P3042 >= 9 ~ 'Educación superior',
      TRUE ~ NA_character_
    )
  )

base_final %>%
  ggplot(aes(y = capital_humano, x = educacion_cat)) +
  geom_boxplot()

base_final %>%
  ggplot(aes(y = bienestar_economico, x = educacion_cat)) +
  geom_boxplot()

base_final %>%
  ggplot(aes(y = intensidad_laboral, x = educacion_cat)) +
  geom_boxplot()
