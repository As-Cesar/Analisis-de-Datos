rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

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
# CREACIÓN DE VARIABLES PARA EL ANÁLISIS DE CORRELACIÓN
# =========================================================

# Se construyen nuevas variables debido a que las variables
# originales del dataset no presentan correlaciones fuertes
# o moderadas suficientes para el análisis estadístico.

# ---------------------------------------------------------
# UNIÓN DE BASES DE DATOS
# ---------------------------------------------------------
# Se une la base de ocupados con variables de características
# generales usando DIRECTORIO y ORDEN como identificadores.

base_final <- ocupados_mayo %>%
  left_join(
    caract_gral_mayo %>%
      select(
        DIRECTORIO,
        ORDEN,
        P6040,   # Edad
        P3042,   # Nivel educativo
        P3038,   # Alfabetización / asistencia
        P3039    # Asistencia educativa
      ),
    by = c("DIRECTORIO", "ORDEN")
  )

# ---------------------------------------------------------
# CREACIÓN VARIABLE EDUCACIÓN
# ---------------------------------------------------------
# Se transforma el nivel educativo en una escala ordinal:
#
# 1 = nivel educativo bajo
# 2 = nivel educativo medio
# 3 = nivel educativo alto
#
# Esto permite tratar la educación como una variable numérica
# ordinal para el análisis de correlación.

base_final <- base_final %>%
  mutate(
    educacion = case_when(
      P3042 <= 5 ~ 1,
      P3042 <= 8 ~ 2,
      P3042 >= 9 ~ 3,
      TRUE ~ NA_real_
    )
  )

# ---------------------------------------------------------
# ESTANDARIZACIÓN DE VARIABLES
# ---------------------------------------------------------
# Se estandarizan las variables para eliminar diferencias
# de escala entre ingresos, horas trabajadas y educación.
#
# La función scale() transforma las variables para que
# tengan media 0 y desviación estándar 1.

base_final <- base_final %>%
  mutate(
    ingreso_std = scale(INGLABO)[,1], # Ingreso laboral estandarizado
    horas_std   = scale(P6800)[,1],   # Horas trabajadas estandarizadas
    educ_std    = scale(educacion)[,1]# Educación estandarizada
  )

# ---------------------------------------------------------
# ÍNDICE DE CAPITAL HUMANO
# ---------------------------------------------------------
# Este índice representa el nivel de capital humano de cada
# individuo combinando:
#
# - Nivel educativo
# - Ingreso laboral
# - Horas trabajadas
#
# Se calcula como el promedio de las variables estandarizadas.

base_final <- base_final %>%
  mutate(
    indice_capital_humano =
      (
        ingreso_std +
          horas_std +
          educ_std
      ) / 3
  )

# ---------------------------------------------------------
# VARIABLES DE FORMALIDAD LABORAL
# ---------------------------------------------------------
# Se crean variables binarias:
#
# 1 = condición favorable
# 0 = condición no favorable
#
# Estas variables representan condiciones asociadas al empleo formal.

base_final <- base_final %>%
  mutate(
    salud    = ifelse(P7090 == 1, 1, 0), # Afiliación a salud
    pension  = ifelse(P7020 == 1, 1, 0), # Cotización a pensión
    contrato = ifelse(P6430 == 1, 1, 0)  # Contrato laboral formal
  )

# ---------------------------------------------------------
# ÍNDICE DE FORMALIDAD
# ---------------------------------------------------------
# Este índice mide el nivel de formalidad laboral sumando:
#
# - Afiliación a salud
# - Cotización a pensión
# - Contrato formal
#
# El índice toma valores entre 0 y 3.

base_final <- base_final %>%
  mutate(
    indice_formalidad =
      salud +
      pension +
      contrato
  )

# ---------------------------------------------------------
# ÍNDICE DE ESTABILIDAD ECONÓMICA
# ---------------------------------------------------------
# Este índice representa la estabilidad económica combinando:
#
# - Ingreso laboral
# - Nivel de formalidad
# - Horas trabajadas
#
# Se utilizan variables estandarizadas para evitar problemas
# de diferencias de escala.

base_final <- base_final %>%
  mutate(
    indice_estabilidad =
      ingreso_std +
      scale(indice_formalidad)[,1] +
      horas_std
  )

# ---------------------------------------------------------
# BASE FINAL PARA CORRELACIÓN
# ---------------------------------------------------------
# Se seleccionan únicamente las variables construidas y
# se eliminan los valores faltantes.

base_cor <- base_final %>%
  select(
    indice_capital_humano,
    indice_formalidad,
    indice_estabilidad
  ) %>%
  na.omit()

# ---------------------------------------------------------
# MATRIZ DE CORRELACIÓN
# ---------------------------------------------------------
# Se calcula la correlación entre las variables creadas.

cor(base_cor)
