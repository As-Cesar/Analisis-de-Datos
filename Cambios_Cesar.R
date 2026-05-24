library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
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

base_data <- ocupados_mayo %>%
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


base_data <- base_data %>%
  mutate(
    educacion_anios = case_when(
      P3042 <= 5 ~ 5,
      P3042 <= 8 ~ 11,
      P3042 >= 9 ~ 16,
      TRUE ~ NA_real_
    )
  )

base_data <- base_data %>%
  mutate(
    experiencia =
      P6040 - educacion_anios - 6
  )

base_data <- base_data %>%
  mutate(
    experiencia2 = experiencia^2
  )

base_data <- base_data %>%
  mutate(
    ingreso_log = log1p(INGLABO)
  )

m_mincer <- lm(
  ingreso_log ~
    educacion_anios +
    experiencia +
    experiencia2,
  data = base_data
)

summary(m_mincer)




