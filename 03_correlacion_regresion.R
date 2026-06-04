library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(tseries)
library(MASS)
library(labstatR)
library(GGally)
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

Datos_hogar_mayo <- read_delim("/home/arley/Estadistica_2/Datos/Mayo_2025/CSV/Datos del hogar y la vivienda.CSV",
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
# =========================================================

base_data <- ocupados_mayo %>%
  
  left_join(
    caract_gral_mayo %>%
      select(
        DIRECTORIO,
        ORDEN,
        P6040,
        P3042,
        P3271
      ),
    by = c("DIRECTORIO", "ORDEN")
  ) %>%
  
  left_join(
    Datos_hogar_mayo %>%
      select(
        DIRECTORIO,
        P4030S1A1
      ),
    by = "DIRECTORIO"
  )

base_data <- base_data %>%
  filter(
    P4030S1A1 %in% 1:6
  )

base_data <- base_data %>%
  mutate(
    educacion_anios = case_when(
      P3042 == 1  ~ 0,   # Ninguno
      P3042 == 2  ~ 1,   # Preescolar
      P3042 == 3  ~ 5,   # Básica primaria
      P3042 == 4  ~ 9,   # Básica secundaria
      P3042 == 5  ~ 11,  # Media académica
      P3042 == 6  ~ 11,  # Media técnica
      P3042 == 7  ~ 13,  # Normalista
      P3042 == 8  ~ 13,  # Técnica profesional
      P3042 == 9  ~ 15,  # Tecnológica
      P3042 == 10 ~ 16,  # Universitaria
      P3042 == 11 ~ 17,  # Especialización
      P3042 == 12 ~ 18,  # Maestría
      P3042 == 13 ~ 22,  # Doctorado
      TRUE ~ NA_real_
    )
  )
base_data <- base_data %>%
  filter(INGLABO > 0)

base_data <- base_data %>%
  mutate(
    experiencia = pmax(
      P6040 - educacion_anios - 6,
      0
    )
  )

base_data <- base_data %>%
  mutate(
    experiencia2 = experiencia^2
  )

base_data <- base_data %>%
  mutate(
    ingreso_log = log1p(INGLABO)
  )


#INFORMAL Y FORMAL
base_data <- base_data %>%
  mutate(
    informalidad = ifelse(P6440 == 1, 0, 1)
  )

m_final <- lm(
  ingreso_log ~
    educacion_anios +
    experiencia +
    experiencia2 +
    factor(P3271) +
    P6800 +
    informalidad+
    factor(P4030S1A1),  
  data = base_data
)

summary(m_final)

base_data %>%
  select(
    P6800,
    P3271,
    educacion_anios,
    ingreso_log,
    experiencia,
    experiencia2,
    informalidad,
    P4030S1A1
  ) %>%
  ggpairs()

# Variables y Test


base_data <- base_data %>%
  mutate(
    estrato = case_when(
      P4030S1A1 %in% 1 ~ "Pobreza extrema",
      P4030S1A1 %in% 2 ~ "Bajo",
      P4030S1A1 %in% 3 ~ "Medio-Bajo",
      P4030S1A1 %in% 4 ~ "Medio",
      P4030S1A1 %in% 5 ~ "Medio-Alto",
      P4030S1A1 %in% 6 ~ "Alto",
      TRUE ~ NA_character_
    ),
    estrato = factor(estrato)
  )



m1 <- lm(INGLABO ~ educacion_anios + experiencia + experiencia2 +
           factor(P3271) + P6800 + informalidad,
         data = base_data)


#----------------------
# Homocedasticidad
#----------------------

bptest(m1)

#----------------------
# Normalidad (por la cantidad de datos no es posible usar el test de shapiro)
#----------------------
hist(residuals(m1))
jarque.bera.test(residuals(m1))
#----------------------
# Incorrelacion
#----------------------
dwtest(m1)

#----------------------
# Box-Cox
#----------------------

boxcox(m1,plotit=T)
bc<-boxcox(m1,plotit=F)
lambda<-bc$x[which.max(bc$y)]; lambda


z <- (base_data$INGLABO^lambda - 1) /
  (lambda * mean(base_data$INGLABO, na.rm = TRUE)^(lambda - 1))

# y el nuevo ajuste con dicha variable
fit.bc <- lm(z ~ educacion_anios + experiencia + experiencia2 +
               factor(P3271) + P6800 + informalidad,
             data = base_data)
summary(fit.bc)
hist(residuals(fit.bc))
jarque.bera.test(residuals(fit.bc))

#----------------------
# Ponderacion
#----------------------

var_reg <- base_data %>%
  group_by(P4030S1A1) %>%
  summarise(
    wtd = 1 / var(INGLABO, na.rm = TRUE),
    .groups = "drop"
  )
base_data <- base_data %>%
  left_join(var_reg, by = "P4030S1A1")

m3 <- lm(
  ingreso_log ~ educacion_anios + experiencia + experiencia2 +
    factor(P3271) + P6800 + informalidad,
  data = base_data,
  weights = wtd
)

summary(m3)
jarque.bera.test(residuals(m3))



base_data2 <- base_data %>%
  filter(INGLABO > 0,
         INGLABO <= quantile(INGLABO, 0.99, na.rm = TRUE))
m4 <- lm(
  ingreso_log ~ educacion_anios + experiencia + experiencia2 +
    factor(P3271) + P6800 + informalidad,
  data = base_data2
)

summary(m4)
jarque.bera.test(residuals(m4))

base_data3 <- base_data %>%
  filter(INGLABO != max(INGLABO, na.rm = TRUE))  # o filtro extremo

m5 <- lm(
  ingreso_log ~ educacion_anios + experiencia + experiencia2 +
    factor(P3271) + P6800 + informalidad,
  data = base_data3
)

summary(m5)

jarque.bera.test(residuals(m5))


