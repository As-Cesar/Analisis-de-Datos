# =============================================================================
# TALLER ESTADISTICA 2 - PRUEBAS DE HIPOTESIS (POR DIRECTORIO)
# César Ascencio
# =============================================================================

rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

source("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Taller Hipótesis/pruebas_hipotesis.R")

fuerzat_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Mayo 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Junio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Julio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Mayo 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Junio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Julio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Mayo 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Junio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Julio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Mayo 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Junio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/datos/Julio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

# FUNCION PARA CREAR BASE POR MES (NIVEL DIRECTORIO/HOGAR)
# -----------------------------------------------------------------------------
crear_base_mes <- function(fuerzat_mes, no_ocup_mes, ocupados_mes, caract_gral_mes) {
  
  # Convertir ingresos a numerico
  ocupados_mes$INGLABO <- as.numeric(ocupados_mes$INGLABO)
  
  # Nivel educativo por directorio
  edu_mes <- caract_gral_mes %>%
    mutate(nivel_educacion = case_when(
      P3042 == 1                 ~ "Sin educación",
      P3042 %in% c(2, 3)        ~ "Primaria",
      P3042 %in% c(4, 5, 6, 7)  ~ "Secundaria",
      P3042 == 8                 ~ "Técnico",
      P3042 == 9                 ~ "Tecnólogo",
      P3042 %in% c(10,11,12,13) ~ "Profesional",
      P3042 == 99                ~ NA_character_
    )) %>%
    group_by(DIRECTORIO) %>%
    summarise(
      nivel_educacion = names(which.max(table(nivel_educacion, useNA = "no"))),
      .groups = 'drop'
    )
  
  # Fuerza laboral por directorio
  fuerza_mes <- fuerzat_mes %>%
    group_by(DIRECTORIO) %>%
    summarise(en_fuerza_trabajo = sum(FT == 1, na.rm = TRUE), .groups = 'drop')
  
  # No ocupados por directorio
  no_ocup_mes_agg <- no_ocup_mes %>%
    group_by(DIRECTORIO) %>%
    summarise(n_desocupados = sum(DSI == 1, na.rm = TRUE), .groups = 'drop')
  
  # Ingreso promedio por directorio
  ingreso_mes <- ocupados_mes %>%
    group_by(DIRECTORIO) %>%
    summarise(ingreso_prom = mean(INGLABO, na.rm = TRUE), .groups = 'drop')
  
  base_mes <- fuerza_mes %>%
    left_join(no_ocup_mes_agg, by = "DIRECTORIO") %>%
    left_join(ingreso_mes, by = "DIRECTORIO") %>%
    left_join(edu_mes, by = "DIRECTORIO") %>%
    mutate(
      n_desocupados = replace_na(n_desocupados, 0),
      tasa_desempleo = if_else(en_fuerza_trabajo == 0, 0, (n_desocupados / en_fuerza_trabajo) * 100),
      log_ingreso_prom = log(ingreso_prom),
      es_profesional = if_else(nivel_educacion == "Profesional", 1, 0)
    ) %>%
    filter(!is.na(ingreso_prom), ingreso_prom > 0, en_fuerza_trabajo > 0)
  
  return(base_mes)
}

base_mayo <- crear_base_mes(fuerzat_mayo, no_ocup_mayo, ocupados_mayo, caract_gral_mayo)
base_julio <- crear_base_mes(fuerzat_julio, no_ocup_julio, ocupados_julio, caract_gral_julio)

cat("Base mayo:", nrow(base_mayo), "hogares\n")
cat("Base julio:", nrow(base_julio), "hogares\n\n")

niveles <- c(0.030, 0.070, 0.025, 0.045, 0.001)

# =============================================================================
# EJERCICIO 1: PRUEBA Z PARA UNA MEDIA (VARIANZA CONOCIDA) - UNILATERAL DERECHA
# Variable: log_ingreso_prom (por hogar)
# Pregunta: El ingreso promedio por hogar en julio es MAYOR al de mayo?
# H0: μ_julio <= μ_mayo
# H1: μ_julio > μ_mayo
# =============================================================================

log_ingreso_mayo <- base_mayo$log_ingreso_prom
log_ingreso_mayo <- log_ingreso_mayo[!is.na(log_ingreso_mayo) & is.finite(log_ingreso_mayo)]

log_ingreso_julio <- base_julio$log_ingreso_prom
log_ingreso_julio <- log_ingreso_julio[!is.na(log_ingreso_julio) & is.finite(log_ingreso_julio)]

mu0_real <- mean(log_ingreso_mayo)
sigma_real <- sd(log_ingreso_mayo)

# Guardar parámetros originales
old_par <- par(no.readonly = TRUE)

# Ajustar márgenes (más pequeños)
par(mar = c(4, 4, 2, 2))

resultado_z <- mi_z_test(
  x = log_ingreso_julio,
  mu0 = mu0_real,
  sigma = sigma_real,
  alternativa = "mayor",  # Cambiado a unilateral derecha
  conf.level = 0.95,
  graficar = TRUE
)

cat("\n--- RESULTADOS PRUEBA ---\n")
cat("Media log ingreso MAYO (μ0):", round(mu0_real, 4), "\n")
cat("Media log ingreso JULIO (x̄):", round(mean(log_ingreso_julio), 4), "\n")
cat("Diferencia (x̄ - μ0):", round(mean(log_ingreso_julio) - mu0_real, 4), "\n")
cat("Estadistico Z:", round(resultado_z$estadistico_z, 4), "\n")
cat("Valor p (unilateral derecha):", resultado_z$p_value, "\n")
cat("Decision:", resultado_z$decision, "\n")

cat("\n--- DECISIONES POR NIVEL DE SIGNIFICANCIA ---\n")
for(alpha in niveles) {
  resultado <- mi_z_test(
    x = log_ingreso_julio,
    mu0 = mu0_real,
    sigma = sigma_real,
    alternativa = "mayor",
    conf.level = 1 - alpha,
    graficar = TRUE
  )
  cat("α =", alpha, ":", resultado$decision, "\n")
}

# =============================================================================
# EJERCICIO 2: PRUEBA Z PARA UNA PROPORCION (UNILATERAL DERECHA)
# Variable: es_profesional (hogares con nivel Profesional)
# Pregunta: La proporcion de hogares con nivel profesional en julio es MAYOR a la de mayo?
# H0: p_julio <= p_mayo
# H1: p_julio > p_mayo
# =============================================================================

prof_mayo <- base_mayo$es_profesional
prof_mayo <- prof_mayo[!is.na(prof_mayo)]

prof_julio <- base_julio$es_profesional
prof_julio <- prof_julio[!is.na(prof_julio)]

p_mayo <- mean(prof_mayo)
p_julio <- mean(prof_julio)
n_prop_julio <- length(prof_julio)
diferencia_prop <- p_julio - p_mayo

# Error estandar bajo H0
error_estandar_prop <- sqrt(p_mayo * (1 - p_mayo) / n_prop_julio)

# Guardar parámetros originales
old_par <- par(no.readonly = TRUE)

# Ajustar márgenes (más pequeños)
par(mar = c(4, 4, 2, 2))

resultado_prop <- mi_prop_test_vect(
  x = prof_julio,
  p0 = p_mayo,
  alternativa = "mayor",
  conf.level = 0.95,
  graficar = TRUE
)

cat("\n--- DATOS DESCRIPTIVOS ---\n")
cat("Proporcion profesionales MAYO (p0):", round(p_mayo, 4), "\n")
cat("Proporcion profesionales JULIO (p_hat):", round(p_julio, 4), "\n")
cat("Diferencia (p_julio - p_mayo):", round(diferencia_prop, 4), "\n")
cat("Tamaño muestra JULIO (n):", n_prop_julio, "\n")
cat("Error estandar:", round(error_estandar_prop, 6), "\n")

cat("\n--- RESULTADOS PRUEBA ---\n")
cat("Estadistico Z:", round(resultado_prop$estadistico_z, 4), "\n")
cat("Valor p (unilateral):", resultado_prop$p_value, "\n")
cat("Decision:", resultado_prop$decision, "\n")

cat("\n--- DECISIONES POR NIVEL DE SIGNIFICANCIA ---\n")
for(alpha in niveles) {
  resultado <- mi_prop_test_vect(
    x = prof_julio,
    p0 = p_mayo,
    alternativa = "mayor",
    conf.level = 1 - alpha,
    graficar = TRUE
  )
  cat("α =", alpha, ":", resultado$decision, "\n")
}

# =============================================================================
# EJERCICIO 3: PRUEBA T PARA UNA MEDIA (VARIANZA DESCONOCIDA)
# Variable: tasa_desempleo (por hogar)
# Pregunta: La tasa de desempleo promedio en julio es menor al 10%?
# H0: μ >= 10
# H1: μ < 10
# =============================================================================

cat("\n", rep("=", 80), "\n")
cat("EJERCICIO 3: PRUEBA T PARA UNA MEDIA (VARIANZA DESCONOCIDA)\n")
cat(rep("=", 80), "\n")

tasa_desempleo_julio <- base_julio$tasa_desempleo
tasa_desempleo_julio <- tasa_desempleo_julio[!is.na(tasa_desempleo_julio) & is.finite(tasa_desempleo_julio)]

mu0_t <- 10
n_t <- length(tasa_desempleo_julio)
media_t <- mean(tasa_desempleo_julio)
s_t <- sd(tasa_desempleo_julio)
error_estandar_t <- s_t / sqrt(n_t)
t_calc <- (media_t - mu0_t) / error_estandar_t
gl_t <- n_t - 1
p_value_t <- pt(t_calc, df = gl_t, lower.tail = TRUE)

# Guardar parámetros originales
old_par <- par(no.readonly = TRUE)

# Ajustar márgenes (más pequeños)
par(mar = c(4, 4, 2, 2))

resultado_t <- mi_t_test(
  x = tasa_desempleo_julio,
  mu0 = mu0_t,
  alternativa = "menor",
  conf.level = 0.95,
  graficar = TRUE
)

cat("\n--- DATOS DESCRIPTIVOS ---\n")
cat("Media muestral:", round(media_t, 4), "%\n")
cat("Valor hipotetico (μ0):", mu0_t, "%\n")
cat("Diferencia (x̄ - μ0):", round(media_t - mu0_t, 4), "%\n")
cat("Desviacion estandar muestral (s):", round(s_t, 4), "%\n")
cat("Tamaño de muestra (n):", n_t, "\n")
cat("Error estandar (s/√n):", round(error_estandar_t, 6), "\n")
cat("Grados de libertad (gl):", gl_t, "\n")

cat("\n--- RESULTADOS PRUEBA ---\n")
cat("Estadistico t:", round(resultado_t$estadistico_t, 4), "\n")
cat("Valor p (unilateral izquierda):", resultado_t$p_value, "\n")
cat("Decision:", resultado_t$decision, "\n")

cat("\n--- DECISIONES POR NIVEL DE SIGNIFICANCIA ---\n")
for(alpha in niveles) {
  resultado <- mi_t_test(
    x = tasa_desempleo_julio,
    mu0 = mu0_t,
    alternativa = "menor",
    conf.level = 1 - alpha,
    graficar = TRUE
  )
  cat("α =", alpha, ":", resultado$decision, "\n")
}

