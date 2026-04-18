library(readr)
library(dplyr)
library(tidyr)

# ── IMPORTAR ──────────────────────────────────────────────────────────────────
fuerzat_mayo  <- read_delim("Mayo 2025/CSV/Fuerza de trabajo.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("Junio 2025/CSV/Fuerza de trabajo.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("Julio 2025/CSV/Fuerza de trabajo.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("Mayo 2025/CSV/No ocupados.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("Junio 2025/CSV/No ocupados.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("Julio 2025/CSV/No ocupados.CSV", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("Mayo 2025/CSV/Ocupados.CSV", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("Junio 2025/CSV/Ocupados.CSV", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("Julio 2025/CSV/Ocupados.CSV", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("Mayo 2025/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_junio  <- read_delim("Junio 2025/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_julio  <- read_delim("Julio 2025/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

fuerzat <- bind_rows(fuerzat_mayo, fuerzat_junio, fuerzat_julio)
no_ocup <- bind_rows(no_ocup_mayo, no_ocup_junio, no_ocup_julio)

ocupados_mayo$INGLABO  <- as.numeric(ocupados_mayo$INGLABO)
ocupados_junio$INGLABO <- as.numeric(ocupados_junio$INGLABO)
ocupados_julio$INGLABO <- as.numeric(ocupados_julio$INGLABO)
ocupados <- bind_rows(ocupados_mayo, ocupados_junio, ocupados_julio)

caract_grales <- bind_rows(caract_gral_mayo, caract_gral_junio, caract_gral_julio)

# ── VARIABLE CUALITATIVA  ─────────────────────────────────────────────────────────────────────────────────
caract_grales <- caract_grales %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1                 ~ "Sin educación",
    P3042 %in% c(2, 3)        ~ "Primaria",
    P3042 %in% c(4, 5, 6, 7)  ~ "Secundaria",
    P3042 == 8                 ~ "Técnico",
    P3042 == 9                 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    P3042 == 99                ~ NA_character_
  )) %>%
  mutate(nivel_educacion = factor(nivel_educacion,
                                  levels = c("Sin educación", "Primaria",
                                             "Secundaria", "Técnico",
                                             "Tecnólogo", "Profesional"),
                                  ordered = TRUE))

edu_directorio <- caract_grales %>%
  group_by(DIRECTORIO) %>%
  summarise(
    nivel_educacion = names(which.max(table(nivel_educacion, useNA = "no"))),
    .groups = 'drop'
  ) %>%
  mutate(nivel_educacion = factor(nivel_educacion,
                                  levels = c("Sin educación", "Primaria",
                                             "Secundaria", "Técnico",
                                             "Tecnólogo", "Profesional"),
                                  ordered = TRUE))

# ── BASE AGREGADA POR DIRECTORIO ──────────────────────────────────────────────
base <- plyr::join_all(
  list(
    fuerzat %>%
      group_by(DIRECTORIO) %>%
      summarise(
        en_edad           = sum(PET == 1, na.rm = TRUE),
        en_fuerza_trabajo = sum(FT  == 1, na.rm = TRUE),
        fuera_ft          = sum(FFT == 1, na.rm = TRUE),
        .groups = 'drop'
      ),
    ocupados %>%
      group_by(DIRECTORIO) %>%
      summarise(
        n_ocupados   = n(),
        ingreso_prom = mean(INGLABO, na.rm = TRUE),
        .groups = 'drop'
      ),
    no_ocup %>%
      group_by(DIRECTORIO) %>%
      summarise(
        n_desocupados = sum(DSI  == 1, na.rm = TRUE),
        n_disponibles = sum(P744 == 1, na.rm = TRUE),
        .groups = 'drop'
      ),
    edu_directorio
  ),
  by = 'DIRECTORIO', type = "left", match = 'all'
)

# ── LIMPIEZA Y TRANSFORMACIONES──────────────────────────────────────────────────────────────────
base <- base %>%
  filter(
    en_edad > 0,
    !is.na(ingreso_prom),
    ingreso_prom > 0
  ) %>%
  mutate(
    n_desocupados = replace_na(n_desocupados, 0),
    n_disponibles = replace_na(n_disponibles, 0),
    
    #if_else(condición, valor_si_TRUE, valor_si_FALSE)
    tasa_desempleo      = if_else(en_fuerza_trabajo == 0, 0, (n_desocupados / en_fuerza_trabajo) * 100),
    tasa_disponibilidad = if_else(fuera_ft == 0, 0, (n_disponibles / fuera_ft) * 100),
    log_ingreso_prom    = log(ingreso_prom)
  )

base %>% summary()

# Plantilla para pruebas

base_medias <- base %>%
  filter(!is.na(nivel_educacion)) %>%
  transmute(
    directorio          = DIRECTORIO,
    nivel_educacion     = nivel_educacion,
    grupo_edu           = factor(
      if_else(nivel_educacion %in% c("Técnico", "Tecnólogo", "Profesional"),
              "Superior", "Básica")),
    log_ingreso_prom    = log_ingreso_prom,
    tasa_desempleo      = tasa_desempleo,
    tasa_disponibilidad = tasa_disponibilidad
  )

superior <- base_hipo %>% filter(grupo_edu == "Superior")
basica   <- base_hipo %>% filter(grupo_edu == "Básica")


base_medias %>% summary()

# ══════════════════════════════════════════════════════════════════════════════
# REPORTE DE PRUEBAS DE HIPÓTESIS - ANÁLISIS MULTINIVEL (ALPHA)
# ══════════════════════════════════════════════════════════════════════════════

# 1. Definición de niveles de significancia solicitados
alphas <- c(0.030, 0.070, 0.025, 0.045, 0.001)

# 2. Función auxiliar para dar formato de tabla profesional
imprimir_tabla_alpha <- function(titulo, estadistico_nombre, datos_tabla) {
  ancho <- 90
  cat("\n", rep("═", ancho), "\n", sep="")
  cat(format(titulo, width = ancho, justify = "centre"), "\n")
  cat(rep("─", ancho), "\n", sep="")
  
  # Encabezados de columna
  cat(format("Alpha (α)", width=12), 
      format(estadistico_nombre, width=15), 
      format("Valor Crítico", width=15), 
      format("Valor p", width=15), 
      format("Confianza", width=12),
      format("Decisión", width=15), "\n")
  cat(rep("─", ancho), "\n", sep="")
  
  # Filas de datos
  for(i in 1:nrow(datos_tabla)) {
    cat(format(datos_tabla$alpha[i], width=12), 
        format(datos_tabla$est[i], width=15), 
        format(datos_tabla$crit[i], width=15), 
        format(datos_tabla$p[i], width=15), 
        format(datos_tabla$conf[i], width=12),
        format(datos_tabla$dec[i], width=15), "\n")
  }
  cat(rep("═", ancho), "\n", sep="")
}

# ══════════════════════════════════════════════════════════════════════════════
# PRUEBA 1: Diferencia de medias — Varianzas CONOCIDAS (Z test)
# Variable: log_ingreso_prom
# ══════════════════════════════════════════════════════════════════════════════

# Pregunta de investigación:
# ¿El ingreso promedio (en escala logarítmica) de los hogares con educación
# superior es significativamente mayor que el de hogares con educación básica?

# H0: μ_superior - μ_basica  = 0   (no hay diferencia en el ingreso promedio)
# H1: μ_superior - μ_basica  > 0   (hogares con educación superior ganan más)

# Prueba: Z para dos muestras independientes con varianzas conocidas.
# Justificación: se asumen como conocidas las desviaciones estándar poblacionales
# aproximándolas con las desviaciones de cada grupo (muestras grandes, n >> 30).
# Nivel de significancia: α = 0.030

prueba1 <- mi_z_test_2muestras(
  x1          = superior$log_ingreso_prom,
  x2          = basica$log_ingreso_prom,
  sigma1      = sd(superior$log_ingreso_prom, na.rm = TRUE),
  sigma2      = sd(basica$log_ingreso_prom,   na.rm = TRUE),
  D0          = 0,
  alternativa = "mayor",
  conf.level  = 1 - 0.030,
  graficar    = TRUE
)

salida <- paste0(
  "\n====== PRUEBA 1: Z dos muestras — log ingreso promedio ======\n",
  "Nivel de significancia (α):  0.030\n",
  "Media grupo Superior:        ", round(prueba1$media1, 4), "\n",
  "Media grupo Básica:          ", round(prueba1$media2, 4), "\n",
  "Diferencia observada:        ", round(prueba1$diferencia_observada, 4), "\n",
  "Estadístico Z:               ", round(prueba1$estadistico_z, 4), "\n",
  "Valor crítico Z (α=0.030):   ", round(qnorm(1 - 0.030), 4), "\n",
  "Valor p:                     ", round(prueba1$p_value, 6), "\n",
  "IC 97%:                      [", round(prueba1$intervalo_confianza[1], 4), 
  ",", round(prueba1$intervalo_confianza[2], 4), "]\n",
  "─────────────────────────────────────────────────────────────\n",
  "Regla de decisión: Rechazar H0 si Z > ", round(qnorm(1 - 0.030), 4), "\n",
  "Decisión:                     ", prueba1$decision, "\n",
  "Conclusión: Con un nivel de significancia del 3%, se ", 
  ifelse(prueba1$decision == "Rechazar H0",
         "encuentra evidencia estadística suficiente para afirmar que el ingreso promedio
           (log) de los hogares con educación superior es mayor que el de los hogares
           con educación básica.",
         "no encuentra evidencia estadística suficiente para afirmar que el ingreso promedio
           (log) de los hogares con educación superior es mayor que el de los hogares
           con educación básica."), "\n")

cat(salida)


# ══════════════════════════════════════════════════════════════════════════════
# PRUEBA 2: Diferencia de medias — Varianzas IGUALES y DESCONOCIDAS (t test)
# Variable: tasa_desempleo
# ══════════════════════════════════════════════════════════════════════════════

# Pregunta de investigación:
# ¿La tasa de desempleo promedio de los hogares con educación básica es
# significativamente mayor que la de los hogares con educación superior?

# H0: μ_basica - μ_superior = 0   (tasas de desempleo iguales entre grupos)
# H1: μ_basica - μ_superior > 0   (hogares con educación básica tienen más desempleo)

# Prueba: t para dos muestras independientes con varianzas iguales y desconocidas.
# Justificación: se asume homogeneidad de varianzas (varianzas poblacionales
# desconocidas pero iguales), usando varianza agrupada (pooled). Se usa t de Student
# con gl = n1 + n2 - 2.

prueba2 <- mi_t_test_2muestras(
  x1          = basica$tasa_desempleo,
  x2          = superior$tasa_desempleo,
  alternativa = "mayor",
  tipo        = "varianzas_iguales",
  conf.level  = 1 - 0.045,
  graficar    = TRUE
)



salida2 <- paste0(
  "\n====== PRUEBA 2: t varianzas iguales — tasa desempleo ======\n",
  "Nivel de significancia (α):  0.045\n",
  "Estadístico t:               ", round(prueba2$estadistico_t, 4), "\n",
  "Grados de libertad:          ", round(prueba2$grados_libertad, 2), "\n",
  "Valor crítico t (α=0.045):   ", round(qt(1 - 0.045, df = prueba2$grados_libertad), 4), "\n",
  "Valor p:                     ", round(prueba2$p_value, 6), "\n",
  "IC 95.5%:                    [", round(prueba2$intervalo_confianza[1], 4), 
  ",", round(prueba2$intervalo_confianza[2], 4), "]\n",
  "─────────────────────────────────────────────────────────────\n",
  "Regla de decisión: Rechazar H0 si t > ", round(qt(1 - 0.045, df = prueba2$grados_libertad), 4), "\n",
  "Decisión:                    ", prueba2$decision, "\n",
  "Conclusión: Con un nivel de significancia del 4.5%, se ", 
  ifelse(prueba2$decision == "Rechazar H0", 
         "encuentra evidencia estadística suficiente para afirmar que la tasa de desempleo de los hogares con educación básica es mayor que la de los hogares con educación superior.", 
         "no encuentra evidencia estadística suficiente para afirmar que la tasa de desempleo de los hogares con educación básica es mayor que la de los hogares con educación superior."), "\n")

cat(salida2)


# ══════════════════════════════════════════════════════════════════════════════
# PRUEBA 3: Diferencia de medias — Varianzas DESIGUALES y DESCONOCIDAS (Welch)
# ══════════════════════════════════════════════════════════════════════════════

# Pregunta de investigación:
# ¿Existe una diferencia significativa en la tasa de disponibilidad laboral
# entre hogares con educación básica y hogares con educación superior?

# H0: μ_basica - μ_superior = 0   (no hay diferencia en la tasa de disponibilidad)
# H1: μ_basica - μ_superior ≠ 0   (existe diferencia en la tasa de disponibilidad)

# Prueba: t de Welch para dos muestras independientes con varianzas desiguales
# y desconocidas.
# Justificación: no se asume homogeneidad de varianzas entre grupos. La prueba
# de Welch ajusta los grados de libertad mediante la aproximación de Satterthwaite,
# siendo más robusta cuando las varianzas poblacionales difieren.

prueba3 <- mi_t_test_2muestras(
  x1          = basica$tasa_disponibilidad,
  x2          = superior$tasa_disponibilidad,
  alternativa = "bilateral",
  tipo        = "varianzas_desiguales",
  conf.level  = 1 - 0.025,
  graficar    = TRUE
)

salida3 <- paste0(
  "\n====== PRUEBA 3: t Welch varianzas desiguales — tasa disponibilidad ======\n",
  "Nivel de significancia (α):  0.025\n",
  "Estadístico t:               ", round(prueba3$estadistico_t, 4), "\n",
  "Grados de libertad (Welch):  ", round(prueba3$grados_libertad, 2), "\n",
  "Valor crítico t (α/2=0.0125):", round(qt(1 - 0.025/2, df = prueba3$grados_libertad), 4), "\n",
  "Valor p:                     ", round(prueba3$p_value, 6), "\n",
  "IC 97.5%:                    [", round(prueba3$intervalo_confianza[1], 4), 
  ",", round(prueba3$intervalo_confianza[2], 4), "]\n",
  "─────────────────────────────────────────────────────────────\n",
  "Regla de decisión: Rechazar H0 si |t| > ", round(qt(1 - 0.025/2, df = prueba3$grados_libertad), 4), "\n",
  "Decisión:                    ", prueba3$decision, "\n",
  "Conclusión: Con un nivel de significancia del 2.5%, se ", 
  ifelse(prueba3$decision == "Rechazar H0", 
         "encuentra evidencia estadística suficiente para afirmar que existe una 
         diferencia significativa en la tasa de disponibilidad laboral entre
         hogares con educación básica y hogares con educación superior.", 
         "no encuentra evidencia estadística suficiente para afirmar que existe una
         diferencia significativa en la tasa de disponibilidad laboral entre
         hogares con educación básica y hogares con educación superior."), "\n"
)

cat(salida3)
