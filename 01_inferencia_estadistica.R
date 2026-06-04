rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

# ── IMPORTAR ──────────────────────────────────────────────────────────────────
fuerzat_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
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

# === ANÁLISIS NUMÉRICO VARIABLES CUANTITATIVAS =====================================================================
analisis_numerico <- function(x) {
  x <- x[!is.na(x)]
  data.frame(
    Media    = mean(x),
    Mediana  = median(x),
    Moda     = as.numeric(names(sort(table(x), decreasing = TRUE)[1])),
    STD       = sd(x),
    Varianza = var(x),
    CV       = (sd(x) / mean(x)) * 100,
    Min      = min(x),
    Q1       = quantile(x, 0.25),
    Q3       = quantile(x, 0.75),
    Max      = max(x),
    IQR      = IQR(x),
    Asimetria = (mean(x) - median(x)) / sd(x)  # índice de Pearson
  )
}

resumen_desempleo      <- analisis_numerico(base$tasa_desempleo)
resumen_disponibilidad <- analisis_numerico(base$tasa_disponibilidad)
resumen_ingreso        <- analisis_numerico(base$log_ingreso_prom)

tabla_resumen <- bind_rows(
  resumen_desempleo,
  resumen_disponibilidad,
  resumen_ingreso
)

# Agregar nombres de variables
tabla_resumen$Variable <- c("Tasa desempleo (%)",
                            "Tasa disponibilidad (%)",
                            "Log ingreso promedio")

tabla_resumen <- tabla_resumen %>%
  select(Variable, everything())

print(tabla_resumen)

# === ANÁLISIS NUMÉRICO VARIABLE CUALITATIVA ======================================================
tabla_edu <- base %>%   
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%
  mutate(
    Porcentaje    = round((n / sum(n)) * 100, 2),
    Acumulado     = cumsum(Porcentaje)
  ) %>%
  rename(
    Nivel         = nivel_educacion,
    Frecuencia    = n
  )

print(tabla_edu)

# === ANÁLISIS GRÁFICO ============================================================================
library(ggplot2)
library(grid)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# HISTOGRAMAS ──────────────────────────────────────────────────────────────────
multiplot(
  ggplot(data = base, aes(x = tasa_desempleo)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Desempleo del Hogar') +
    xlab('Tasa (%)') + ylab('Densidad'),
  
  ggplot(data = base, aes(x = tasa_disponibilidad)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Disponibilidad del Hogar') +
    xlab('Tasa (%)') + ylab('Densidad'),
  
  ggplot(data = base, aes(x = log_ingreso_prom)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Log del Ingreso Promedio del Hogar') +
    xlab('Log(ingreso)') + ylab('Densidad'),
  
  cols = 3
)

# BOXPLOTS ─────────────────────────────────────────────────────────────────────
multiplot(
  ggplot(data = base, aes(y = tasa_desempleo)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Desempleo') +
    ylab('Tasa (%)'),
  
  ggplot(data = base, aes(y = tasa_disponibilidad)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Disponibilidad') +
    ylab('Tasa (%)'),
  
  ggplot(data = base, aes(y = log_ingreso_prom)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Log Ingreso Promedio') +
    ylab('Log(ingreso)'),
  
  cols = 3
)

# GRÁFICO DE BARRAS — nivel_educacion ──────────────────────────────────────────
ggplot(data = base %>% filter(!is.na(nivel_educacion)),
       aes(x = nivel_educacion)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = 'white', col = 'black') +
  theme_test() +
  ggtitle('Distribución por Nivel Educativo del Hogar') +
  xlab('Nivel educativo') +
  ylab('Porcentaje (%)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === ESTIMACIÓN POR ANALOGÍA =====================================================================================================
set.seed(123)

n_muestra <- 500  # tamaño de muestra

muestra_ale_base <- base[sample(nrow(base), n_muestra), ]  # muestra aleatoria de la base

muestra_v1 <- muestra_ale_base$tasa_desempleo
muestra_v2 <- muestra_ale_base$tasa_disponibilidad
muestra_v3 <- muestra_ale_base$log_ingreso_prom

# ── Variables Cuantitativas ──────────────────────────────────────────────────
analogia <- data.frame(
  Variable  = c("Tasa desempleo (%)", "Tasa disponibilidad (%)", "Log ingreso promedio"),
  Estimador = c("Media muestral", "Media muestral", "Media muestral"),
  Valor     = c(
    round(mean(muestra_v1, na.rm = TRUE), 4),
    round(mean(muestra_v2, na.rm = TRUE), 4),
    round(mean(muestra_v3, na.rm = TRUE), 4)
  ),
  Valor_Poblacional = c(
    round(mean(base$tasa_desempleo,      na.rm = TRUE), 4),
    round(mean(base$tasa_disponibilidad, na.rm = TRUE), 4),
    round(mean(base$log_ingreso_prom,    na.rm = TRUE), 4)
  )
)
print(analogia)

# ── Variable Cualitativa ──────────────────────────────────────────────────
analogia_edu <- muestra_ale_base %>%
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%     # Genera automáticamente una columna llamada n con esos conteos.
  mutate(
    p_muestral   = round(n / sum(n), 4),
    p_poblacional = round(
      table(base$nivel_educacion)[as.character(nivel_educacion)] / 
        sum(!is.na(base$nivel_educacion)), 4)
  )
print(analogia_edu)

# === ESTIMACIÓN POR MÁXIMA VEROSIMILITUD =====================================

# ── FUNCIÓN DE LOG-VEROSIMILITUD (normal) ───────────────────────────────────
log_verosimilitud_normal <- function(params, datos) {
  mu    <- params[1]  # media
  sigma <- params[2]  # desviación estándar
  
  if (sigma <= 0) return(Inf)  # sigma debe ser positivo
  -sum(dnorm(datos, mean = mu, sd = sigma, log = TRUE))  # negativo porque optim() minimiza
}

# ── OPTIMIZACIÓN ──────────────────────────────────────────────────────────────
optimizar_mle <- function(datos) {
  datos <- datos[!is.na(datos) & is.finite(datos)]
  optim(
    par    = c(mu = mean(datos), sigma = sd(datos)),  # valores iniciales
    fn     = log_verosimilitud_normal,
    datos  = datos,
    #method = "L-BFGS-B",                              # método eficiente O(n)
    lower  = c(-Inf, 1e-6)                            # sigma > 0
  )
}

mle_v1 <- optimizar_mle(muestra_v1)
mle_v2 <- optimizar_mle(muestra_v2)
mle_v3 <- optimizar_mle(muestra_v3)

# ── TABLA RESUMEN ─────────────────────────────────────────────────────────────
mle_tabla <- data.frame(
  Variable = c("Tasa desempleo (%)", "Tasa disponibilidad (%)", "Log ingreso promedio"),
  MLE_media = c(
    round(mle_v1$par["mu"], 4),
    round(mle_v2$par["mu"], 4),
    round(mle_v3$par["mu"], 4)
  ),
  MLE_sigma = c(
    round(mle_v1$par["sigma"], 4),
    round(mle_v2$par["sigma"], 4),
    round(mle_v3$par["sigma"], 4)
  ),
  Valor_Poblacional = c(
    round(mean(base$tasa_desempleo,      na.rm = TRUE), 4),
    round(mean(base$tasa_disponibilidad, na.rm = TRUE), 4),
    round(mean(base$log_ingreso_prom,    na.rm = TRUE), 4)
  )
)
print(mle_tabla)

# ── VARIABLE CUALITATIVA: MLE para proporción ─────────────────────────────────
log_verosimilitud_bernoulli <- function(p, x, n) {
  if (p <= 0 | p >= 1) return(Inf)
  -(x * log(p) + (n - x) * log(1 - p))  # negativo para minimizar
}

n_total_muestra <- sum(!is.na(muestra_ale_base$nivel_educacion))

mle_edu <- muestra_ale_base %>%
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%
  mutate(
    MLE_proporcion = round(n / n_total_muestra, 4),
    p_poblacional  = round(
      table(base$nivel_educacion)[as.character(nivel_educacion)] /
        sum(!is.na(base$nivel_educacion)), 4),
    
    # Verificar con optimización numérica
    MLE_verificado = round(sapply(n, function(x) {
      optimize(log_verosimilitud_bernoulli,
               interval = c(1e-6, 1 - 1e-6),
               x = x, n = n_total_muestra)$minimum 
    }), 4)
  )
print(mle_edu)

# === INTERVALOS DE CONFIANZA (t-Student) =====================================
ic_t <- function(datos, nivel_conf = 0.95, nombre_var = "") {
  x     <- datos[!is.na(datos) & is.finite(datos)]
  n     <- length(x)
  media <- mean(x)
  ee    <- sd(x) / sqrt(n)                        # error estándar
  alpha <- 1 - nivel_conf
  t_crit <- qt(1 - alpha/2, df = n - 1)           # valor crítico t
  
  li <- media - t_crit * ee                        # límite inferior
  ls <- media + t_crit * ee                        # límite superior
  
  cat("======", nombre_var, "======\n")
  cat("n:               ", n,              "\n")
  cat("Media muestral:  ", round(media, 4), "\n")
  cat("Error estándar:  ", round(ee,    4), "\n")
  cat("t crítico:       ", round(t_crit,4), "\n")
  cat("IC 95%:          [", round(li, 4), ",", round(ls, 4), "]\n\n")
  
  invisible(list(li = li, ls = ls, media = media, n = n))
}

ic_v1 <- ic_t(muestra_v1, nombre_var = "Tasa desempleo (%)")
ic_v2 <- ic_t(muestra_v2, nombre_var = "Tasa disponibilidad (%)")
ic_v3 <- ic_t(muestra_v3, nombre_var = "Log ingreso promedio")


#── IC para proporciones — variable cualitativa──────────────────────────────────────
ic_proporcion <- function(m_base, nivel_conf = 0.95) {
  n     <- sum(!is.na(m_base$nivel_educacion))
  alpha <- 1 - nivel_conf
  z     <- qnorm(1 - alpha/2)
  
  tabla <- m_base %>%
    filter(!is.na(nivel_educacion)) %>%
    count(nivel_educacion) %>%
    mutate(
      p     = n / sum(n),
      ee    = sqrt(p * (1 - p) / sum(n)),          # error estándar de proporción
      li    = round(p - z * ee, 4),                 # límite inferior
      ls    = round(p + z * ee, 4),                 # límite superior
      p     = round(p, 4)
    ) %>%
    select(nivel_educacion, p, li, ls)
  
  print(tabla)
  invisible(tabla)
}
ic_v4 <- ic_proporcion(muestra_ale_base)

# =============================================================================
# VARIABLES CUANTITATIVAS
# =============================================================================
# === INSESGAMIENTO ===========================================================
insesgamiento <- function(datos, nombre_var, B = 10000) {
  x <- datos[!is.na(datos) & is.finite(datos)]
  n <- length(x)
  
  media_original   <- mean(x)
  mediana_original <- median(x)
  
  #Bootstrap: B remuestras con reemplazo
  media_boot   <- numeric(B)
  mediana_boot <- numeric(B)
  
  for (b in 1:B) {
    remuestra      <- sample(x, size = n, replace = TRUE)
    media_boot[b]   <- mean(remuestra)
    mediana_boot[b] <- median(remuestra)
  }
  
  prom_media_boot   <- mean(media_boot)
  prom_mediana_boot <- mean(mediana_boot)
  
  sesgo_media   <- prom_media_boot   - media_original
  sesgo_mediana <- prom_mediana_boot - mediana_original
  
  cat("======", nombre_var, "======\n")
  cat("Muestra original:\n")
  cat("  Media:           ", round(media_original,   4), "\n")
  cat("  Mediana:         ", round(mediana_original, 4), "\n")
  cat("Bootstrap (B =", B, "):\n")
  cat("  Prom. medias:    ", round(prom_media_boot,   4), "\n")
  cat("  Prom. medianas:  ", round(prom_mediana_boot, 4), "\n")
  cat("Sesgo:\n")
  cat("  Sesgo media:     ", round(sesgo_media,   6), "\n")
  cat("  Sesgo mediana:   ", round(sesgo_mediana, 6), "\n")
  cat("Interpretación:\n")
  cat("  Media:  ", ifelse(abs(sesgo_media)   < 0.01, "Insesgada", "Sesgada"), "\n")
  cat("  Mediana:", ifelse(abs(sesgo_mediana) < 0.01, "Insesgada", "Sesgada"), "\n\n")
  
  invisible(list(
    media_original   = media_original,
    mediana_original = mediana_original,
    sesgo_media      = sesgo_media,
    sesgo_mediana    = sesgo_mediana
  ))
}
ins_v1 <- insesgamiento(muestra_v1, "Tasa desempleo (%)")
ins_v2 <- insesgamiento(muestra_v2, "Tasa disponibilidad (%)")
ins_v3 <- insesgamiento(muestra_v3, "Log ingreso promedio")

# === CONSISTENCIA DE ESTIMADORES =============================================
consistencia <- function(datos, nombre_var, n_muestra) {
  x <- datos[!is.na(datos) & is.finite(datos)]
  
  cat("======", nombre_var, "======\n")
  cat("-- Estimadores muestra original --\n")
  cat("  Media:    ", round(mean(x),   4), "\n")
  cat("  Mediana:  ", round(median(x), 4), "\n")
  cat("  Varianza: ", round(var(x),    4), "\n\n")

  valores_B <- c(100, 1000, 5000, 10000)
  plots_consistencia <- list()
  
  for (i in seq_along(valores_B)) {
    B <- valores_B[i]
    media_boot <- numeric(B)
    
    for (b in 1:B) {
      remuestra     <- sample(x, size = n_muestra, replace = TRUE)
      media_boot[b] <- mean(remuestra)
    }
    
    df_boot <- data.frame(media_boot = media_boot)
    
    plots_consistencia[[i]] <- ggplot(df_boot, aes(x = media_boot)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50,
                     fill = 'white', col = 'black') +
      theme_test() +
      ggtitle(paste0(nombre_var, " B = ", B)) +
      xlab("Media bootstrap") +
      ylab("Densidad")
  }
  
  multiplot(plots_consistencia[[1]],
            plots_consistencia[[2]],
            plots_consistencia[[3]],
            plots_consistencia[[4]],
            cols = 2)
  
  invisible(plots_consistencia)
}
con_v1 <- consistencia(muestra_v1, "Tasa desempleo (%)",      n_muestra)
con_v2 <- consistencia(muestra_v2, "Tasa disponibilidad (%)", n_muestra)
con_v3 <- consistencia(muestra_v3, "Log ingreso promedio",    n_muestra)

# === EFICIENCIA DE ESTIMADORES ===============================================
eficiencia <- function(datos, nombre_var, n_muestra, B = 10000) {
  x <- datos[!is.na(datos) & is.finite(datos)]
  
  cat("======", nombre_var, "======\n")
  cat("-- Estimadores muestra original --\n")
  cat("  Media:    ", round(mean(x),   4), "\n")
  cat("  Mediana:  ", round(median(x), 4), "\n")
  cat("  Varianza: ", round(var(x),    4), "\n\n")
  
  media_boot   <- numeric(B)
  mediana_boot <- numeric(B)
  
  for (b in 1:B) {
    remuestra      <- sample(x, size = n_muestra, replace = TRUE)
    media_boot[b]   <- mean(remuestra)
    mediana_boot[b] <- median(remuestra)
  }
  
  # Varianzas y eficiencia relativa
  var_media     <- var(media_boot)
  var_mediana   <- var(mediana_boot)
  ef_relativa   <- min(var_media, var_mediana) / max(var_media, var_mediana)
  mas_eficiente <- ifelse(var_media < var_mediana, "Media", "Mediana")
  
  cat("-- Eficiencia (B =", B, ") --\n")
  cat("  Var(media):            ", round(var_media,    6), "\n")
  cat("  Var(mediana):          ", round(var_mediana,  6), "\n")
  cat("  Eficiencia relativa:   ", round(ef_relativa,  4), "\n")
  cat("  Estimador más eficiente:", mas_eficiente,          "\n\n")
  
  invisible(list(
    var_media     = var_media,
    var_mediana   = var_mediana,
    ef_relativa   = ef_relativa,
    mas_eficiente = mas_eficiente
  ))
}
ef_v1 <- eficiencia(muestra_v1, "Tasa desempleo (%)",      n_muestra)
ef_v2 <- eficiencia(muestra_v2, "Tasa disponibilidad (%)", n_muestra)
ef_v3 <- eficiencia(muestra_v3, "Log ingreso promedio",    n_muestra)

# === ANÁLISIS VARIABLE CUALITATIVA: nivel_educacion ==========================

# Vector categórico de la muestra
muestra_v4 <- muestra_ale_base$nivel_educacion
muestra_v4 <- muestra_v4[!is.na(muestra_v4)]
n_v4       <- length(muestra_v4)

# ── FUNCIÓN AUXILIAR: calcula proporciones y moda de un vector ────────────────
calc_prop_moda <- function(x) {
  props <- prop.table(table(x))  # proporciones por categoría
  moda  <- names(which.max(props))  # categoría más frecuente
  list(props = props, moda = moda)
}

# =============================================================================
# VARIABLE CUALITATIVA
# =============================================================================
muestra_v4 <- muestra_ale_base$nivel_educacion
muestra_v4 <- muestra_v4[!is.na(muestra_v4)]
n_v4       <- length(muestra_v4)

# ── FUNCIÓN AUXILIAR: calcula proporciones y moda de un vector ────────────────
calc_prop_moda <- function(x) {
  props <- prop.table(table(x))  # proporciones por categoría
  moda  <- names(which.max(props))  # categoría más frecuente
  list(props = props, moda = moda)
}

# == Insesgamiento ============================================================
insesgamiento_v4 <- function(datos, B = 5000) {
  n <- length(datos)
  
  original      <- calc_prop_moda(datos)
  props_orig    <- original$props
  moda_orig     <- original$moda
  categorias    <- names(props_orig)
  
  props_boot <- matrix(0, nrow = B, ncol = length(categorias),
                       dimnames = list(NULL, categorias))
  moda_boot  <- character(B)
  
  for (b in 1:B) {
    remuestra       <- sample(datos, size = n, replace = TRUE)
    res             <- calc_prop_moda(remuestra)
    props_boot[b, categorias] <- as.numeric(res$props[categorias])
    moda_boot[b]    <- res$moda
  }
  
  # Promedio bootstrap y sesgo por categoría
  prom_boot <- colMeans(props_boot)
  sesgo     <- prom_boot - as.numeric(props_orig[categorias])
  
  cat("-- Sesgo por categoría (B =", B, ") --\n")
  tabla_sesgo <- data.frame(
    Categoria      = as.character(categorias),
    Prop_original  = round(as.numeric(props_orig), 4),
    Prom_bootstrap = round(prom_boot,              4),
    Sesgo          = round(sesgo,                  6),
    Insesgado      = ifelse(abs(sesgo) < 0.01, "Sí", "No")
  )
  print(tabla_sesgo)
  
  # Estabilidad de la moda
  cat("\n-- Frecuencia de moda en bootstrap --\n")
  frec_moda <- prop.table(table(moda_boot)) * 100
  for (cat in names(frec_moda)) {
    cat("  ", cat, ":", round(frec_moda[cat], 2), "%\n")
  }
  cat("  Moda original:", moda_orig, "→",
      round(frec_moda[moda_orig], 2), "% de las remuestras\n\n")
  
  invisible(list(props_orig = props_orig, sesgo = sesgo,
                 moda_orig = moda_orig, frec_moda = frec_moda))
}
ins_v4 <- insesgamiento_v4(muestra_v4)

# == CONSISTENCIA ==============================================================
consistencia_v4 <- function(datos, n_muestra) {
  x          <- datos[!is.na(datos)]
  cat_principal <- names(which.max(prop.table(table(x))))  # categoría más frecuente
  valores_B  <- c(100, 500, 1000, 5000)
  plots_v4   <- list()
  
  cat("====== nivel_educacion — Consistencia ======\n")
  cat("Categoría analizada:", cat_principal, "\n\n")
  
  for (i in seq_along(valores_B)) {
    B          <- valores_B[i]
    prop_boot  <- numeric(B)
    
    for (b in 1:B) {
      remuestra    <- sample(x, size = n_muestra, replace = TRUE)
      prop_boot[b] <- mean(remuestra == cat_principal)  # proporción de cat_principal
    }
    
    df_boot <- data.frame(prop_boot = prop_boot)
    
    plots_v4[[i]] <- ggplot(df_boot, aes(x = prop_boot)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40,
                     fill = 'white', col = 'black') +
      theme_test() +
      ggtitle(paste0("nivel_educacion (", cat_principal, ") B = ", B)) +
      xlab("Proporción bootstrap") +
      ylab("Densidad")
  }
  
  multiplot(plots_v4[[1]], plots_v4[[2]],
            plots_v4[[3]], plots_v4[[4]],
            cols = 2)
  
  invisible(plots_v4)
}
con_v4 <- consistencia_v4(muestra_v4, n_v4)

# == EFICIENCIA ================================================================
evaluar_eficiencia_v4 <- function(datos, n_muestra, B = 10000) {
  x          <- datos[!is.na(datos)]
  categorias <- levels(x)
  
  props_boot <- matrix(0, nrow = B, ncol = length(categorias),
                       dimnames = list(NULL, categorias))
  moda_boot  <- character(B)
  
  for (b in 1:B) {
    remuestra <- sample(x, size = n_muestra, replace = TRUE)
    for (cat in categorias) {
      props_boot[b, cat] <- mean(remuestra == cat)
    }
    moda_boot[b] <- names(which.max(table(remuestra)))
  }
  
  # Varianza por categoría
  var_props <- apply(props_boot, 2, var)
  mas_estable <- names(which.min(var_props))
  
  cat("====== nivel_educacion — Eficiencia (B =", B, ") ======\n")
  cat("-- Varianza de proporciones bootstrap por categoría --\n")
  tabla_ef <- data.frame(
    Categoria  = as.character(categorias),
    Varianza   = round(var_props[categorias], 8),
    Eficiencia = ifelse(categorias == mas_estable, "← Más estable", "")
  )
  print(tabla_ef)

  cat("\n-- Estabilidad de la moda --\n")
  frec_moda <- prop.table(table(moda_boot)) * 100
  for (cat in names(frec_moda)) {
    cat("  ", cat, ":", round(frec_moda[cat], 2), "%\n")
  }
  
  invisible(list(var_props = var_props, frec_moda = frec_moda))
}
ef_v4 <- evaluar_eficiencia_v4(muestra_v4, n_v4)
