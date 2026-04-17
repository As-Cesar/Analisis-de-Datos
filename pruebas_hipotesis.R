# --------------------------------------------
# PRUEBA DE HIPÓTESIS SOBRE UNA MEDIA (VARIANZA CONOCIDA)
# --------------------------------------------
# En lugar de usar una función predefinida como z.test(), 
# vamos a construir nuestra propia función para aplicar 
# una prueba de hipótesis con media conocida y varianza conocida.
# Esto nos ayudará a comprender cada paso del procedimiento.
# --------------------------------------------

# Creamos la función personalizada
mi_z_test <- function(x, mu0, sigma, alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # Argumentos:
  # x: vector de datos
  # mu0: valor hipotético de la media bajo H0
  # sigma: desviación estándar poblacional (conocida)
  # alternativa: "mayor", "menor", o "bilateral"
  # conf.level: nivel de confianza (por defecto, 95%)
  # graficar: si TRUE, muestra gráfica del contraste en la curva normal

  # Paso 1: cálculo de estadísticas básicas
  n <- length(x)                         # tamaño muestral
  media_muestral <- mean(x)             # media de la muestra
  error_estandar <- sigma / sqrt(n)     # error estándar
  z <- (media_muestral - mu0) / error_estandar  # estadístico Z

  # Paso 2: cálculo del valor-p según el tipo de prueba
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Paso 3: cálculo del intervalo de confianza (simétrico, bilateral)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  IC <- c(media_muestral - z_crit * error_estandar,
          media_muestral + z_crit * error_estandar)

  # Paso 4: mostrar gráfico si se solicita
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = "Distribución Z bajo H0")
    abline(v = z, col = "blue", lwd = 2, lty = 2)
    legend("topright", legend = paste0("Estadístico Z = ", round(z, 3)),
           col = "blue", lty = 2, bty = "n")
    
    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
      legend("topleft", legend = paste("Z críticos:", round(-z_crit, 2), "y", round(z_crit, 2)),
             col = "red", lty = 3, bty = "n")
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Paso 5: devolver los resultados
  list(
    media_muestral = media_muestral,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# --------------------------------------------
# PRUEBA DE HIPÓTESIS PARA UNA PROPORCIÓN
# --------------------------------------------
# Esta función reproduce lo que hace prop.test() pero desde cero,
# para entender cada paso de la prueba sobre una proporción.
# --------------------------------------------

mi_prop_test <- function(x, n, p0, alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # x: número de éxitos observados
  # n: tamaño de la muestra
  # p0: proporción bajo la hipótesis nula
  # alternativa: "mayor", "menor", "bilateral"
  # conf.level: nivel de confianza
  # graficar: si TRUE, muestra la curva Z con el valor calculado
  
  p_hat <- x / n                     # proporción observada
  error_estandar <- sqrt(p0 * (1 - p0) / n)
  z <- (p_hat - p0) / error_estandar  # estadístico Z
  
  # Cálculo del valor-p según el tipo de prueba
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza para p (usando p_hat)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  IC <- c(p_hat - z_crit * sqrt(p_hat * (1 - p_hat) / n),
          p_hat + z_crit * sqrt(p_hat * (1 - p_hat) / n))

  # Gráfico (si se desea)
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = "Distribución Z bajo H0")
    abline(v = z, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("Z =", round(z, 3)),
           col = "blue", lty = 2, bty = "n")
    
    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Resultados
  list(
    proporcion_observada = p_hat,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# --------------------------------------------
# PRUEBA DE HIPÓTESIS PARA UNA MEDIA (VARIANZA DESCONOCIDA)
# --------------------------------------------
# Esta función reproduce el funcionamiento básico de t.test()
# para una muestra, calculando manualmente el estadístico t,
# el valor-p y el intervalo de confianza.
# --------------------------------------------

mi_t_test <- function(x, mu0, alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # x: vector de datos
  # mu0: media hipotética bajo H0
  # alternativa: "mayor", "menor", "bilateral"
  # conf.level: nivel de confianza
  # graficar: mostrar la curva t con el valor t calculado

  n <- length(x)
  media_muestral <- mean(x)
  s <- sd(x)
  error_estandar <- s / sqrt(n)
  gl <- n - 1
  t <- (media_muestral - mu0) / error_estandar

  # Valor-p según el tipo de prueba
  p_value <- switch(alternativa,
                    "mayor" = pt(t, df = gl, lower.tail = FALSE),
                    "menor" = pt(t, df = gl, lower.tail = TRUE),
                    "bilateral" = 2 * pt(-abs(t), df = gl),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza
  alpha <- 1 - conf.level
  t_crit <- qt(1 - alpha / 2, df = gl)
  IC <- c(media_muestral - t_crit * error_estandar,
          media_muestral + t_crit * error_estandar)

  # Gráfico opcional
  if (graficar) {
    curve(dt(x, df = gl), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "t", main = "Distribución t de Student")
    abline(v = t, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("t =", round(t, 3)),
           col = "blue", lty = 2, bty = "n")

    if (alternativa == "bilateral") {
      abline(v = c(-t_crit, t_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qt(1 - alpha, df = gl), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qt(alpha, df = gl), col = "red", lty = 3)
    }
  }

  # Salida
  list(
    media_muestral = media_muestral,
    estadistico_t = t,
    grados_libertad = gl,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# --------------------------------------------
# PRUEBA Z PARA DOS MUESTRAS INDEPENDIENTES
# --------------------------------------------
# Esta función permite comparar dos medias poblacionales
# con varianzas conocidas, usando una prueba Z.
# --------------------------------------------

mi_z_test_2muestras <- function(x1, x2, sigma1, sigma2, D0 = 0,
                                alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # x1, x2: vectores con las dos muestras
  # sigma1, sigma2: desviaciones estándar poblacionales conocidas
  # D0: diferencia esperada bajo H0 (por defecto = 0)
  # alternativa: "mayor", "menor", o "bilateral"
  # conf.level: nivel de confianza para IC
  # graficar: si TRUE, dibuja la curva Z con el valor crítico y estadístico
  
  n1 <- length(x1)
  n2 <- length(x2)
  media1 <- mean(x1)
  media2 <- mean(x2)
  
  # Estadístico z
  error_estandar <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
  z <- ((media1 - media2) - D0) / error_estandar

  # Valor-p
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  IC <- c((media1 - media2) - z_crit * error_estandar,
          (media1 - media2) + z_crit * error_estandar)

  # Gráfico
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = "Prueba Z para dos muestras")
    abline(v = z, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("Z =", round(z, 3)),
           col = "blue", lty = 2, bty = "n")

    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Resultado
  list(
    media1 = media1,
    media2 = media2,
    diferencia_observada = media1 - media2,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# --------------------------------------------
# PRUEBA Z PARA DOS PROPORCIONES INDEPENDIENTES
# --------------------------------------------
# Esta función compara dos proporciones y evalúa
# si la diferencia observada es significativa.
# --------------------------------------------

mi_prop_test_2muestras <- function(x1, n1, x2, n2,
                                   alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # x1, x2: número de éxitos en cada muestra
  # n1, n2: tamaños de las muestras
  # alternativa: "mayor", "menor", "bilateral"
  # conf.level: nivel de confianza para el IC
  # graficar: mostrar curva Z con valores críticos

  # Proporciones muestrales
  p1_hat <- x1 / n1
  p2_hat <- x2 / n2

  # Proporción combinada (bajo H0)
  p_comb <- (x1 + x2) / (n1 + n2)

  # Error estándar
  error_estandar <- sqrt(p_comb * (1 - p_comb) * (1 / n1 + 1 / n2))

  # Estadístico Z
  z <- (p1_hat - p2_hat) / error_estandar

  # Valor-p
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza (no depende de p combinada)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  error_indep <- sqrt((p1_hat * (1 - p1_hat)) / n1 + (p2_hat * (1 - p2_hat)) / n2)
  IC <- c((p1_hat - p2_hat) - z_crit * error_indep,
          (p1_hat - p2_hat) + z_crit * error_indep)

  # Gráfico opcional
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = "Prueba Z para dos proporciones")
    abline(v = z, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("Z =", round(z, 3)),
           col = "blue", lty = 2, bty = "n")

    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Resultados
  list(
    proporcion1 = p1_hat,
    proporcion2 = p2_hat,
    diferencia_observada = p1_hat - p2_hat,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# --------------------------------------------------------------------
# PRUEBA t PARA DOS MUESTRAS - FUNCIÓN PERSONALIZADA EN R
# --------------------------------------------------------------------
# En esta función vamos a construir manualmente la prueba t para comparar
# dos medias poblacionales bajo tres escenarios distintos:
#
# 1. Muestras independientes con varianzas desconocidas e IGUALES
# 2. Muestras independientes con varianzas desconocidas y DESIGUALES
# 3. Muestras pareadas o dependientes (comparación de diferencias)
#
# Esta función permite visualizar claramente cómo se calculan
# el estadístico t, los grados de libertad, el valor-p y el
# intervalo de confianza, sin depender de la función t.test().
#
# Además, incluye la opción de graficar la distribución t de Student,
# el valor crítico y el valor observado del estadístico.
# --------------------------------------------------------------------

mi_t_test_2muestras <- function(x1, x2,
                                alternativa = "bilateral",
                                tipo = "varianzas_desiguales", # "varianzas_iguales", "dependientes"
                                conf.level = 0.95,
                                graficar = TRUE) {
  # x1, x2: vectores con las dos muestras
  # alternativa: "mayor", "menor", "bilateral"
  # tipo: tipo de prueba t -> "varianzas_iguales", "varianzas_desiguales", "dependientes"
  # conf.level: nivel de confianza
  # graficar: dibujar la curva t con el estadístico y valores críticos

  if (tipo == "dependientes") {
    if (length(x1) != length(x2)) stop("Las muestras pareadas deben tener igual longitud.")
    d <- x1 - x2
    n <- length(d)
    media_d <- mean(d)
    sd_d <- sd(d)
    error <- sd_d / sqrt(n)
    gl <- n - 1
    t <- media_d / error

    # IC
    alpha <- 1 - conf.level
    t_crit <- qt(1 - alpha/2, df = gl)
    IC <- c(media_d - t_crit * error, media_d + t_crit * error)

  } else {
    n1 <- length(x1)
    n2 <- length(x2)
    media1 <- mean(x1)
    media2 <- mean(x2)
    var1 <- var(x1)
    var2 <- var(x2)

    if (tipo == "varianzas_iguales") {
      # Pooled variance
      sp2 <- ((n1 - 1)*var1 + (n2 - 1)*var2) / (n1 + n2 - 2)
      error <- sqrt(sp2 * (1/n1 + 1/n2))
      gl <- n1 + n2 - 2

    } else if (tipo == "varianzas_desiguales") {
      # Welch’s t-test
      error <- sqrt(var1/n1 + var2/n2)
      gl <- ( (var1/n1 + var2/n2)^2 ) /
            ( (var1^2 / (n1^2 * (n1 - 1))) + (var2^2 / (n2^2 * (n2 - 1))) )
    } else {
      stop("Tipo de prueba no válido.")
    }

    t <- (media1 - media2) / error
    alpha <- 1 - conf.level
    t_crit <- qt(1 - alpha/2, df = gl)
    IC <- c((media1 - media2) - t_crit * error, (media1 - media2) + t_crit * error)
  }

  # Valor-p según alternativa
  p_value <- switch(alternativa,
                    "mayor" = pt(t, df = gl, lower.tail = FALSE),
                    "menor" = pt(t, df = gl, lower.tail = TRUE),
                    "bilateral" = 2 * pt(-abs(t), df = gl),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))
  alphas <- c(0.030, 0.070, 0.025, 0.045, 0.001)
  
  # Decisiones para cada alpha
  decisiones <- data.frame(
    alpha = alphas,
    decision = ifelse(p_value < alphas, "Rechazar H0", "No Rechazar H0")
  )
  # Gráfico
  if (graficar) {
    curve(dt(x, df = gl), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "t", main = paste("Distribución t (gl =", round(gl, 1), ")"))
    abline(v = t, col = "blue", lwd = 2, lty = 2)
    legend("topright", legend = paste("t =", round(t, 3)), col = "blue", lty = 2, bty = "n")
    if (alternativa == "bilateral") {
      abline(v = c(-t_crit, t_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qt(1 - alpha, df = gl), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qt(alpha, df = gl), col = "red", lty = 3)
    }
  }

  # Salida
  list(
    estadistico_t = t,
    grados_libertad = gl,
    p_value = p_value,
    intervalo_confianza = IC,
    decisiones = decisiones,
    tipo_prueba = tipo
  )
}

# --------------------------------------------------
# PRUEBA DE HIPÓTESIS PARA UNA PROPORCIÓN
# (Versión que recibe un vector binario como entrada)
# --------------------------------------------------

mi_prop_test_vect <- function(x, p0, alternativa = "bilateral", conf.level = 0.95, graficar = TRUE) {
  # x: vector binario (0 y 1) indicando éxito o fracaso
  # p0: proporción bajo H0
  # alternativa: "mayor", "menor", "bilateral"
  # conf.level: nivel de confianza
  # graficar: si TRUE, muestra curva Z

  if (!all(x %in% c(0, 1))) stop("El vector debe contener solo ceros y unos.")

  n <- length(x)
  x_sum <- sum(x)
  p_hat <- x_sum / n
  error_estandar <- sqrt(p0 * (1 - p0) / n)
  z <- (p_hat - p0) / error_estandar

  # Valor-p
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza (basado en p_hat)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  IC <- c(p_hat - z_crit * sqrt(p_hat * (1 - p_hat) / n),
          p_hat + z_crit * sqrt(p_hat * (1 - p_hat) / n))

  # Gráfico
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = "Prueba Z para una proporción")
    abline(v = z, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("Z =", round(z, 3)), col = "blue", lty = 2, bty = "n")
    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  list(
    proporcion_observada = p_hat,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# ---------------------------------------------------------
# PRUEBA Z PARA UNA PROPORCIÓN CON VARIABLES CATEGÓRICAS
# ---------------------------------------------------------
# x: vector categórico (factor o carácter)
# categoria: categoría de interés (valor observado en x)
# p0: proporción bajo H0
# alternativa: "mayor", "menor", "bilateral"
# conf.level: nivel de confianza
# graficar: TRUE para mostrar gráfica de contraste
# ---------------------------------------------------------

mi_prop_test_categoria <- function(x, categoria, p0,
                                   alternativa = "bilateral",
                                   conf.level = 0.95,
                                   graficar = TRUE) {
  
  # Validaciones
  if (!is.factor(x)) x <- as.factor(x)
  if (!(categoria %in% levels(x))) {
    stop("La categoría especificada no se encuentra en los datos.")
  }

  # Cálculos básicos
  n <- length(x)
  x_sum <- sum(x == categoria)
  p_hat <- x_sum / n
  error_estandar <- sqrt(p0 * (1 - p0) / n)
  z <- (p_hat - p0) / error_estandar

  # Valor-p
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza (basado en p_hat)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  IC <- c(p_hat - z_crit * sqrt(p_hat * (1 - p_hat) / n),
          p_hat + z_crit * sqrt(p_hat * (1 - p_hat) / n))

  # Gráfico
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = paste("Contraste Z para la categoría:", categoria))
    abline(v = z, col = "blue", lty = 2, lwd = 2)
    legend("topright", legend = paste("Z =", round(z, 3)), col = "blue", lty = 2, bty = "n")
    
    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Resultados
  list(
    categoria = categoria,
    proporcion_observada = p_hat,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# ----------------------------------------------------------
# PRUEBA Z PARA DOS PROPORCIONES (con vectores binarios)
# ----------------------------------------------------------
# x1, x2: vectores binarios (0 = fracaso, 1 = éxito)
# alternativa: "mayor", "menor", "bilateral"
# conf.level: nivel de confianza
# graficar: si TRUE, muestra curva normal con z
# ----------------------------------------------------------

mi_prop_test_2vectores_binarios <- function(x1, x2,
                                            alternativa = "bilateral",
                                            alphas = c(0.05),
                                            graficar = TRUE) {
  # Validación
  if (!all(x1 %in% c(0, 1)) || !all(x2 %in% c(0, 1))) {
    stop("Ambos vectores deben ser binarios (0 y 1).")
  }
  
  # Conteos y proporciones
  n1 <- length(x1)
  n2 <- length(x2)
  x1_sum <- sum(x1)
  x2_sum <- sum(x2)
  p1_hat <- x1_sum / n1
  p2_hat <- x2_sum / n2
  
  # Proporción combinada (bajo H0)
  p_comb <- (x1_sum + x2_sum) / (n1 + n2)
  
  # Error estándar
  error_estandar <- sqrt(p_comb * (1 - p_comb) * (1 / n1 + 1 / n2))
  
  # Estadístico Z
  z <- (p1_hat - p2_hat) / error_estandar
  
  # Valor-p
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)))
  
  # 🔹 Resultados por cada alpha
  resultados <- lapply(alphas, function(alpha) {
    
    z_crit <- qnorm(1 - alpha / 2)
    
    error_indep <- sqrt((p1_hat * (1 - p1_hat)) / n1 +
                          (p2_hat * (1 - p2_hat)) / n2)
    
    IC <- c((p1_hat - p2_hat) - z_crit * error_indep,
            (p1_hat - p2_hat) + z_crit * error_indep)
    
    list(
      alpha = alpha,
      proporcion_1 = p1_hat,
      proporcion_2 = p2_hat,
      diferencia_observada = p1_hat - p2_hat,
      estadistico_z = z,
      p_value = p_value,
      intervalo_confianza = IC,
      decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
    )
  })
  
  if (graficar) {
    par(mfrow = c(ceiling(length(alphas)/2), 2))  # varias gráficas
    
    for (alpha in alphas) {
      z_crit <- qnorm(1 - alpha / 2)
      
      curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
            ylab = "Densidad", xlab = "Z",
            main = paste("Alpha =", alpha))
      
      abline(v = z, col = "blue", lwd = 2, lty = 2)
      
      if (alternativa == "bilateral") {
        abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
      } else if (alternativa == "mayor") {
        abline(v = qnorm(1 - alpha), col = "red", lty = 3)
      } else if (alternativa == "menor") {
        abline(v = qnorm(alpha), col = "red", lty = 3)
      }
      
      legend("topright",
             legend = c(paste("Z =", round(z, 3)),
                        paste("±Zcrit =", round(z_crit, 3))),
             col = c("blue", "red"),
             lty = c(2, 3),
             bty = "n")
    }
    
    par(mfrow = c(1,1))  # reset
  }
  
  return(resultados)
}

# --------------------------------------------------------------
# PRUEBA Z PARA DOS PROPORCIONES CON VARIABLES CATEGÓRICAS
# --------------------------------------------------------------
# x1, x2: vectores categóricos (factor o character)
# categoria: categoría de interés común en ambos vectores
# alternativa: "mayor", "menor", "bilateral"
# conf.level: nivel de confianza
# graficar: si TRUE, dibuja curva Z con estadístico
# --------------------------------------------------------------

mi_prop_test_2categorias <- function(x1, x2,
                                     categoria,
                                     alternativa = "bilateral",
                                     conf.level = 0.95,
                                     graficar = TRUE) {
  # Convertir a factor si es necesario
  if (!is.factor(x1)) x1 <- as.factor(x1)
  if (!is.factor(x2)) x2 <- as.factor(x2)

  # Verificar que la categoría esté en ambos vectores
  if (!(categoria %in% levels(x1)) || !(categoria %in% levels(x2))) {
    stop("La categoría especificada no se encuentra en ambos vectores.")
  }

  # Cálculos
  n1 <- length(x1)
  n2 <- length(x2)
  x1_sum <- sum(x1 == categoria)
  x2_sum <- sum(x2 == categoria)
  p1_hat <- x1_sum / n1
  p2_hat <- x2_sum / n2

  # Proporción combinada
  p_comb <- (x1_sum + x2_sum) / (n1 + n2)

  # Error estándar combinado bajo H0
  error_estandar <- sqrt(p_comb * (1 - p_comb) * (1 / n1 + 1 / n2))

  # Estadístico Z
  z <- (p1_hat - p2_hat) / error_estandar

  # Valor-p según alternativa
  p_value <- switch(alternativa,
                    "mayor" = pnorm(z, lower.tail = FALSE),
                    "menor" = pnorm(z, lower.tail = TRUE),
                    "bilateral" = 2 * pnorm(-abs(z)),
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Intervalo de confianza (basado en proporciones individuales)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)
  error_indep <- sqrt((p1_hat * (1 - p1_hat)) / n1 + (p2_hat * (1 - p2_hat)) / n2)
  IC <- c((p1_hat - p2_hat) - z_crit * error_indep,
          (p1_hat - p2_hat) + z_crit * error_indep)

  # Gráfico
  if (graficar) {
    curve(dnorm(x), from = -4, to = 4, lwd = 2, col = "gray30",
          ylab = "Densidad", xlab = "Z", main = paste("Contraste para la categoría:", categoria))
    abline(v = z, col = "blue", lwd = 2, lty = 2)
    legend("topright", legend = paste("Z =", round(z, 3)), col = "blue", lty = 2, bty = "n")

    if (alternativa == "bilateral") {
      abline(v = c(-z_crit, z_crit), col = "red", lty = 3)
    } else if (alternativa == "mayor") {
      abline(v = qnorm(1 - alpha), col = "red", lty = 3)
    } else if (alternativa == "menor") {
      abline(v = qnorm(alpha), col = "red", lty = 3)
    }
  }

  # Salida
  list(
    categoria = categoria,
    proporcion_grupo1 = p1_hat,
    proporcion_grupo2 = p2_hat,
    diferencia_observada = p1_hat - p2_hat,
    estadistico_z = z,
    p_value = p_value,
    intervalo_confianza = IC,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0")
  )
}

# ----------------------------------------------------------------
# PRUEBA DE McNEMAR PARA PROPORCIONES PAREADAS (DATOS BINARIOS)
# ----------------------------------------------------------------
# x: vector binario antes (0/1)
# y: vector binario después (0/1)
# exacto: si TRUE usa prueba binomial, si FALSE usa chi-cuadrado
# ----------------------------------------------------------------

mi_prueba_mcnemar <- function(x, y, exacto = FALSE) {
  # Validación
  if (!all(x %in% c(0, 1)) || !all(y %in% c(0, 1))) {
    stop("Ambos vectores deben ser binarios (0 y 1).")
  }
  if (length(x) != length(y)) {
    stop("Los vectores deben tener la misma longitud.")
  }

  # Tabla de contingencia 2x2
  tabla <- table(x, y)

  # Extraer las celdas relevantes
  b <- tabla["1", "0"]  # casos que cambiaron de 1 a 0
  c <- tabla["0", "1"]  # casos que cambiaron de 0 a 1
  n <- b + c

  # Estadístico McNemar
  if (exacto) {
    # Prueba exacta de binomial (más precisa en muestras pequeñas)
    p_value <- binom.test(min(b, c), n = n, p = 0.5, alternative = "two.sided")$p.value
    metodo <- "Prueba exacta de McNemar (binomial)"
  } else {
    # Versión chi-cuadrado sin continuidad
    chi2 <- (abs(b - c))^2 / (b + c)
    p_value <- pchisq(chi2, df = 1, lower.tail = FALSE)
    metodo <- "Prueba de McNemar (chi-cuadrado)"
  }

  # Resultados
  list(
    tabla = tabla,
    b = b,
    c = c,
    n = n,
    p_value = p_value,
    metodo = metodo,
    decision = ifelse(p_value < 0.05, "Rechazar H0", "No Rechazar H0"),
    interpretacion = "H0: No hay cambio significativo entre las proporciones (efecto pareado)"
  )
}

# -----------------------------------------------------------------------
# PRUEBA DE McNEMAR PARA PROPORCIONES PAREADAS CON CATEGORÍAS CUALITATIVAS
# -----------------------------------------------------------------------
# x: vector de respuestas antes (factor o character)
# y: vector de respuestas después
# categoria: categoría de interés (por ejemplo, "Sí")
# exacto: TRUE para prueba exacta (binomial), FALSE para chi-cuadrado
# -----------------------------------------------------------------------

mi_mcnemar_categoria <- function(x, y, categoria, exacto = FALSE) {
  # Convertir a factor si es necesario
  if (!is.factor(x)) x <- as.factor(x)
  if (!is.factor(y)) y <- as.factor(y)

  # Verificación
  if (length(x) != length(y)) stop("Los vectores deben tener la misma longitud.")
  if (!(categoria %in% union(levels(x), levels(y)))) {
    stop("La categoría especificada no se encuentra en los datos.")
  }

  # Convertimos en binarios: 1 si pertenece a la categoría, 0 en caso contrario
  bin_x <- as.integer(x == categoria)
  bin_y <- as.integer(y == categoria)

  # Tabla de contingencia 2x2
  tabla <- table("Antes" = bin_x, "Después" = bin_y)
  
  # Asegurar que tenga todas las combinaciones posibles
  if (!all(c("1", "0") %in% rownames(tabla))) tabla <- rbind("0" = 0, tabla)
  if (!all(c("1", "0") %in% colnames(tabla))) tabla <- cbind("0" = 0, tabla)
  tabla <- tabla[order(rownames(tabla)), order(colnames(tabla))]

  # Extraer celdas relevantes
  b <- tabla["1", "0"]
  c <- tabla["0", "1"]
  n <- b + c

  # Prueba
  if (exacto) {
    p_value <- binom.test(min(b, c), n = n, p = 0.5, alternative = "two.sided")$p.value
    metodo <- "Prueba exacta de McNemar (binomial)"
  } else {
    chi2 <- (abs(b - c))^2 / (b + c)
    p_value <- pchisq(chi2, df = 1, lower.tail = FALSE)
    metodo <- "Prueba de McNemar (chi-cuadrado)"
  }

  # Resultado
  list(
    categoria_de_interes = categoria,
    tabla_2x2 = tabla,
    cambios_de_1_a_0 = b,
    cambios_de_0_a_1 = c,
    metodo = metodo,
    p_value = p_value,
    decision = ifelse(p_value < 0.05, "Rechazar H0", "No Rechazar H0"),
    interpretacion = paste("H0: No hay cambio significativo en la categoría", shQuote(categoria))
  )
}

# -------------------------------------------------------------------
# PRUEBA CHI-CUADRADA GENERALIZADA (BONDAD DE AJUSTE E INDEPENDENCIA)
# -------------------------------------------------------------------
# x: vector categórico o data.frame con dos columnas
# p: vector con proporciones esperadas (sólo para bondad de ajuste)
# tipo: "bondad" o "independencia"
# -------------------------------------------------------------------

mi_chi2_test <- function(x, p = NULL, tipo = c("bondad", "independencia"), graficar = TRUE) {
  tipo <- match.arg(tipo)
  
  alphas <- c(0.030, 0.070, 0.025, 0.045, 0.001)
  
  if (tipo == "bondad") {
    # ----- BONDAD DE AJUSTE -----
    if (!is.factor(x)) x <- as.factor(x)
    fo <- table(x)
    k <- length(fo)

    if (is.null(p)) {
      p <- rep(1 / k, k)
    }

    if (length(p) != k) stop("Número de proporciones no coincide con las categorías.")
    if (abs(sum(p) - 1) > 0.001) {
      warning("Las proporciones no suman 1. Se normalizan.")
      p <- p / sum(p)
    }

    n <- sum(fo)
    fe <- n * p

    chi2 <- sum((fo - fe)^2 / fe)
    gl <- k - 1
    p_value <- pchisq(chi2, df = gl, lower.tail = FALSE)

    if (graficar) {
      barplot(rbind(fo, fe), beside = TRUE, col = c("steelblue", "darkorange"),
              legend.text = c("Observadas", "Esperadas"),
              args.legend = list(x = "topright"),
              main = "Bondad de ajuste: FO vs FE",
              ylab = "Frecuencia", xlab = "Categorías")
    }
    
    
    decisiones <- data.frame(
      alpha = alphas,
      decision = ifelse(p_value < alphas, "Rechazar H0", "No Rechazar H0")
    )  
    resultado <- list(
      tipo = "Bondad de ajuste",
      frecuencias_observadas = fo,
      frecuencias_esperadas = fe,
      estadistico_chi2 = chi2,
      grados_libertad = gl,
      p_value = p_value,
      decisiones = decisiones,
      interpretacion = "H0: Las proporciones observadas no difieren significativamente de las esperadas."
    )

  } else {
    # ----- INDEPENDENCIA -----
    if (!is.data.frame(x) || ncol(x) != 2) {
      stop("Para independencia, x debe ser un data.frame con dos columnas categóricas.")
    }

    v1 <- as.factor(x[[1]])
    v2 <- as.factor(x[[2]])
    tabla <- table(v1, v2)

    fe <- outer(rowSums(tabla), colSums(tabla)) / sum(tabla)
    chi2 <- sum((tabla - fe)^2 / fe)
    gl <- (nrow(tabla) - 1) * (ncol(tabla) - 1)
    p_value <- pchisq(chi2, df = gl, lower.tail = FALSE)

    if (graficar) {
      mosaicplot(tabla, color = TRUE, main = "Mosaic plot: Independencia entre variables",
                 xlab = names(x)[1], ylab = names(x)[2])
    }
    
    
    decisiones <- data.frame(
      alpha = alphas,
      decision = ifelse(p_value < alphas, "Rechazar H0", "No Rechazar H0")
    )
    resultado <- list(
      tipo = "Independencia entre variables",
      tabla_contingencia = tabla,
      frecuencias_esperadas = round(fe, 2),
      estadistico_chi2 = chi2,
      grados_libertad = gl,
      p_value = p_value,
      decisiones = decisiones,
      interpretacion = "H0: No hay relación significativa entre las variables."
    )
  }

  return(resultado)
}

# -------------------------------------------------------------------------------
# PRUEBA DE SIGNOS (versión manual con opción de rankings y visualización)
# -------------------------------------------------------------------------------
# Esta función implementa la prueba de signos para comparar dos variables ordinales
# o rankings pareados. Se utiliza cuando los datos no cumplen los supuestos de
# normalidad o cuando sólo interesa la dirección del cambio entre pares.
#
# ARGUMENTOS:
# - x, y           : vectores pareados numéricos (valores ordinales o continuos).
# - alternativa    : tipo de hipótesis alternativa. Opciones:
#                    "bilateral" → hay diferencia en cualquier dirección,
#                    "mayor"     → x tiende a ser mayor que y,
#                    "menor"     → x tiende a ser menor que y.
# - graficar       : si TRUE, muestra gráfico de barras con conteo de signos.
# - usar_rankings  : si TRUE, convierte x e y en rankings ordinales antes de comparar.
# - descendente    : si usar_rankings = TRUE, controla si el valor más alto recibe rango 1.
# - ties           : método para manejar empates en rankings ("average", "min", etc.).
# - alpha          : nivel de significancia (por defecto, 0.05).
#
# RETORNA:
# - n_pares_validos   : número de pares sin empates.
# - signos_positivos  : cantidad de diferencias positivas (x > y).
# - signos_negativos  : cantidad de diferencias negativas (x < y).
# - empates           : cantidad de pares iguales (x == y).
# - estadistico       : el menor de los conteos (+ o -), usado como estadístico de prueba.
# - p_value           : valor-p exacto de la prueba binomial.
# - decision          : "Rechazar H0" o "No Rechazar H0" según el nivel de significancia.
# - interpretacion    : descripción textual de la hipótesis nula.
# - datos_ordenados   : rankings de x e y si se activó usar_rankings = TRUE.
# -------------------------------------------------------------------------------

mi_prueba_de_signos <- function(x, y,
                                alternativa = "bilateral",
                                graficar = TRUE,
                                usar_rankings = FALSE,
                                descendente = FALSE,
                                ties = "average",
                                alpha = 0.05) {
  # Validaciones generales
  if (length(x) != length(y)) stop("Los vectores deben tener la misma longitud.")
  if (!is.numeric(x) || !is.numeric(y)) stop("Ambos vectores deben ser numéricos.")

  # Si usar_rankings es TRUE, convertir a posiciones
  if (usar_rankings) {
    x <- ranking_ordinal(x, descendente = descendente, ties = ties)
    y <- ranking_ordinal(y, descendente = descendente, ties = ties)
  } else {
    # Validar que sean enteros positivos
    if (any(x != floor(x)) || any(y != floor(y)) || any(x <= 0) || any(y <= 0)) {
      stop("Los valores deben ser enteros positivos si no se usan rankings.")
    }
  }

  # Cálculo de diferencias
  d <- x - y
  positivos <- sum(d > 0)
  negativos <- sum(d < 0)
  empates <- sum(d == 0)
  n <- positivos + negativos
  menor <- min(positivos, negativos)

  # Prueba binomial exacta
  p_value <- switch(alternativa,
                    "bilateral" = 2 * binom.test(menor, n, p = 0.5)$p.value,
                    "mayor" = binom.test(positivos, n, p = 0.5, alternative = "greater")$p.value,
                    "menor" = binom.test(positivos, n, p = 0.5, alternative = "less")$p.value,
                    stop("Alternativa no válida. Use 'mayor', 'menor' o 'bilateral'."))

  # Gráfico opcional
  if (graficar) {
    alturas <- c(positivos, negativos, empates)
    nombres <- c("+", "-", "0")
    colores <- c("steelblue", "tomato", "gray70")
    barplot(alturas, names.arg = nombres, col = colores,
            ylab = "Frecuencia", xlab = "Signo de la diferencia",
            main = "Conteo de signos (prueba de signos)")
    abline(h = 0)
  }

  # Salida
  list(
    n_pares_validos = n,
    signos_positivos = positivos,
    signos_negativos = negativos,
    empates = empates,
    estadistico = menor,
    p_value = min(p_value, 1),
    alpha = alpha,
    decision = ifelse(p_value < alpha, "Rechazar H0", "No Rechazar H0"),
    interpretacion = "H0: No hay diferencia sistemática entre las posiciones (p = 0.5)",
    datos_ordenados = if (usar_rankings) list(x_rank = x, y_rank = y) else NULL
  )
}


# -------------------------------------------------------------------
# Función auxiliar para codificar factores ordenados como enteros
# -------------------------------------------------------------------
# x: vector tipo factor (idealmente ordenado)
# Devuelve: vector numérico correspondiente a las posiciones
# -------------------------------------------------------------------

codificar_orden <- function(x) {
  if (!is.factor(x)) {
    stop("El vector no es un factor. Por favor conviértalo a factor ordenado.")
  }

  if (!is.ordered(x)) {
    warning("El factor no es ordenado. Se convertirá con el orden actual de niveles.")
    x <- factor(x, levels = unique(x), ordered = TRUE)
  }

  as.numeric(x)
}

# ------------------------------------------------------------------
# Función para generar rankings ordinales desde una variable continua
# ------------------------------------------------------------------
# x: vector numérico
# descendente: si TRUE, el valor más alto recibe rango 1
# ties: método para empates ("average", "first", "min", "max", "random")
# ------------------------------------------------------------------

ranking_ordinal <- function(x, descendente = FALSE, ties = "average") {
  if (!is.numeric(x)) stop("La variable debe ser numérica.")
  
  x_rank <- if (descendente) {
    rank(-x, ties.method = ties)
  } else {
    rank(x, ties.method = ties)
  }
  
  return(x_rank)
}



