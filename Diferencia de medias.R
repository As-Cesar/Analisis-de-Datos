base_hipo <- base %>%
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


base_hipo %>% summary()

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
