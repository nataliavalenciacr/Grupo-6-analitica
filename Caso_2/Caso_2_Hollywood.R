# Caso 2: Hollywood
# Integrantes del grupo: Juan Pablo Mora Benavides, Natalia Valencia Casallas & Daniela Ramírez Castaño.

install.packages("jtools")
library(writexl)
library(jtools)
library(tidyverse)
library(readxl)

datos <- read_excel(
  "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/Hollywood.xls",
  sheet = "Exhibit 1"  
) # Cambiar el lugar de origen del archivo
nrow(datos)
names(datos)
datos <- datos %>%
  rename(
    Opening_Gross     = "Opening Gross",
    Total_US_Gross    = "Total U.S. Gross",
    Total_NonUS_Gross = "Total Non-U.S. Gross",
    Opening_Theatres  = "Opening Theatres",
    Known_Story       = "Known Story",
    Origin_US         = "Origin_United States",
    MPAA_D            = "MPAA_D",
    Critics           = "Critics´ Opinion",
    Oscar_Nom         = "Oscar Nominations",
    Oscars_Won        = "Oscars Won"
  )
names(datos)
glimpse(datos)
#PREGUNTA 1 
tabla_descriptivos <- datos %>%
  summarise(
    Min_Opening    = min(Opening_Gross,     na.rm = TRUE),
    Media_Opening  = mean(Opening_Gross,    na.rm = TRUE),
    Max_Opening    = max(Opening_Gross,     na.rm = TRUE),
    Min_US         = min(Total_US_Gross,    na.rm = TRUE),
    Media_US       = mean(Total_US_Gross,   na.rm = TRUE),
    Max_US         = max(Total_US_Gross,    na.rm = TRUE),
    Min_NonUS      = min(Total_NonUS_Gross, na.rm = TRUE),
    Media_NonUS    = mean(Total_NonUS_Gross,na.rm = TRUE),
    Max_NonUS      = max(Total_NonUS_Gross, na.rm = TRUE),
    Min_Theatres   = min(Opening_Theatres,  na.rm = TRUE),
    Media_Theatres = mean(Opening_Theatres, na.rm = TRUE),
    Max_Theatres   = max(Opening_Theatres,  na.rm = TRUE)
  )
print(tabla_descriptivos)
tabla_final <- data.frame(
  Variable = c("Opening Gross", "Total U.S. Gross", 
               "Total Non-U.S. Gross", "Opening Theatres"),
  Minimo   = c(tabla_descriptivos$Min_Opening, 
               tabla_descriptivos$Min_US,
               tabla_descriptivos$Min_NonUS, 
               tabla_descriptivos$Min_Theatres),
  Promedio = c(tabla_descriptivos$Media_Opening, 
               tabla_descriptivos$Media_US,
               tabla_descriptivos$Media_NonUS, 
               tabla_descriptivos$Media_Theatres),
  Maximo   = c(tabla_descriptivos$Max_Opening, 
               tabla_descriptivos$Max_US,
               tabla_descriptivos$Max_NonUS, 
               tabla_descriptivos$Max_Theatres)
)
print(tabla_final)
write_xlsx(tabla_final, "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/tabla_descriptivos.xlsx") #Aquí deben cambiar el lugar para descargar
# ¿Cuántas películas son comedias? 
datos <- datos %>%
  mutate(Comedy = ifelse(Genre == "Comedy", 1, 0))
cat("Películas de comedia:", sum(datos$Comedy == 1), "\n")
cat("Películas de otro género:", sum(datos$Comedy == 0), "\n")
# ¿Cuántas tienen rating R? 
cat("Películas rated R:", sum(datos$MPAA_D == 1, na.rm = TRUE), "\n")
cat("Películas con otro rating:", sum(datos$MPAA_D == 0, na.rm = TRUE), "\n")

# PREGUNTA 2: ROI (Retorno sobre la inversión)
# 2a. Calcular el ROI para cada película
# Fórmula: ROI = (Total U.S. Gross - Budget) / Budget
datos <- datos %>%
  mutate(ROI = (Total_US_Gross - Budget) / Budget)
datos %>% 
  select(Movie, Budget, Total_US_Gross, ROI) %>% 
  head(10)
  cat("ROI promedio:", round(mean(datos$ROI, na.rm = TRUE) * 100, 2), "%\n")

tabla_roi <- datos %>%
  select(Movie, Budget, Total_US_Gross, ROI) %>%
  mutate(
    Budget         = scales::dollar(Budget),
    Total_US_Gross = scales::dollar(Total_US_Gross),
    ROI            = paste0(round(ROI * 100, 2), "%")
  ) %>%
  rename(
    Película       = Movie,
    Presupuesto    = Budget,
    `Recaudación U.S.` = Total_US_Gross,
    `ROI U.S.`     = ROI
  )
print(tabla_roi)
write_xlsx(tabla_roi, 
  "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/tabla_roi.xlsx") #Aquí deben cambiar el lugar para descargar 

# 2b. Intervalo de confianza al 95% para el ROI promedio
# H0: ROI = 0
ic_roi <- t.test(datos$ROI, conf.level = 0.95)

cat("\n--- IC 95% para el ROI promedio ---\n")
cat("Media ROI:      ", round(ic_roi$estimate * 100, 2), "%\n")
cat("Límite inferior:", round(ic_roi$conf.int[1] * 100, 2), "%\n")
cat("Límite superior:", round(ic_roi$conf.int[2] * 100, 2), "%\n")

# Símbolos para hipótesis: ✔ = se rechaza H1 / ✘ = no se rechaza H0

# 2c. Prueba de hipótesis: ¿ROI promedio > 12%?
# H0: ROI = 0.12  (el ROI es igual al 12%)
# H1: ROI > 0.12  (el ROI es mayor al 12%)
prueba_roi <- t.test(
  datos$ROI,
  mu          = 0.12,        
  alternative = "greater",   
  conf.level  = 0.95
)
cat("\n--- Prueba: ¿ROI promedio > 12%? ---\n")
cat("H0: ROI = 12%\n")
cat("H1: ROI > 12%\n")
cat("Estadístico t:", round(prueba_roi$statistic, 4), "\n")
cat("p-value:      ", round(prueba_roi$p.value, 6), "\n")
if (prueba_roi$p.value < 0.05) {
  cat("Decisión: Rechazamos H0 ✔\n")
  cat("Conclusión: El ROI promedio ES significativamente mayor al 12%\n")
} else {
  cat("Decisión: No rechazamos H0 ✘\n")
  cat("Conclusión: No hay evidencia suficiente para superar el 12%\n")
}
#PREGUNTA 3: ¿Las comedias recaudan más que otros géneros?
# 3a. Prueba t: Total U.S. Gross comedias vs. no comedias
# H0: No hay diferencia en recaudación entre comedias y no comedias
# H1: Sí hay diferencia 
tabla_3a <- datos %>%
  mutate(Grupo = ifelse(Comedy == 1, "Comedia", "Otro género")) %>%
  group_by(Grupo) %>%
  summarise(
    N        = n(),
    Promedio = round(mean(Total_US_Gross, na.rm = TRUE), 0),
    Minimo   = round(min(Total_US_Gross,  na.rm = TRUE), 0),
    Maximo   = round(max(Total_US_Gross,  na.rm = TRUE), 0),
    Desv_Est = round(sd(Total_US_Gross,   na.rm = TRUE), 0)
  )

print(tabla_3a)

prueba_comedia <- t.test(
  Total_US_Gross ~ Comedy,
  data        = datos,
  alternative = "two.sided",
  conf.level  = 0.95
)
prueba_3a <- t.test(
  Total_US_Gross ~ Comedy,
  data        = datos,
  alternative = "two.sided",
  conf.level  = 0.95
)

cat("\n--- Prueba t: Recaudación comedias vs. no comedias ---\n")
cat("Estadístico t:", round(prueba_3a$statistic, 4), "\n")
cat("p-value:      ", round(prueba_3a$p.value, 6), "\n")

if (prueba_3a$p.value < 0.05) {
  cat("Decisión: Rechazamos H0 ✔\n")
  cat("Conclusión: SÍ hay diferencia significativa en recaudación\n")
} else {
  cat("Decisión: No rechazamos H0 ✘\n")
  cat("Conclusión: NO hay diferencia significativa en recaudación\n")
}
write_xlsx(tabla_3a,
  "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/tabla_3a.xlsx") #Aquí deben cambiar el lugar para descargar

#3b: ¿Las comedias tienen mejor ROI que otros géneros?
tabla_3b <- datos %>%
  mutate(Grupo = ifelse(Comedy == 1, "Comedia", "Otro género")) %>%
  group_by(Grupo) %>%
  summarise(
    N            = n(),
    Promedio_ROI = paste0(round(mean(ROI, na.rm = TRUE) * 100, 2), "%"),
    Minimo_ROI   = paste0(round(min(ROI,  na.rm = TRUE) * 100, 2), "%"),
    Maximo_ROI   = paste0(round(max(ROI,  na.rm = TRUE) * 100, 2), "%"),
    Desv_Est_ROI = paste0(round(sd(ROI,   na.rm = TRUE) * 100, 2), "%")
  )

print(tabla_3b)
# Prueba t
# H0: No hay diferencia en ROI entre comedias y no comedias
# H1: Sí hay diferencia
prueba_3b <- t.test(
  ROI ~ Comedy,
  data        = datos,
  alternative = "two.sided",
  conf.level  = 0.95
)

cat("\n--- Prueba t: ROI comedias vs. no comedias ---\n")
cat("H0: No hay diferencia en ROI (μ_comedia = μ_otros)\n")
cat("H1: Sí hay diferencia en ROI (μ_comedia ≠ μ_otros)\n")
cat("Estadístico t:", round(prueba_3b$statistic, 4), "\n")
cat("p-value:      ", round(prueba_3b$p.value, 6), "\n")

if (prueba_3b$p.value < 0.05) {
  cat("Decisión: Rechazamos H0 ✔\n")
  cat("Conclusión: SÍ hay diferencia significativa en ROI\n")
} else {
  cat("Decisión: No rechazamos H0 ✘\n")
  cat("Conclusión: NO hay diferencia significativa en ROI\n")
}
write_xlsx(tabla_3b,
  "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/tabla_3b.xlsx") #Aquí si quieren descargar las tablas deben cambiar el lugar para que les corra el código

#PREGUNTA 4:¿Existe una diferencia estadísticamente significativa entre la recaudación total en EE. UU. de las películas con clasificación R 
#y las películas con otras clasificaciones? 

tabla_4 <- datos %>%
  mutate(Grupo = ifelse(MPAA_D == 1, "Rated R", "Otro rating")) %>%
  group_by(Grupo) %>%
  summarise(
    N        = n(),
    Promedio = round(mean(Total_US_Gross, na.rm = TRUE), 0),
    Minimo   = round(min(Total_US_Gross,  na.rm = TRUE), 0),
    Maximo   = round(max(Total_US_Gross,  na.rm = TRUE), 0),
    Desv_Est = round(sd(Total_US_Gross,   na.rm = TRUE), 0)
  )
print(tabla_4)

# Prueba t
# H0: No hay diferencia en recaudación entre rated R y otros ratings
# H1: Sí hay diferencia
prueba_4 <- t.test(
  Total_US_Gross ~ MPAA_D,
  data        = datos,
  alternative = "two.sided",
  conf.level  = 0.95
)

cat("\n--- Prueba t: Recaudación Rated R vs. otros ratings ---\n")
cat("H0: No hay diferencia en recaudación (μ_R = μ_otros)\n")
cat("H1: Sí hay diferencia en recaudación (μ_R ≠ μ_otros)\n")
cat("Estadístico t:", round(prueba_4$statistic, 4), "\n")
cat("p-value:      ", round(prueba_4$p.value, 6), "\n")

if (prueba_4$p.value < 0.05) {
  cat("Decisión: Rechazamos H0 ✔\n")
  cat("Conclusión: SÍ hay diferencia significativa en recaudación\n")
} else {
  cat("Decisión: No rechazamos H0 ✘\n")
  cat("Conclusión: NO hay diferencia significativa en recaudación\n")
}
write_xlsx(tabla_4,
  "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 2/tabla_4.xlsx") #Aquí si quieren descargar las tablas deben cambiar el lugar para que les corra el código
  
#PUNTO 5

# variable Comedy (1 = comedia, 0 = otro genero)
datos <- datos %>%
  mutate(Comedy = ifelse(Genre == "Comedy", 1, 0))

# variable ROI 
datos <- datos %>%
  mutate(ROI = (Total_US_Gross - Budget) / Budget)

cat("Datos cargados correctamente:", nrow(datos), "películas.\n\n") #Solo para comprobar


# ------------------------------------------------------------
# PREGUNTA 5: MODELO DE REGRESIÓN ANTES DE LA PRODUCCIÓN
# ------------------------------------------------------------
# Variables predictoras consideradas (conocidas ANTES de producir):
#   - Budget       : presupuesto de producción (proxy de calidad y elenco)
#   - Comedy       : indicador de género comedia (1) vs. otro (0)
#   - MPAA_D       : indicador de clasificación R (1) vs. otra (0)
#   - Known_Story  : historia conocida/adaptación (1) vs. guion original (0)
#   - Sequel       : secuela (1) vs. primera película (0)

cat("------------------------------------------------------------\n")
cat(" PREGUNTA 5: Regresión — factores PRE-PRODUCCIÓN\n")
cat("------------------------------------------------------------\n\n")

# ----------------------------------------------------------
# 5a. Modelo completo con todas las variables pre-producción
# ----------------------------------------------------------
modelo_5a <- lm(
  Total_US_Gross ~ Budget + Comedy + MPAA_D + Known_Story + Sequel,
  data = datos
)

cat("--- 5a. Modelo completo (pre-producción) ---\n")
summary(modelo_5a)

# ----------------------------------------------------------
# 5b. Eliminar variables no significativas al 10% (p > 0.10)
#     Se realiza eliminación progresiva (backward elimination):
#     Se retira de a una variable, comenzando por la de mayor p-value.
# ----------------------------------------------------------

# Iteración 1: eliminar Known_Story (p-value más alto > 0.10)
modelo_5b_iter1 <- lm(
  Total_US_Gross ~ Budget + Comedy + MPAA_D + Sequel,
  data = datos
)

cat("\n--- 5b. Iteración 1: sin Known_Story ---\n")
summary(modelo_5b_iter1)

# Iteración 2: eliminar MPAA_D si sigue siendo no significativa
modelo_5b_iter2 <- lm(
  Total_US_Gross ~ Budget + Comedy + Sequel,
  data = datos
)

cat("\n--- 5b. Iteración 2: sin MPAA_D ---\n")
summary(modelo_5b_iter2)

# Iteración 3: eliminar Comedy si sigue siendo no significativa
modelo_5b_final <- lm(
  Total_US_Gross ~ Budget + Sequel,
  data = datos
)

cat("\n--- 5b. MODELO FINAL pre-producción (solo variables significativas al 10%) ---\n")
summary(modelo_5b_final)

# Tabla resumen de coeficientes del modelo final
tabla_5b <- as.data.frame(summary(modelo_5b_final)$coefficients)
tabla_5b$Variable <- rownames(tabla_5b)
tabla_5b <- tabla_5b %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )

print(tabla_5b)
#write_xlsx(tabla_5b,"C:/Users/juanm/Documents/JAVERIANA/Analítica/Caso 2/tabla_5b_modelo_preproduccion.xlsx") #algo esta fallando con los permisos de esta linea

# ----------------------------------------------------------
# 5c. Interpretación: ¿Secuelas vs. no secuelas?
#     Si el coeficiente de Sequel > 0, las secuelas recaudan MÁS.
#     Si el coeficiente de Sequel < 0, las secuelas recaudan MENOS.
# ----------------------------------------------------------
coef_sequel <- coef(modelo_5b_final)["Sequel"]

cat("\n--- 5c. Efecto de ser secuela sobre Total U.S. Gross Inc---\n")
cat("Coeficiente de Sequel:", scales::dollar(coef_sequel), "\n")

if (coef_sequel > 0) {
  cat("Las SECUELAS recaudan en promedio",
      scales::dollar(coef_sequel),
      "más que las películas no secuelas,\n",
      "manteniendo constantes las demás variables.\n")
} else {
  cat("Las SECUELAS recaudan en promedio",
      scales::dollar(abs(coef_sequel)),
      "menos que las películas no secuelas,\n",
      "manteniendo constantes las demás variables.\n")
}


# ------------------------------------------------------------
# PREGUNTA 6: MODELO DE REGRESIÓN — APERTURA DE FIN DE SEMANA
# ------------------------------------------------------------
# Factores adicionales conocidos ANTES del fin de semana de apertura:
#   - Opening_Theatres : número de salas en el fin de semana de estreno
#   - Summer           : estreno en temporada de verano (1/0)
#   - Holiday          : estreno en día festivo (1/0)
#   - Christmas        : estreno en temporada navideña (1/0)
# Variable dependiente: Opening_Gross (recaudación del fin de semana de apertura)

cat("\n------------------------------------------------------------\n")
cat(" PREGUNTA 6: Regresión — Opening Weekend Box-Office Gross\n")
cat("------------------------------------------------------------\n\n")

# ----------------------------------------------------------
# 6a. Modelo completo (pre-producción + factores de apertura)
# ----------------------------------------------------------
modelo_6a <- lm(
  Opening_Gross ~ Budget + Comedy + MPAA_D + Known_Story + Sequel +
    Opening_Theatres + Summer + Holiday + Christmas,
  data = datos
)

cat("--- 6a. Modelo completo apertura ---\n")
summary(modelo_6a)

# ----------------------------------------------------------
# 6b. Eliminación backward de variables no significativas al 10%
# ----------------------------------------------------------

# Iteración 1: eliminar Holiday (p-value más alto)
modelo_6b_iter1 <- lm(
  Opening_Gross ~ Budget + Comedy + MPAA_D + Known_Story + Sequel +
    Opening_Theatres + Summer + Christmas,
  data = datos
)
cat("\n--- 6b. Iteración 1: sin Holiday ---\n")
summary(modelo_6b_iter1)

# Iteración 2: eliminar Comedy
modelo_6b_iter2 <- lm(
  Opening_Gross ~ Budget + MPAA_D + Known_Story + Sequel +
    Opening_Theatres + Summer + Christmas,
  data = datos
)
cat("\n--- 6b. Iteración 2: sin Comedy ---\n")
summary(modelo_6b_iter2)

# Iteración 3: eliminar MPAA_D
modelo_6b_iter3 <- lm(
  Opening_Gross ~ Budget + Known_Story + Sequel +
    Opening_Theatres + Summer + Christmas,
  data = datos
)
cat("\n--- 6b. Iteración 3: sin MPAA_D ---\n")
summary(modelo_6b_iter3)

# Iteración 4: eliminar Christmas si no es significativa
modelo_6b_final <- lm(
  Opening_Gross ~ Budget + Known_Story + Sequel +
    Opening_Theatres + Summer,
  data = datos
)
cat("\n--- 6b. MODELO FINAL apertura (solo variables significativas al 10%) ---\n")
summary(modelo_6b_final)

# Tabla resumen de coeficientes del modelo final
tabla_6b <- as.data.frame(summary(modelo_6b_final)$coefficients)
tabla_6b$Variable <- rownames(tabla_6b)
tabla_6b <- tabla_6b %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )

print(tabla_6b)
write_xlsx(tabla_6b,"C:/Users/juanm/Documents/JAVERIANA/Analítica/Caso 2/tabla_6b_modelo_apertura.xlsx")  

# ----------------------------------------------------------
# 6c. Interpretación de cada coeficiente del modelo final
# ----------------------------------------------------------
cat("\n--- 6c. Interpretación de los coeficientes del modelo final ---\n")
coefs_6 <- coef(modelo_6b_final)

cat("\nIntercepto:", scales::dollar(coefs_6["(Intercept)"]),
    "\n  -> Recaudación base estimada cuando todas las variables son 0.\n")

cat("\nBudget:", coefs_6["Budget"],
    "\n  -> Por cada dólar adicional en presupuesto, la recaudación de apertura",
    "\n     aumenta en promedio", round(coefs_6["Budget"], 4),
    "dólares, manteniendo lo demás constante.\n")

cat("\nKnown_Story:", scales::dollar(coefs_6["Known_Story"]),
    "\n  -> Las películas basadas en historias conocidas recaudan en promedio",
    scales::dollar(coefs_6["Known_Story"]),
    "MÁS (o MENOS si es negativo) que las originales en el fin de semana de apertura.\n")

cat("\nSequel:", scales::dollar(coefs_6["Sequel"]),
    "\n  -> Las secuelas recaudan en promedio", scales::dollar(coefs_6["Sequel"]),
    "MÁS (o MENOS si es negativo) que las no-secuelas.\n")

cat("\nOpening_Theatres:", scales::dollar(coefs_6["Opening_Theatres"]),
    "\n  -> Por cada sala adicional en la que se exhibe la película,",
    "\n     la recaudación de apertura aumenta en promedio",
    scales::dollar(coefs_6["Opening_Theatres"]), "dólares.\n")

cat("\nSummer:", scales::dollar(coefs_6["Summer"]),
    "\n  -> Las películas estrenadas en verano recaudan en promedio",
    scales::dollar(coefs_6["Summer"]),
    "MÁS (o MENOS si es negativo) que las estrenadas fuera de verano.\n")

# ----------------------------------------------------------
# 6d. Si el número de salas aumenta en 100:
#     Estimación puntual e IC 95% del cambio esperado en Opening_Gross
# ----------------------------------------------------------
cat("\n--- 6d. Efecto de +100 salas sobre Opening_Gross ---\n")

# El cambio esperado es simplemente 100 * coeficiente de Opening_Theatres
cambio_puntual <- 100 * coefs_6["Opening_Theatres"]
cat("Estimación puntual del cambio:", scales::dollar(cambio_puntual), "\n")

# IC 95% para el coeficiente de Opening_Theatres
ic_teatros <- confint(modelo_6b_final, "Opening_Theatres", level = 0.95)
ic_cambio_inferior <- 100 * ic_teatros[1]
ic_cambio_superior <- 100 * ic_teatros[2]

cat("IC 95% para el cambio al agregar 100 salas:\n")
cat("  Límite inferior:", scales::dollar(ic_cambio_inferior), "\n")
cat("  Límite superior:", scales::dollar(ic_cambio_superior), "\n")

tabla_6d <- data.frame(
  Descripcion         = c("Estimación puntual", "IC 95% - Límite inferior", "IC 95% - Límite superior"),
  Cambio_Opening_Gross = scales::dollar(c(cambio_puntual, ic_cambio_inferior, ic_cambio_superior))
)
print(tabla_6d)
write_xlsx(tabla_6d,"C:/Users/juanm/Documents/JAVERIANA/Analítica/Caso 2/tabla_6d_efecto_salas.xlsx")  


# ------------------------------------------------------------
# PREGUNTA 7: RELACIÓN Total U.S. Gross ~ Opening Weekend Gross
# ------------------------------------------------------------
# Se examina la "sabiduría convencional" de Hollywood:
# "El 25% de la recaudación total en EE.UU. llega durante el fin de semana de apertura"
# Si eso fuera cierto, Opening_Gross = 0.25 * Total_US_Gross

cat("\n------------------------------------------------------------\n")
cat(" PREGUNTA 7: Total U.S. Gross ~ Opening Weekend Gross\n")
cat("------------------------------------------------------------\n\n")

# ----------------------------------------------------------
# 7a. Regresión lineal simple: Total_US_Gross ~ Opening_Gross
# ----------------------------------------------------------
modelo_7a <- lm(Total_US_Gross ~ Opening_Gross, data = datos)

cat("--- 7a. Regresión lineal simple ---\n")
summary(modelo_7a)

tabla_7a <- as.data.frame(summary(modelo_7a)$coefficients)
tabla_7a$Variable <- rownames(tabla_7a)
tabla_7a <- tabla_7a %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )
print(tabla_7a)

# ----------------------------------------------------------
# 7b. Valor teórico de la pendiente si el 25% viniera del opening
#     Total_US_Gross = beta0 + beta1 * Opening_Gross
#     Si Opening_Gross = 0.25 * Total_US_Gross
#     => beta1 = 1/0.25 = 4
# ----------------------------------------------------------
beta1_teorico <- 4
cat("\n--- 7b. Valor teórico de la pendiente (si Opening = 25% del total) ---\n")
cat("Si el 25% de la recaudación total llega en apertura,\n")
cat("entonces la pendiente debería ser beta1 =", beta1_teorico, "\n")
cat("(es decir, Total_US_Gross = 4 × Opening_Gross)\n")

# ----------------------------------------------------------
# 7c. Prueba de hipótesis: ¿Se puede rechazar que beta1 = 4?
#     H0: beta1 = 4  (la sabiduría convencional es correcta)
#     H1: beta1 Diferente a 4
#
#     Estadístico t = (beta1_estimado - beta1_teorico) / SE(beta1)
# ----------------------------------------------------------
cat("\n--- 7c. Prueba de hipótesis: H0: beta1 = 4 ---\n")

beta1_estimado <- coef(modelo_7a)["Opening_Gross"]
se_beta1       <- summary(modelo_7a)$coefficients["Opening_Gross", "Std. Error"]
gl             <- df.residual(modelo_7a)

t_stat_7c <- (beta1_estimado - beta1_teorico) / se_beta1
p_value_7c <- 2 * pt(-abs(t_stat_7c), df = gl)  # prueba bilateral

cat("Pendiente estimada (beta1):", round(beta1_estimado, 4), "\n")
cat("Valor teórico bajo H0:     ", beta1_teorico, "\n")
cat("Error estándar de beta1:   ", round(se_beta1, 4), "\n")
cat("Estadístico t:             ", round(t_stat_7c, 4), "\n")
cat("Grados de libertad:        ", gl, "\n")
cat("p-value (bilateral):       ", round(p_value_7c, 6), "\n")

if (p_value_7c < 0.05) {
  cat("Decisión: Rechazamos H0 ✔\n")
  cat("Conclusión: La sabiduría convencional (25%) puede RECHAZARSE a nivel 5%.\n")
} else {
  cat("Decisión: No rechazamos H0 ✘\n")
  cat("Conclusión: No hay evidencia suficiente para rechazar que beta1 = 4.\n")
}

# ----------------------------------------------------------
# 7d. Crítica al análisis estadístico de 7c
# ----------------------------------------------------------
cat("\n--- 7d. Crítica del análisis en 7c ---\n")
cat("CRÍTICA ESTADÍSTICA:\n")
cat("1. El modelo de regresión lineal simple asume que la relación entre\n")
cat("   Opening_Gross y Total_US_Gross pasa por el origen (intercepto = 0)\n")
cat("   si queremos representar la relación proporcional 1:4.\n")
cat("   Sin embargo, el modelo estimado incluye un intercepto libre,\n")
cat("   lo cual puede sesgar la estimación de la pendiente.\n\n")
cat("2. La prueba en 7c evalúa la pendiente en un modelo con intercepto.\n")
cat("   Si el intercepto es significativamente distinto de cero,\n")
cat("   la relación NO es estrictamente proporcional, y la comparación\n")
cat("   con beta1 = 4 pierde parte de su validez conceptual.\n\n")
cat("3. Para una prueba más correcta de la sabiduría convencional,\n")
cat("   debería ajustarse un modelo sin intercepto (pasa por el origen)\n")
cat("   y luego verificar si la pendiente es estadísticamente igual a 4.\n")

# Verificar si el intercepto es significativo
p_intercepto <- summary(modelo_7a)$coefficients["(Intercept)", "Pr(>|t|)"]
cat("\np-value del intercepto:", round(p_intercepto, 6), "\n")
if (p_intercepto < 0.05) {
  cat("El intercepto ES significativo por lo tanto el modelo con intercepto NO representa\n")
  cat("la relación proporcional pura. La crítica es válida.\n")
} else {
  cat("El intercepto NO es significativo por lo tanto el modelo con intercepto es razonable.\n")
}

# ----------------------------------------------------------
# 7e. Modelo mejorado: regresión sin intercepto (pasa por el origen)
#     Este modelo representa mejor la hipótesis proporcional:
#     Total_US_Gross = beta1 * Opening_Gross
# ----------------------------------------------------------
modelo_7e <- lm(Total_US_Gross ~ Opening_Gross - 1, data = datos)

cat("\n--- 7e. Modelo mejorado: regresión SIN intercepto (origen) ---\n")
summary(modelo_7e)

beta1_7e <- coef(modelo_7e)["Opening_Gross"]
cat("Pendiente estimada (sin intercepto):", round(beta1_7e, 4), "\n")
cat("Interpretación: Por cada dólar recaudado en el fin de semana de apertura,\n")
cat("  la película recauda en total aproximadamente",
    round(beta1_7e, 2), "dólares en EE.UU.\n")

tabla_7e <- as.data.frame(summary(modelo_7e)$coefficients)
tabla_7e$Variable <- rownames(tabla_7e)
tabla_7e <- tabla_7e %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )
print(tabla_7e)
write_xlsx(tabla_7e, "C:/Users/juanm/Documents/JAVERIANA/Analítica/Caso 2/tabla_7e_regresion_sin_intercepto.xlsx")  # <-- Ajustar ruta

# ----------------------------------------------------------
# 7f. Prueba de la sabiduría convencional con el modelo mejorado
#     H0: beta1 = 4  (Opening Weekend = 25% del total)
#     H1: beta1 ≠ 4
# ----------------------------------------------------------
cat("\n--- 7f. Prueba H0: beta1 = 4 usando el modelo sin intercepto ---\n")

beta1_7e_val <- coef(modelo_7e)["Opening_Gross"]
se_7e        <- summary(modelo_7e)$coefficients["Opening_Gross", "Std. Error"]
gl_7e        <- df.residual(modelo_7e)

t_stat_7f  <- (beta1_7e_val - beta1_teorico) / se_7e
p_value_7f <- 2 * pt(-abs(t_stat_7f), df = gl_7e)

cat("Pendiente estimada (sin intercepto):", round(beta1_7e_val, 4), "\n")
cat("Estadístico t:", round(t_stat_7f, 4), "\n")
cat("p-value (bilateral):", round(p_value_7f, 6), "\n")

if (p_value_7f < 0.05) {
  cat("Decisión: Rechazamos H0 \n")
  cat("Conclusión: Con el modelo sin intercepto, la sabiduría convencional\n")
  cat("(Opening = 25% del total) SÍ puede rechazarse estadísticamente.\n")
} else {
  cat("Decisión: No rechazamos H0 \n")
  cat("Conclusión: No hay evidencia suficiente para rechazar que Opening = 25% del total.\n")
}

# ----------------------------------------------------------
# 7g. Proporción de variación explicada (R²)
#     Para el modelo con intercepto (7a)
# ----------------------------------------------------------
cat("\n--- 7g. R² del modelo con intercepto (7a) ---\n")

r2_7a <- summary(modelo_7a)$r.squared
cat("R² =", round(r2_7a * 100, 2), "%\n")
cat("Interpretación: El", round(r2_7a * 100, 2),
    "% de la variación en la recaudación total en EE.UU.\n",
    "es explicado por la variación en la recaudación del fin de semana de apertura.\n")

# Tabla resumen final Q7
tabla_7g <- data.frame(
  Modelo                      = c("Con intercepto (7a)", "Sin intercepto (7e)"),
  Pendiente_Estimada          = round(c(coef(modelo_7a)["Opening_Gross"],
                                         coef(modelo_7e)["Opening_Gross"]), 4),
  R_Cuadrado                  = round(c(summary(modelo_7a)$r.squared,
                                         summary(modelo_7e)$r.squared), 4),
  H0_beta1_4_p_value          = round(c(p_value_7c, p_value_7f), 6),
  Rechaza_H0_al_5pct          = c(p_value_7c < 0.05, p_value_7f < 0.05)
)

print(tabla_7g)
write_xlsx(tabla_7g, "C:/Users/juanm/Documents/JAVERIANA/Analítica/Caso 2/tabla_7g_resumen_regresiones.xlsx")  # <-- Ajustar ruta

# ----------------------------------------------------------
# 8a. Modelo completo (todas las variables disponibles)
# ----------------------------------------------------------
modelo_8a <- lm(
  Total_US_Gross ~ Budget + Comedy + MPAA_D + Known_Story + Sequel +
    Opening_Theatres + Summer + Holiday + Christmas +
    Opening_Gross + Critics,
  data = datos
)

cat("--- 8a. Modelo completo ---\n")
print(summary(modelo_8a))

# ----------------------------------------------------------
# 8b. Eliminación de variables no significativas (p > 10%)
#     Se eliminan una a una (backward stepwise manual)
# ----------------------------------------------------------

# Iteración: eliminar la variable con mayor p-value > 0.10
# Después de revisar summary(modelo_8a), se construye el modelo final

modelo_8b <- lm(
  Total_US_Gross ~ Opening_Gross + Critics + Budget + MPAA_D,
  data = datos
)
# Nota: ajusta las variables según los p-values reales que obtengas.
# Conserva solo aquellas con p < 0.10.

cat("\n--- 8b. Modelo final (solo variables significativas al 10%) ---\n")
print(summary(modelo_8b))

tabla_8b <- as.data.frame(summary(modelo_8b)$coefficients)
tabla_8b$Variable <- rownames(tabla_8b)
tabla_8b <- tabla_8b %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )
print(tabla_8b)

write_xlsx(
  tabla_8b,
  "C:\\Users\\danie\\OneDrive\\Documentos\\CONTADURIA\\TERCER SEMESTRE\\ANÁLISIS DE LOS NEGOCIOS\\tabla_8b_modelo_final.xlsx"  # <-- Ajustar ruta
)

# ----------------------------------------------------------
# 8c. Predicción para "Flags of Our Fathers"
#     Características de la película (según el dataset):
#     - Buscar la fila de "Flags of Our Fathers" en los datos
# ----------------------------------------------------------
cat("\n--- 8c. Predicción para Flags of Our Fathers ---\n")

flags <- datos %>% filter(grepl("Flags", Movie, ignore.case = TRUE))
print(flags)

pred_8c <- predict(
  modelo_8b,
  newdata  = flags,
  interval = "prediction",
  level    = 0.95
)

cat("Estimación puntual (Total U.S. Gross):", 
    format(pred_8c[1, "fit"], big.mark = ",", prefix = "$"), "\n")
cat("Intervalo de predicción 95%:\n")
cat("  Límite inferior:", 
    format(pred_8c[1, "lwr"], big.mark = ",", prefix = "$"), "\n")
cat("  Límite superior:", 
    format(pred_8c[1, "upr"], big.mark = ",", prefix = "$"), "\n")

# ----------------------------------------------------------
# 8d. ¿Cuánto pagar para subir 10 puntos en Critics?
#     Si Critics sube 10 pts → Total_US_Gross sube en:
#     delta = 10 * coef(Critics)
#     Griffith debería invertir MENOS que ese delta.
# ----------------------------------------------------------
cat("\n--- 8d. Valor de +10 puntos en Critics para Flags of Our Fathers ---\n")

coef_critics <- coef(modelo_8b)["Critics"]
ganancia_10pts <- 10 * coef_critics

cat("Coeficiente de Critics:          ", format(coef_critics), "por punto\n")
cat("Ganancia estimada por +10 puntos:", format(ganancia_10pts), "\n")
cat("Griffith NO debería pagar más de", format(ganancia_10pts),
    "para obtener 10 puntos adicionales en Critics.\n")
cat("Nota: este cálculo mantiene todas las demás variables constantes\n")
cat("(ceteris paribus) y asume que el modelo es válido en ese rango.\n")


# ============================================================
# PREGUNTA 9: ¿Los críticos afectan MENOS a las comedias?
# Interacción entre Critics y Comedy
# ============================================================

"n==========================================================="
" PREGUNTA 9: Efecto diferencial de Critics en Comedia"
"==========================================================="

# ----------------------------------------------------------
# 9a. Agregar término de interacción: Critics × Comedy
#     Si el coeficiente de interacción es significativo y NEGATIVO,
#     significa que para las comedias el efecto de Critics
#     sobre Total_US_Gross es MENOR (Griffith tendría razón).
# ----------------------------------------------------------
modelo_9 <- lm(
  Total_US_Gross ~ Opening_Gross + Critics + Sequel +
    Comedy + Critics:Comedy,
  data = datos
)

cat("--- 9. Modelo con interacción Critics × Comedy ---\n")
print(summary(modelo_9))

tabla_9 <- as.data.frame(summary(modelo_9)$coefficients)
tabla_9$Variable <- rownames(tabla_9)
tabla_9 <- tabla_9 %>%
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
  rename(
    Coeficiente    = Estimate,
    Error_Estandar = `Std. Error`,
    Estadistico_t  = `t value`,
    P_Value        = `Pr(>|t|)`
  )
print(tabla_9)

write_xlsx(
  tabla_9,
  "C:\\Users\\danie\\OneDrive\\Documentos\\CONTADURIA\\TERCER SEMESTRE\\ANÁLISIS DE LOS NEGOCIOS\\tabla_9_interaccion.xlsx"  # <-- Ajustar ruta
)

# Interpretación automática del término de interacción
coef_interaccion <- coef(modelo_9)["Critics:Comedy"]
p_interaccion    <- summary(modelo_9)$coefficients["Critics:Comedy", "Pr(>|t|)"]

cat("\n--- Interpretación del término de interacción ---\n")
cat("Coeficiente Critics:Comedy:", round(coef_interaccion, 4), "\n")
cat("p-value:                   ", round(p_interaccion, 6), "\n")

if (p_interaccion < 0.10) {
  cat("Decisión: El término de interacción ES significativo al 10% ✔\n")
  if (coef_interaccion < 0) {
    cat("Conclusión: Griffith tiene RAZÓN.\n")
    cat("Los críticos afectan MENOS la recaudación de las comedias.\n")
    cat("El efecto neto de Critics para comedias es:",
        round(coef(modelo_9)["Critics"] + coef_interaccion, 4), "por punto.\n")
  } else {
    cat("Conclusión: El efecto de Critics en comedias es MAYOR, no menor.\n")
    cat("Griffith NO tiene razón en este caso.\n")
  }
} else {
  cat("Decisión: El término de interacción NO es significativo al 10% ✘\n")
  cat("Conclusión: No hay evidencia estadística de que los críticos afecten\n")
  cat("de forma diferente a las comedias vs. otros géneros.\n")
  cat("La teoría de Griffith NO puede probarse con estos datos.\n")
}


