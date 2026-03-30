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
  
