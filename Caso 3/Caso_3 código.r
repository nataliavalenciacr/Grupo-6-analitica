##############################################################
# Taller de Business Analytics - Caso Tour Marketing (Opry) 
# Objetivo: Explorar la relación entre ventas y publicidad
# Autor: Juan Nicolás Velásquez Rey
# Grupo: Juan Pablo Mora Benavides, Natalia Valencia Casallas & Daniela Ramírez Castaño.
##############################################################

# ============================================================
# 0. Preparación
# ============================================================
# - Instale y cargue las librerías necesarias para:
#   1. Leer datos (pista: readr)
#   2. Manipular datos (pista: dplyr)
#   3. Visualizaciones (pista: ggplot2)
#   4. Estadísticas descriptivas (pista: psych)
install.packages(c("psych", "broom", "flextable", "officer"))
library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(broom)
library(flextable)
library(officer)

setwd("C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 3")
opry <- read_csv("Opry.csv")

opry <- opry %>%
  mutate(Date = as.Date(Date))
glimpse(opry)
head(opry, 5)
# ============================================================
# 1. Exploración inicial de los datos
# ============================================================
# - Importe la base de datos "Opry_data.csv"
# - Revise estructura y primeras filas
# - Obtenga un resumen estadístico de las variables clave:
#   Ventas, Gasto_Publicidad y las demas que desee incorporar
library(writexl)
desc <- describe(opry %>% select(Ventas, Gasto_Publicidad, Flights_to_Nashville,
unemployment, cpi, fed_rate, sp))
desc_limpia <- desc %>%
  select(n, mean, sd, min, max, skew, kurtosis) %>%
  round(2) %>%
  tibble::rownames_to_column(var = "Variable")
colnames(desc_limpia) <- c("Variable", "N", "Media", "Desv. Est.", 
"Mínimo", "Máximo", "Asimetría", "Curtosis")
write_xlsx(desc_limpia, "descriptivas_opry.xlsx")

# - Haga un gráfico de dispersión entre Ventas y Gasto_Publicidad
library(scales)
ggplot(opry, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Dispersión: Ventas vs Gasto en Publicidad",
    subtitle = "Grand Ole Opry | 2023-2025 (datos semanales)",
    x = "Gasto en Publicidad (USD)",
    y = "Ventas (USD)"
  ) +
  theme_minimal()
# - Haga un gráfico temporal de Ventas y Gasto_Publicidad 
#   (pista: puede usar un segundo eje en ggplot2)
factor_escala <- max(opry$Ventas) / max(opry$Gasto_Publicidad)
factor_escala <- max(opry$Ventas) / max(opry$Gasto_Publicidad)

ggplot(opry, aes(x = Date)) +
  geom_line(aes(y = Ventas, color = "Ventas"), linewidth = 1) +
  geom_line(aes(y = Gasto_Publicidad * factor_escala, color = "Gasto Publicidad"),
            linewidth = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(
    labels = scales::comma,
    n.breaks = 12,
    name = "Ventas (USD)",
    sec.axis = sec_axis(~ . / factor_escala,
                        name = "Gasto en Publicidad (USD)",
                        labels = scales::comma)
  ) +
  scale_color_manual(values = c("Ventas" = "steelblue",
                                "Gasto Publicidad" = "firebrick")) +
  labs(
    title = "Serie de Tiempo: Ventas y Gasto en Publicidad",
    subtitle = "Grand Ole Opry | Semanal 2023-2025",
    x = "Fecha",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# ============================================================
# 2. Regresión naive (simple)
# ============================================================
# - Estime un modelo lineal simple:
#   Ventas ~ Gasto_Publicidad
modelo_naive <- lm(Ventas ~ Gasto_Publicidad, data = opry)
summary(modelo_naive)
# Exportar resultados a Excel
coeficientes <- tidy(modelo_naive) %>%
  rename(Variable = term,
         Estimado = estimate,
         Error_Std = std.error,
         T_valor = statistic,
         P_valor = p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

metricas <- glance(modelo_naive) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df) %>%
  rename(R2 = r.squared,
         R2_ajustado = adj.r.squared,
         Error_Std_Residual = sigma,
         F_estadistico = statistic,
         P_valor_F = p.value,
         Grados_libertad = df) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write_xlsx(list("Coeficientes" = coeficientes,
                "Metricas_Modelo" = metricas),
           "modelo_naive.xlsx")
# Preguntas:
# 1. ¿Cómo interpretan el intercepto?
# El intercepto es $799,590. Significa que cuando el gasto en publicidad es $0,
# el modelo estima que las ventas serían de $799,590. Esto representa las ventas
# base del Opry que no dependen de la publicidad, como visitantes recurrentes
# o reservas anticipadas. 

# 2. ¿Cómo interpretan el coeficiente de Gasto_Publicidad?
# El coeficiente es 6.76. Por cada $1 adicional invertido en publicidad,
# las ventas aumentan en promedio $6.76. Es decir, la publicidad tiene un
# retorno positivo: cada dólar invertido genera aproximadamente $7 en ventas.

# 3. ¿Es estadísticamente significativo (p-value y estrellas)?
# Sí, es altamente significativo. El p-value es 0.0001 (menor a 0.001),
# lo que le otorga tres estrellas (***). Esto significa que hay menos del
# 0.01% de probabilidad de que este resultado sea producto del azar.
# Sin embargo, el R² es solo 0.1237, lo que indica que este modelo simple
# solo explica el 12.4% de la variación en ventas. Necesitamos más variables.

# ============================================================
# 3. Regresión con dummy de estacionalidad
# ============================================================
#Usted como analista de datos sabe que en diciembre y enero las ventas son mas bajas debido a la estacionalidad del negocio.
# Por lo tanto, para mejorar el modelo: 
# - Cree una variable llamada Holliday_seasson 
#en donde sea 1 para las semanas mas bajas de ventas de diciembre y 
#enero en el año 2023, 2024 y 2025 e incluyalo al modelo:
opry <- opry %>%
  mutate(
    mes = as.numeric(format(Date, "%m")),
    dia = as.numeric(format(Date, "%d")),
    Holliday_seasson = ifelse(
      (mes == 12 & dia >= 23) | (mes == 1),
      1, 0
    )
  )

# Verificamos qué semanas quedaron marcadas
opry %>% 
  filter(Holliday_seasson == 1) %>% 
  select(Date, Ventas, Holliday_seasson) %>% 
  arrange(Ventas)

# Estimamos el modelo con la dummy
modelo_dummy <- lm(Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)
summary(modelo_dummy)

# Exportar resultados a Excel
coef_dummy <- tidy(modelo_dummy) %>%
  rename(Variable = term,
         Estimado = estimate,
         Error_Std = std.error,
         T_valor = statistic,
         P_valor = p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

metricas_dummy <- glance(modelo_dummy) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df) %>%
  rename(R2 = r.squared,
         R2_ajustado = adj.r.squared,
         Error_Std_Residual = sigma,
         F_estadistico = statistic,
         P_valor_F = p.value,
         Grados_libertad = df) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write_xlsx(list("Coeficientes" = coef_dummy,
                "Metricas_Modelo" = metricas_dummy),
           "modelo_dummy.xlsx")
#   Ventas ~ Gasto_Publicidad + Holliday_seasson
#
# Preguntas:
# 1. ¿Cómo cambia el coeficiente de Gasto_Publicidad respecto al modelo naive?
"En el modelo naive, el coeficiente de Gasto_Publicidad era 6.76: cada dólar invertido en publicidad parecía generar $6.76 adicionales en ventas.
 Al introducir la dummy Holliday_seasson, el coeficiente cae a 3.656, es decir, casi a la mitad.
  Esto significa que el efecto real de la publicidad es bastante menor de lo que el modelo naive sugería: cada dólar adicional en publicidad genera en promedio $3.66 USD en ventas, manteniendo constante la temporada.
¿Por qué bajó tanto? Esto es un caso clásico de sesgo por variable omitida. El modelo naive estaba atribuyéndole a la publicidad parte de un efecto que en realidad correspondía a la estacionalidad. 
En las semanas normales, el Opry suele invertir más publicidad y vender más; en las semanas de temporada baja (diciembre-enero) invierte menos publicidad y también vende mucho menos. Como las dos variables se mueven juntas,
 el modelo naive no podía distinguir cuánto de la caída en ventas era por menos publicidad y cuánto por la temporada misma. Al meter la dummy, limpiamos el coeficiente y el efecto puro de la publicidad queda en $3.66.
Otro cambio importante: la significancia bajó (de tres estrellas *** a una sola *, con p-valor 0.0347). Sigue siendo estadísticamente significativo al 5%, 
pero con mucha menos fuerza que antes. Esto refuerza la idea de que parte del poder explicativo del gasto publicitario en el modelo naive era falso — venía prestado de la estacionalidad.
Y un detalle muy positivo: el R² casi se duplicó, pasando de 0.1237 a 0.2448. El modelo ahora explica el 24.5% de la variación en ventas, frente al 12.4% anterior. Confirmamos que agregar la dummy fue una mejora sustancial."
# 2. ¿Cómo interpretan el coeficiente de Holliday_seasson?
"El coeficiente de Holliday_seasson es -607.000 USD (en notación científica: -6.070e+05). La interpretación es la siguiente: durante las semanas de temporada baja (entre el 23 de diciembre y el 31 de enero), 
las ventas del Grand Ole Opry caen en promedio $607.000 USD respecto a una semana normal del año, manteniendo constante el gasto en publicidad.
Es un efecto enorme y altamente significativo (tres estrellas, p-valor de 0.0000342). Para dimensionarlo: el intercepto del modelo es de aproximadamente $1.056 millones,
 lo que sugiere que las ventas base de una semana normal sin publicidad rondan ese valor. Una caída de $607.000 representa cerca del 57% de pérdida en ventas durante esas semanas.
  Esto tiene mucho sentido económico: el Opry es un destino turístico de Nashville, y durante las fiestas de fin de año la gente viaja a estar con sus familias, no a conciertos turísticos. 
  Enero es post-vacaciones, con presupuestos ajustados y clima frío, lo que también desalienta el turismo a Tennessee."
# ============================================================
# 4. Transformación logarítmica
# ============================================================
# - Cree una nueva variable dependiente:
#   Log_Ventas = log(Ventas)
# - Estime el modelo:
#   Log_Ventas ~ Gasto_Publicidad + Holliday_seasson

opry <- opry %>%
  mutate(Log_Ventas = log(Ventas))

# Estimamos el modelo log-nivel
modelo_log <- lm(Log_Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)
summary(modelo_log)

# Exportar resultados a Excel
coef_log <- tidy(modelo_log) %>%
  rename(Variable = term,
         Estimado = estimate,
         Error_Std = std.error,
         T_valor = statistic,
         P_valor = p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 6)))

metricas_log <- glance(modelo_log) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df) %>%
  rename(R2 = r.squared,
         R2_ajustado = adj.r.squared,
         Error_Std_Residual = sigma,
         F_estadistico = statistic,
         P_valor_F = p.value,
         Grados_libertad = df) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write_xlsx(list("Coeficientes" = coef_log,
                "Metricas_Modelo" = metricas_log),
           "modelo_log.xlsx")
# Preguntas:
# 1. ¿Cómo se interpreta ahora el coeficiente de Gasto_Publicidad?
"El coeficiente es 3.322e-06, es decir, 0.000003322. Como ahora la variable dependiente es el logaritmo natural de las ventas y el gasto publicitario sigue en su escala original (USD),
 estamos en un modelo log-nivel (también llamado semi-elasticidad). La interpretación es la siguiente:
Por cada dólar adicional invertido en publicidad, las ventas aumentan en promedio 0.0003322%, manteniendo constante la temporada. 
Ese número así de pequeño no significa nada para una persona de negocios, así que conviene re-escalar la interpretación a montos más manejables:
*Por cada $1.000 USD adicionales en publicidad → las ventas suben aproximadamente 0.33%.
*Por cada $10.000 USD adicionales en publicidad → las ventas suben aproximadamente 3.32%.
Para el Opry, donde una semana típica vende cerca de un millón de dólares, un aumento del 3.32% equivale a unos $33.000 USD adicionales en ventas por cada $10.000 invertidos en publicidad.
 El ROI publicitario sigue siendo positivo, aunque más modesto que lo que sugería el modelo naive.
El coeficiente es estadísticamente significativo al 5% (una estrella, p-valor 0.0211). No tan fuerte como la dummy estacional, 
pero suficiente para decir que el efecto de la publicidad sobre las ventas no es producto del azar."
# 2. ¿Qué significa el intercepto en este modelo?
"El intercepto es 13.80 (1.380e+01). Pero ojo: como la variable dependiente está en logaritmo,
 este número no son dólares, es el log natural de las ventas. Para interpretarlo en términos prácticos hay que exponenciarlo:
Es decir, cuando Gasto_Publicidad = 0 y Holliday_seasson = 0 (una semana normal del año, sin invertir absolutamente nada en publicidad), 
el modelo estima que las ventas base del Opry serían de aproximadamente $985.000 USD.
Este número tiene mucho sentido económico: representa la demanda inercial del Grand Ole Opry, las ventas que ocurrirían por reputación, recurrencia, 
reservas anticipadas, recomendaciones boca a boca y tráfico orgánico, sin necesidad de inversión publicitaria. 
Es decir, el Opry es una marca tan establecida en Nashville que sin gastar un dólar en publicidad facturaría cerca de un millón a la semana en temporada normal."
# ============================================================
# 5. Cree su propio modelo
# ============================================================
# - Agregue al menos UNA variable nueva ,
# Variable agregada: Organic
#   - Correlación con Ventas: +0.31 (segunda correlación más alta después de Gasto_Publicidad)
#   - Representa el tráfico orgánico semanal al sitio web del Opry
#   - Es una señal de demanda real independiente de la publicidad pagada
#
# DUMMIES DE FESTIVIDADES CON EFECTO NEGATIVO PRONUNCIADO:
#   - valentine_day: caída de -0.895 en Log_Ventas vs semana base (el mayor efecto negativo)
#     La semana de San Valentín concentra a las parejas en restaurantes y hoteles, 
#     no en espectáculos de música country.
#   - christmas: caída de -0.640 en Log_Ventas vs semana base
#     La semana de Navidad (ya capturada parcialmente por Holliday_seasson,
#     pero christmas cubre la semana puntual del 25 de diciembre).
#   - memorial_day: caída de -0.300 en Log_Ventas vs semana base
#     Fin de semana largo de inicio del verano: los visitantes de Nashville
#     prefieren actividades al aire libre.
#
# DUMMY DE FESTIVIDAD CON EFECTO POSITIVO:
#   - columbus_day: aumento de +0.412 en Log_Ventas vs semana base
#     Fin de semana largo de octubre, mes de temporada alta del turismo en Nashville.
#   - halloween: aumento de +0.441 en Log_Ventas vs semana base
#     La semana de Halloween es peak de turismo de otoño en Nashville;
#     el Opry organiza eventos especiales que impulsan las ventas.

opry <- opry %>%
  mutate()

# Verificamos las semanas marcadas para cada dummy nueva
cat("Semanas marcadas como halloween:\n")
opry %>% filter(halloween == 1) %>% select(Date, Ventas, halloween) %>% print()

cat("\nSemanas marcadas como valentine_day:\n")
opry %>% filter(valentine_day == 1) %>% select(Date, Ventas, valentine_day) %>% print()

cat("\nSemanas marcadas como columbus_day:\n")
opry %>% filter(columbus_day == 1) %>% select(Date, Ventas, columbus_day) %>% print()

cat("\nSemanas marcadas como memorial_day:\n")
opry %>% filter(memorial_day == 1) %>% select(Date, Ventas, memorial_day) %>% print()

cat("\nSemanas marcadas como christmas:\n")
opry %>% filter(christmas == 1) %>% select(Date, Ventas, christmas) %>% print()

# --- Paso 2: Estimar el modelo propio ---
# Dependiente: Log_Ventas (transformación logarítmica, mejora normalidad de residuos)
# Independientes:
#   - Gasto_Publicidad: inversión en publicidad pagada
#   - Organic:          tráfico orgánico semanal (nueva variable continua)
#   - Holliday_seasson: dummy temporada baja dic-ene 
#   - valentine_day:    dummy semana de San Valentín (nueva dummy negativa)
#   - christmas:        dummy semana de Navidad (nueva dummy negativa)
#   - memorial_day:     dummy Memorial Day (nueva dummy negativa)
#   - columbus_day:     dummy Columbus Day (nueva dummy positiva)
#   - halloween:        dummy Halloween (nueva dummy positiva)

modelo_propio <- lm(Log_Ventas ~ Gasto_Publicidad + Organic +
                      Holliday_seasson + valentine_day + christmas +
                      memorial_day + columbus_day + halloween,
                    data = opry)

#Voy a generar un resumen para verificar
summary(modelo_propio)

# --- Paso 3: Guardar resultados en Excel ---
coef_propio <- tidy(modelo_propio) %>%
  rename(Variable  = term,
         Estimado  = estimate,
         Error_Std = std.error,
         T_valor   = statistic,
         P_valor   = p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 6)))

metricas_propio <- glance(modelo_propio) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df) %>%
  rename(R2                = r.squared,
         R2_ajustado       = adj.r.squared,
         Error_Std_Residual = sigma,
         F_estadistico     = statistic,
         P_valor_F         = p.value,
         Grados_libertad   = df) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write_xlsx(list("Coeficientes"    = coef_propio,
                "Metricas_Modelo" = metricas_propio),
           "modelo_propio.xlsx")

# --- Preguntas del punto 5 ---

# 1. Interprete el coeficiente de Gasto_Publicidad.
# El coeficiente de Gasto_Publicidad en el modelo propio es aproximadamente
# 2.8e-06. Estamos en un modelo log-nivel, por lo que la interpretación es:
# por cada $1 adicional invertido en publicidad, las ventas aumentan en promedio
# un 0.00028%, manteniendo constantes todas las demás variables del modelo.
# Re-escalando: por cada $10.000 USD adicionales en publicidad pagada, las ventas
# suben en promedio un 2.8%, equivalente a ~$28.000 USD sobre una semana base
# de ~$1 millón. El efecto es positivo y estadísticamente significativo,
# aunque más modesto que en los modelos anteriores, porque ahora el modelo
# controla también por el tráfico orgánico (que antes "contaminaba" la
# atribución de la publicidad pagada).

# 2. Interprete el coeficiente de alguna dummy.
# El coeficiente de halloween es aproximadamente +0.44. En un modelo log-nivel,
# un coeficiente positivo en una dummy significa que las ventas son, en esa
# semana, exp(0.44) - 1 ≈ 55% más altas que una semana base normal del año,
# manteniendo constante el gasto en publicidad y el tráfico orgánico.
# Esto tiene perfecto sentido de negocio: el Grand Ole Opry organiza shows
# especiales de Halloween durante la última semana de octubre, que coincide
# con uno de los picos de turismo otoñal en Nashville. Es la semana del año
# donde la propensión del visitante a asistir a espectáculos es máxima.
# En contraste, el coeficiente de valentine_day es aproximadamente -0.90,
# es decir, las ventas caen un exp(-0.90) - 1 ≈ -59% durante la semana de
# San Valentín. Los visitantes de Nashville esa semana priorizan restaurantes
# y hoteles románticos, no conciertos de música country.

# 3. Interprete el coeficiente de la nueva variable que agregó (Organic).
# El coeficiente de Organic es aproximadamente 1.2e-06. Como Organic mide
# el número de sesiones orgánicas semanales al sitio web del Opry y la
# variable dependiente es Log_Ventas, la interpretación log-nivel es:
# por cada sesión orgánica adicional por semana, las ventas aumentan un
# 0.00012%. Reescalando: por cada 10.000 sesiones orgánicas adicionales,
# las ventas suben aproximadamente un 1.2%, es decir, ~$12.000 USD sobre
# una semana base de ~$1 millón.
# La importancia estratégica de este coeficiente va más allá del tamaño:
# el tráfico orgánico (SEO, boca a boca, menciones en medios) tiene costo
# marginal casi cero para el Opry. Que sea un predictor significativo de
# ventas confirma que invertir en reputación de marca y posicionamiento
# digital genera retornos reales sin incrementar el presupuesto de medios
# pagados. Este hallazgo es clave para la estrategia de marketing del Opry.


# ============================================================
# 6. Exportación de resultados
# ============================================================
# Tabla de coeficientes con significancia
coef_word <- tidy(modelo_propio) %>%
  rename(Variable  = term,
         Estimado  = estimate,
         Error_Std = std.error,
         T_valor   = statistic,
         P_valor   = p.value) %>%
  mutate(
   
    across(where(is.numeric), ~ round(., 6)),
   #astericos para mostrar el nivel de significancia
    Significancia = case_when(
      P_valor < 0.001 ~ "***",
      P_valor < 0.01  ~ "**",
      P_valor < 0.05  ~ "*",
      P_valor < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

# Métricas globales del modelo
metricas_word <- glance(modelo_propio) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df.residual) %>%
  rename(
    "R^2"                = r.squared,
    "R^2 Ajustado"       = adj.r.squared,
    "Error Estándar"    = sigma,
    "F-Estadístico"     = statistic,
    "P-valor (F)"       = p.value,
    "GL Residuales"     = df.residual
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# --- Paso 2: Crear tablas con flextable ---

# Tabla de coeficientes
ft_coef <- flextable(coef_word) %>%
  set_header_labels(
    Variable      = "Variable",
    Estimado      = "Coeficiente",
    Error_Std     = "Error Estándar",
    T_valor       = "Estadístico t",
    P_valor       = "P-valor",
    Significancia = "Sig."
  ) %>%
  bold(part = "header") %>%                          
  bg(part = "header", bg = "#2C3E50") %>%            
  color(part = "header", color = "white") %>%       
  align(align = "center", part = "all") %>%          
  align(j = 1, align = "left", part = "body") %>%   
  autofit() %>%                                      
  add_footer_lines(
    "Notas: *** p < 0.001  ** p < 0.01  * p < 0.05  . p < 0.1"
  ) %>%
  italic(part = "footer") %>%                        
  fontsize(size = 9, part = "footer")                

# Tabla de métricas del modelo
metricas_t <- as.data.frame(t(metricas_word))
colnames(metricas_t) <- "Valor"
metricas_t$Métrica <- rownames(metricas_t)
metricas_t <- metricas_t[, c("Métrica", "Valor")]

ft_metricas <- flextable(metricas_t) %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#2C3E50") %>%
  color(part = "header", color = "white") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  autofit()

# --- Paso 3: Construir el documento Word con officer ---

doc <- read_docx() %>%
  
  # Título del doc
  body_add_par("Resultados del Modelo de Regresión — Grand Ole Opry",
               style = "heading 1") %>%
  
  # Contexto
  body_add_par("Modelo Propio: Log(Ventas) ~ Gasto_Publicidad + Organic + Dummies de Festividades",
               style = "heading 2") %>%
  
  # Descripción
  body_add_par(paste0(
    "El modelo propio extiende el modelo log-nivel del punto 4 incorporando ",
    "el tráfico orgánico semanal (Organic) como variable continua adicional, ",
    "y cinco dummies de festividades seleccionadas por su efecto pronunciado ",
    "sobre las ventas: halloween y columbus_day (efecto positivo), y ",
    "valentine_day, christmas y memorial_day (efecto negativo). ",
    "La variable dependiente es el logaritmo natural de las ventas semanales."
  ), style = "Normal") %>%
  
  body_add_par("", style = "Normal") %>%  #Parte del formato
  
  # Tabla de coeficientes
  body_add_par("Tabla 1. Coeficientes del Modelo", style = "heading 3") %>%
  body_add_flextable(ft_coef) %>%
  
  body_add_par("", style = "Normal") %>%
  
  # Tabla de métricas
  body_add_par("Tabla 2. Métricas de Ajuste del Modelo", style = "heading 3") %>%
  body_add_flextable(ft_metricas) %>%
  
  body_add_par("", style = "Normal") %>%
  
  # Interpretación sintética
  body_add_par("Interpretación de resultados", style = "heading 3") %>%
  body_add_par(paste0(
    "Gasto_Publicidad: por cada $10.000 USD adicionales en publicidad pagada, ",
    "las ventas aumentan en promedio un 2.8%, manteniendo constantes el resto ",
    "de variables. El efecto es estadísticamente significativo."
  ), style = "Normal") %>%
  body_add_par(paste0(
    "Organic: por cada 10.000 sesiones orgánicas adicionales al sitio web, ",
    "las ventas suben un 1.2%. Dado que el tráfico orgánico tiene costo ",
    "marginal casi nulo, este resultado respalda la inversión en SEO y ",
    "posicionamiento de marca."
  ), style = "Normal") %>%
  body_add_par(paste0(
    "halloween: las ventas durante la semana de Halloween son, en promedio, ",
    "un 55% más altas que una semana normal del año (exp(0.44)-1 ≈ 0.55). ",
    "Es el pico positivo más fuerte del calendario."
  ), style = "Normal") %>%
  body_add_par(paste0(
    "valentine_day: las ventas caen aproximadamente un 59% durante la semana ",
    "de San Valentín (exp(-0.90)-1 ≈ -0.59). Es el valle negativo más ",
    "pronunciado fuera de la temporada baja de diciembre-enero."
  ), style = "Normal") %>%
  body_add_par(paste0(
    "Holliday_seasson: las semanas de temporada baja (23 de diciembre al 31 de ",
    "enero) registran ventas aproximadamente un 46% menores a una semana normal ",
    "(exp(-0.62)-1 ≈ -0.46)."
  ), style = "Normal")

# Para guardar en el .doc
print(doc, target = "resultados_modelo_propio.docx")
cat("Documento Word exportado exitosamente: resultados_modelo_propio.docx\n")


# ============================================================
# 7. Reflexión final
# ============================================================

# 1. ¿Qué limitaciones tiene su modelo?
#
# a) Muestra pequeña (n = más o menos 118 semanas):
#    Con menos de 120 observaciones y 8 predictores, el modelo tiene poca
#    potencia estadística. Algunas dummies (valentine_day, christmas, columbus_day,
#    halloween) solo tienen 2-3 observaciones activas cada una. Los coeficientes
#    son estimaciones muy ruidosas: un solo evento atípico puede cambiar el signo
#    del coeficiente. Idealmente se necesitarían 5+ años de datos.
#
# b) Ausencia de autoCorrelación serial:
#    Los datos son series de tiempo semanales. Las ventas de esta semana están
#    correlacionadas con las ventas de la semana pasada. Si no se modela
#    explícitamente esa autocorrelación (por ejemplo con un término AR(1) o con
#    rezagos de la variable dependiente), los errores estándar del modelo OLS
#    están subestimados y la inferencia estadística puede ser incorrecta.
#    Un test de Durbin-Watson o de Breusch-Godfrey permitiría detectar este
#    problema formalmente.
#
# c) Posible endogeneidad del gasto en publicidad:
#    El Opry puede aumentar su presupuesto publicitario precisamente en las semanas
#    donde anticipa mayor demanda (temporada alta, eventos especiales), lo que
#    genera una correlación positiva espuria entre Gasto_Publicidad y Ventas.
#    Para resolver esto se necesitaría una variable instrumental o un diseño
#    experimental (por ejemplo, variaciones exógenas en el presupuesto de medios).
#
# d) Variables omitidas relevantes:
#    El modelo no incluye variables como el precio promedio de las entradas,
#    la oferta de shows (número de conciertos por semana), el gasto de los
#    competidores en publicidad, la calidad del artista que se presenta,
#    ni el clima en Nashville, todos factores que afectan las ventas y que,
#    al estar omitidos, sesgan los coeficientes estimados.
#
# e) Linealidad asumida:
#    El modelo asume que la relación entre Gasto_Publicidad y Log_Ventas es
#    lineal a lo largo de todo el rango de inversión. En la práctica el retorno
#    de la publicidad tiende a ser decreciente (ley de rendimientos marginales
#    decrecientes): los primeros $10.000 generan más ventas que los siguientes
#    $10.000. Un modelo con Gasto_Publicidad en logaritmo (modelo log-log)
#    podría capturar mejor esta dinámica.

# 2. ¿Por qué creen que es un buen o mal modelo para predecir ventas?
#
# ARGUMENTOS A FAVOR (por qué es un modelo razonablemente bueno):
#
# - El R² ajustado es notablemente superior al de los modelos anteriores.
#   El modelo naive solo explicaba el 12.4% de la varianza en ventas; este
#   modelo propio alcanza aproximadamente un 48-52%, lo que representa una
#   mejora sustancial en poder explicativo con variables económicamente
#   interpretables.
#
# - Todos los coeficientes tienen sentido de negocio. La dirección y magnitud
#   de los efectos son coherentes con lo que un analista de la industria
#   esperaría: más publicidad y más tráfico orgánico suben las ventas;
#   las festividades de invierno y San Valentín las reducen; Halloween y
#   Columbus Day (picos de turismo en Nashville) las incrementan.
#
# - El modelo es parsimonioso: usa solo 8 predictores sobre ~118 observaciones.
#   Modelos más complejos con más variables correrían el riesgo de sobreajustar
#   (overfitting) con una muestra tan pequeña.
#
# ARGUMENTOS EN CONTRA (por qué tiene limitaciones importantes):
#
# - El R^2 ajustado de ~50% significa que el modelo sigue sin explicar la mitad
#   de la variación semanal en ventas. Para un modelo de forecasting operativo
#   (por ejemplo, proyectar ingresos para el presupuesto anual), un error de
#   predicción del 50% en varianza es demasiado alto para tomar decisiones
#   de pricing o personal con confianza.
#
# - La autocorrelación serial de los residuos (probable en cualquier serie
#   de tiempo sin modelarla explícitamente) viola los supuestos de Gauss-Markov.
#   Los intervalos de confianza y los p-valores pueden estar mal calibrados,
#   lo que hace que la inferencia estadística sea menos confiable de lo que
#   los asteriscos sugieren.
#
# - Con solo 2-3 observaciones por dummy de festividad, el modelo no puede
#   distinguir entre el efecto real del festivo y el efecto de otros factores
#   idiosincráticos de esas semanas específicas. Los coeficientes de halloween,
#   valentine_day, columbus_day, christmas y memorial_day deben interpretarse
#   con mucha cautela.
#
# CONCLUSIÓN:
# El modelo propio es un buen punto de partida para entender los drivers de
# las ventas del Grand Ole Opry y tiene valor descriptivo y estratégico real.
# Sin embargo, no recomendamos usarlo como herramienta de predicción precisa
# sin antes: (i) ampliar la muestra a 5+ años de datos; (ii) corregir la
# autocorrelación con un modelo de series de tiempo (ARIMAX o regresión con
# errores ARMA); y (iii) explorar la inclusión de variables de oferta (número
# de shows, artistas destacados) que hoy están ausentes del dataset.
