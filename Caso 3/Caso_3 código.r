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
¿Por qué bajó tanto? Esto es un caso clásico de sesgo por variable omitida. El modelo naive estaba "atribuyéndole" a la publicidad parte de un efecto que en realidad correspondía a la estacionalidad. 
En las semanas normales, el Opry suele invertir más publicidad y vender más; en las semanas de temporada baja (diciembre-enero) invierte menos publicidad y también vende mucho menos. Como las dos variables se mueven juntas,
 el modelo naive no podía distinguir cuánto de la caída en ventas era por menos publicidad y cuánto por la temporada misma. Al meter la dummy, "limpiamos" el coeficiente y el efecto puro de la publicidad queda en $3.66.
Otro cambio importante: la significancia bajó (de tres estrellas *** a una sola *, con p-valor 0.0347). Sigue siendo estadísticamente significativo al 5%, 
pero con mucha menos fuerza que antes. Esto refuerza la idea de que parte del "poder explicativo" del gasto publicitario en el modelo naive era falso — venía prestado de la estacionalidad.
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
#   (ejemplo: Flights_to_Nashville, unemployment, cpi, etc...)
# También pueden mirar las ventas y ver si algun pico o valle muy pronunciado cuadra con un holliday que no este incluido en la base de datos y
# ustedes crearla y agregarla al modelo.
# - Use como dependiente Log_Ventas.
# - Estime el modelo y analice los resultados.
#
# Preguntas:
# 1. Interprete el coeficiente de Gasto_Publicidad.
# 2. Interprete el coeficiente de alguna dummy.
# 3. Interprete el coeficiente de la nueva variable que agregó.

# ============================================================
# 6. Exportación de resultados
# ============================================================
# - Exporte los resultados de su modelo a Word.
# - Pista: use librerías como broom, flextable y officer.
# - Puede crear una tabla con los coeficientes y R², 
#   y agregar un footnote con la interpretación de las estrellas.

# ============================================================
# 7. Reflexión final
# ============================================================
# - ¿Qué limitaciones tiene su modelo?
# - ¿Por qué creen que es un buen o mal modelo para predecir las ventas?
