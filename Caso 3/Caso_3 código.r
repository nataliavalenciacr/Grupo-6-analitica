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

#   Ventas ~ Gasto_Publicidad + Holliday_seasson
#
# Preguntas:
# 1. ¿Cómo cambia el coeficiente de Gasto_Publicidad respecto al modelo naive?
# 2. ¿Cómo interpretan el coeficiente de Holliday_seasson?

# ============================================================
# 4. Transformación logarítmica
# ============================================================
# - Cree una nueva variable dependiente:
#   Log_Ventas = log(Ventas)
# - Estime el modelo:
#   Log_Ventas ~ Gasto_Publicidad + Holliday_seasson
#
# Preguntas:
# 1. ¿Cómo se interpreta ahora el coeficiente de Gasto_Publicidad?
# 2. ¿Qué significa el intercepto en este modelo?

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
