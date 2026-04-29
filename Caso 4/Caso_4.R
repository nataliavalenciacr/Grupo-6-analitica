# CASO: Predicting Customer Churn at QWE Inc.
# Estudiantes: Natalia Valencia Casallas, Juan Pablo Mora Benavides & Daniela Ramírez Castaño 
# PUNTO 1: Importación y exploración inicial de los datos

library(readxl)    
library(dplyr)     
library(ggplot2)
library(caret)
library(pROC)
library(openxlsx)
setwd("C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso 4")
datos <- read_excel("DATA.xlsx", sheet = "Case Data")
dim(datos)
head(datos)
# Renombrar columnas
colnames(datos) <- c(
  "ID",
  "Customer_Age",
  "Churn",
  "CHI_Score_M0",
  "CHI_Score_01",
  "Support_Cases_M0",
  "Support_Cases_01",
  "SP_M0",
  "SP_01",
  "Logins_01",
  "Blog_Articles_01",
  "Views_01",
  "Days_Since_Login_01"
)
colnames(datos)
datos$Churn <- as.factor(datos$Churn)

#¿Cuántos clientes hicieron churn?
table(datos$Churn)
prop.table(table(datos$Churn)) * 100
summary(datos)
# Tabla descriptiva
library(dplyr)
library(writexl)
variables_modelo <- datos %>%
  select(Customer_Age, CHI_Score_M0, CHI_Score_01,
         Support_Cases_M0, Support_Cases_01,
         SP_M0, SP_01,
         Logins_01, Blog_Articles_01, Views_01,
         Days_Since_Login_01)

tabla_resumen <- data.frame(
  Variable = c(
    "Customer Age (meses)",
    "CHI Score Mes 0",
    "CHI Score Cambio 0-1",
    "Support Cases Mes 0",
    "Support Cases Cambio 0-1",
    "SP Mes 0",
    "SP Cambio 0-1",
    "Logins Cambio 0-1",
    "Blog Articles Cambio 0-1",
    "Views Cambio 0-1",
    "Days Since Last Login Cambio 0-1"
  ),
  Min    = round(sapply(variables_modelo, min,    na.rm = TRUE), 2),
  Q1     = round(sapply(variables_modelo, quantile, probs = 0.25, na.rm = TRUE), 2),
  Mediana = round(sapply(variables_modelo, median, na.rm = TRUE), 2),
  Media  = round(sapply(variables_modelo, mean,   na.rm = TRUE), 2),
  Q3     = round(sapply(variables_modelo, quantile, probs = 0.75, na.rm = TRUE), 2),
  Max    = round(sapply(variables_modelo, max,    na.rm = TRUE), 2),
  Desv_Est = round(sapply(variables_modelo, sd,   na.rm = TRUE), 2)
)

print(tabla_resumen)
write_xlsx(tabla_resumen, "tabla_resumen_estadistico.xlsx")

ggplot(datos, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  scale_y_continuous(breaks = seq(0, 6500, by = 500)) +  # <- línea nueva
  labs(title = "Distribución de Churn en la muestra",
       x = "Churn (0 = No, 1 = Sí)",
       y = "Número de clientes") +
  theme_minimal()
# PARTE ii: Análisis exploratorio visual - Variables vs. Churn
# 2.1 Boxplots para cada variable 
# Variable 1: Antigüedad del cliente
ggplot(datos, aes(x = Churn, y = Customer_Age, fill = Churn)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) +  # <- línea nueva
  labs(title = "Antigüedad del cliente vs. Churn",
       x = "Churn (0 = No, 1 = Sí)", y = "Meses como cliente") +
  theme_minimal()
ggsave("boxplot_customer_age.png", width = 6, height = 4, dpi = 300)
# Variable 2: CHI Score mes actual
ggplot(datos, aes(x = Churn, y = CHI_Score_M0, fill = Churn)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +  # marcas cada 25 puntos
  labs(title = "CHI Score (Mes 0) vs. Churn",
       x = "Churn (0 = No, 1 = Sí)", y = "CHI Score Mes 0") +
  theme_minimal()
ggsave("boxplot_chi_score_m0.png", width = 6, height = 4, dpi = 300)
# Variable 3: Cambio en CHI Score
ggplot(datos, aes(x = Churn, y = CHI_Score_01, fill = Churn)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  scale_y_continuous(breaks = seq(-130, 210, by = 25)) +
  labs(title = "Cambio en CHI Score (0-1) vs. Churn",
       x = "Churn (0 = No, 1 = Sí)", y = "Cambio en CHI Score") +
  theme_minimal()
ggsave("boxplot_chi_score_01.png", width = 6, height = 4, dpi = 300)
#Variable 4: Casos de soporte mes actual
ggplot(datos, aes(x = Churn, y = Support_Cases_M0, fill = Churn)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  labs(title = "Casos de Soporte (Mes 0) vs. Churn",
       x = "Churn (0 = No, 1 = Sí)", y = "Número de casos de soporte") +
  theme_minimal()
ggsave("boxplot_support_cases.png", width = 6, height = 4, dpi = 300)
# Variable 5: Días desde último login
ggplot(datos, aes(x = Churn, y = Days_Since_Login_01, fill = Churn)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("0" = "#8dbde4", "1" = "#e69ddf"),
                    labels = c("Se quedó", "Se fue")) +
  coord_cartesian(ylim = c(-50, 65)) +
  scale_y_continuous(breaks = seq(-50, 65, by = 10)) +
  labs(title = "Días desde último login (0-1) vs. Churn",
       x = "Churn (0 = No, 1 = Sí)", 
       y = "Cambio en días desde último login") +
  theme_minimal()
ggsave("boxplot_days_login.png", width = 6, height = 4, dpi = 300)
# Tabla de medias por grupo de churn
tabla_medias <- datos %>%
  group_by(Churn) %>%
  summarise(
    Media_Antigüedad     = round(mean(Customer_Age), 2),
    Media_CHI_M0         = round(mean(CHI_Score_M0), 2),
    Media_CHI_Cambio     = round(mean(CHI_Score_01), 2),
    Media_Soporte_M0     = round(mean(Support_Cases_M0), 2),
    Media_Soporte_Cambio = round(mean(Support_Cases_01), 2),
    Media_SP_M0          = round(mean(SP_M0), 2),
    Media_Logins         = round(mean(Logins_01), 2),
    Media_Days_Login     = round(mean(Days_Since_Login_01), 2)
  )

print(tabla_medias)
write_xlsx(tabla_medias, "tabla_medias_por_churn.xlsx")


#Parte 3
#0. INICIAR LAS VARIABLES SIN ESPACIOS
datos_modelo <- datos %>% select(-ID)

# Imputar valores NA con la mediana de cada columna numérica.
# Se usa la mediana (en lugar de la media) por ser más robusta ante valores extremos.
datos_modelo <- datos_modelo %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Verificar que no queden variables NA
cat("Valores NA restantes:", sum(is.na(datos_modelo)), "\n")

#1. DIVISIÓN EN CONJUNTO DE ENTRENAMIENTO (TRAIN) Y PRUEBA (TEST)

# Fijar semilla para que los resultados tengan un estandar
set.seed(123)

# Crear índices para el 70% de los datos como entrenamiento (Se pueden cambiar los valores)
# Se usa createDataPartition para mantener la proporción de Churn en ambos conjuntos (muestreo estratificado)
indice_train <- createDataPartition(datos_modelo$Churn, p = 0.70, list = FALSE)

# Separar los conjuntos
train <- datos_modelo[indice_train, ]   # 70% para entrenar el modelo
test  <- datos_modelo[-indice_train, ]  # 30% para evaluar el modelo

# Tamaños de la muestra
cat("Observaciones en entrenamiento:", nrow(train), "\n")
cat("Observaciones en prueba:       ", nrow(test),  "\n")

# Verificar que la proporción de Churn es similar en ambos conjuntos
cat("\nProporción de Churn en TRAIN:\n"); print(prop.table(table(train$Churn)) * 100)
cat("\nProporción de Churn en TEST:\n");  print(prop.table(table(test$Churn))  * 100)

#2. CONSTRUCCIÓN DEL MODELO DE REGRESIÓN LOGÍSTICA

# glm() con family = binomial ajusta una regresión logística.
# La variable dependiente es Churn (1 = se fue, 0 = se quedó).
modelo_logistico <- glm(
  Churn ~ Customer_Age + CHI_Score_M0 + CHI_Score_01 +
    Support_Cases_M0 + Support_Cases_01 +
    SP_M0 + SP_01 +
    Logins_01 + Blog_Articles_01 + Views_01 + Days_Since_Login_01,
  data   = train,
  family = binomial(link = "logit")
)

# Resumen completo del modelo: coeficientes, errores estándar, valores z y p
cat("\n RESUMEN DEL MODELO DE REGRESIÓN LOGÍSTICA \n")
print(summary(modelo_logistico))

# R² de McFadden
r2_mcfadden <- 1 - (logLik(modelo_logistico) / logLik(modelo_nulo))
cat("R² de McFadden:", round(r2_mcfadden, 4), "\n")

#install.packages("pscl")
library(pscl)
r2_completo <- pR2(modelo_logistico)
print(r2_completo)
library(writexl)

tabla_r2 <- data.frame(
  Métrica = c("Log-Likelihood (modelo completo)", 
              "Log-Likelihood (modelo nulo)", 
              "G2 (estadístico)",
              "R² McFadden", 
              "R² ML",
              "R² CU (Nagelkerke)"),
  Valor = c(round(-855.46141981, 4),
            round(-896.28084296, 4),
            round(81.63884629, 4),
            round(0.04554312, 4),
            round(0.01820287, 4),
            round(0.05483899, 4))
)

print(tabla_r2)
write_xlsx(tabla_r2, "tabla_r2_mcfadden.xlsx")

#3. RESULTADOS DEL MODELO - TABLA DE COEFICIENTES, SIGNIFICANCIA Y ODDS RATIOS

# Extraer coeficientes y sus intervalos de confianza al 95%
coeficientes   <- coef(modelo_logistico)
ic_95          <- confint(modelo_logistico)   # Intervalos de confianza para los log-odds

# Calcular Odds Ratios: exp(coeficiente)
# Un OR > 1 indica mayor probabilidad de churn; OR < 1 indica menor probabilidad.
odds_ratios    <- exp(coeficientes)
ic_or          <- exp(ic_95)

# Obtener valores p del resumen del modelo
p_valores      <- summary(modelo_logistico)$coefficients[, 4]

# Construir tabla completa de resultados
tabla_resultados <- data.frame(
  Variable       = names(coeficientes),
  Coeficiente    = round(coeficientes, 4),
  Odds_Ratio     = round(odds_ratios, 4),
  IC_2.5         = round(ic_or[, 1], 4),
  IC_97.5        = round(ic_or[, 2], 4),
  P_valor        = round(p_valores, 4),
  Significancia  = ifelse(p_valores < 0.001, "***",
                   ifelse(p_valores < 0.01,  "**",
                   ifelse(p_valores < 0.05,  "*",
                   ifelse(p_valores < 0.1,   ".",  ""))))
)

cat("\n===== TABLA DE COEFICIENTES, ODDS RATIOS E INTERVALOS DE CONFIANZA =====\n")
print(tabla_resultados)

# Exportar tabla a Excel
library(writexl)
write_xlsx(tabla_resultados, "tabla_regresion_logistica.xlsx")
cat("Tabla exportada como 'tabla_regresion_logistica.xlsx'\n")

#4. INTERPRETACIÓN DE COEFICIENTES

cat("\n INTERPRETACIÓN DE COEFICIENTES (ODDS RATIOS) \n")

# Filtrar solo variables estadísticamente significativas (p < 0.05)
vars_significativas <- tabla_resultados %>%
  filter(Variable != "(Intercept)", P_valor < 0.05) %>%
  arrange(desc(abs(Coeficiente)))

cat("\nVariables estadísticamente significativas (p < 0.05):\n")
print(vars_significativas)

# Separar variables que AUMENTAN y DISMINUYEN el riesgo de churn
cat("\n-- Variables que AUMENTAN el riesgo de churn (OR > 1):\n")
print(vars_significativas %>% filter(Odds_Ratio > 1) %>% select(Variable, Odds_Ratio, P_valor))

cat("\n-- Variables que DISMINUYEN el riesgo de churn (OR < 1):\n")
print(vars_significativas %>% filter(Odds_Ratio < 1) %>% select(Variable, Odds_Ratio, P_valor))

#5. IDENTIFICACIÓN DE LOS 3 FACTORES PRINCIPALES (DRIVERS MÁS IMPORTANTES)

# Se identifican los 3 factores más influyentes usando el valor absoluto del
# estadístico z (coeficiente / error estándar), que refleja la importancia
# relativa de cada variable dentro del modelo.
estadisticos_z <- abs(summary(modelo_logistico)$coefficients[, 3])

tabla_importancia <- data.frame(
  Variable    = names(estadisticos_z),
  Estadistico_Z = round(estadisticos_z, 4),
  Odds_Ratio  = round(exp(coeficientes), 4),
  P_valor     = round(p_valores, 4)
) %>%
  filter(Variable != "(Intercept)") %>%
  arrange(desc(Estadistico_Z))

cat("\n===== RANKING DE IMPORTANCIA DE VARIABLES (por estadístico |z|) =====\n")
print(tabla_importancia)

# Top 3 factores
top3 <- tabla_importancia[1:3, ]
cat("\n===== TOP 3 FACTORES PRINCIPALES DE CHURN =====\n")
print(top3)

# Visualización del ranking de importancia
ggplot(tabla_importancia, aes(x = reorder(Variable, Estadistico_Z), y = Estadistico_Z)) +
  geom_bar(stat = "identity", fill = "#4a90d9") +
  geom_bar(data = top3,
           aes(x = reorder(Variable, Estadistico_Z), y = Estadistico_Z),
           stat = "identity", fill = "#e05c5c") +
  coord_flip() +
  labs(
    title    = "Importancia de variables en el modelo logístico",
    subtitle = "Top 3 resaltados en rojo | Métrica: |Estadístico Z|",
    x        = "Variable",
    y        = "Importancia (|z|)"
  ) +
  theme_minimal()

ggsave("importancia_variables_logistico.png", width = 8, height = 5, dpi = 300)

# 5b. EVALUACIÓN DEL MODELO EN EL CONJUNTO DE PRUEBA (TEST)

# Predecir probabilidades de churn para el conjunto de prueba
probabilidades_test <- predict(modelo_logistico, newdata = test, type = "response")

# Convertir probabilidades a clases usando umbral de 0.5
# (si prob >= 0.5, se predice churn = 1)
predicciones_clase <- ifelse(probabilidades_test >= 0.5, 1, 0)
predicciones_clase <- as.factor(predicciones_clase)

# Matriz de confusión: compara predicciones vs. valores reales
cat("\n===== MATRIZ DE CONFUSIÓN (CONJUNTO DE PRUEBA) =====\n")
conf_matrix <- confusionMatrix(predicciones_clase, test$Churn, positive = "1")
print(conf_matrix)

# Curva ROC y AUC (Area Under the Curve)
# AUC cercano a 1 indica excelente capacidad discriminatoria del modelo
roc_obj <- roc(as.numeric(as.character(test$Churn)), probabilidades_test)
cat("\nAUC del modelo:", round(auc(roc_obj), 4), "\n")

# Gráfico de la curva ROC
plot(roc_obj,
     main = paste("Curva ROC - AUC =", round(auc(roc_obj), 4)),
     col  = "#4a90d9",
     lwd  = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Línea de referencia (clasificador aleatorio)


# 4.1 PREDICCIÓN

test$prob_churn  <- predict(modelo_logistico, newdata = test, type = "response")
umbral           <- quantile(test$prob_churn, 0.90)
test$pred_churn  <- as.factor(if_else(test$prob_churn >= umbral, "1", "0"))

# 4.2 TOP 100
top100 <- test |>
  arrange(desc(prob_churn)) |>
  slice(1:100) |>
  select(
    "Edad Cliente (meses)" = Customer_Age,
    "CHI Score"            = CHI_Score_M0,
    "Prob. Churn"          = prob_churn
  ) |>
  mutate("Prob. Churn" = round(`Prob. Churn`, 4))

# 4.3 MATRIZ Y MÉTRICAS
matriz <- table(
  Real     = test$Churn,
  Predicho = test$pred_churn
)

print(matriz)

tn <- matriz["0","0"]
fp <- matriz["0","1"]
fn <- matriz["1","0"]
tp <- matriz["1","1"]

accuracy      <- round((tp + tn) / sum(matriz), 4)
sensibilidad  <- round(tp / (tp + fn), 4)
especificidad <- round(tn / (tn + fp), 4)

metricas <- tibble(
  Métrica = c("Accuracy", "Sensibilidad", "Especificidad"),
  Valor   = c(accuracy, sensibilidad, especificidad)
)

print(metricas)

# 4.4 ROC Y AUC
roc_obj   <- roc(
  as.numeric(as.character(test$Churn)),
  test$prob_churn
)

auc_valor <- round(auc(roc_obj), 4)
cat("AUC:", auc_valor, "\n")

plot(roc_obj,
     main = paste("Curva ROC - AUC:", auc_valor),
     col  = "#2E3B5F",
     lwd  = 2)

# 4.5 EXPORTAR


ruta_salida <- "evaluacion_churn.xlsx"

wb <- createWorkbook()

addWorksheet(wb, "Top 100 Clientes")
writeData(wb, "Top 100 Clientes", top100)

addWorksheet(wb, "Matriz Confusion")
writeData(wb, "Matriz Confusion", as.data.frame(matriz))

addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", metricas)
writeData(wb, "Metricas",
          data.frame(Métrica = "AUC", Valor = auc_valor),
          startRow = nrow(metricas) + 3)

saveWorkbook(wb, ruta_salida, overwrite = TRUE)
cat("Exportado correctamente\n")

png("C:/Users/danie/OneDrive/Documentos/CONTADURIA/TERCER SEMESTRE/ANÁLISIS DE LOS NEGOCIOS/curva_roc.png",
    width = 800, height = 600)

plot(roc_obj,
     main = paste("Curva ROC - AUC:", auc_valor),
     col  = "#2E3B5F",
     lwd  = 2)

dev.off()
cat("Gráfico exportado\n")
