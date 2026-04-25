# CASO: Predicting Customer Churn at QWE Inc.
# Estudiantes: Natalia Valencia Casallas, Juan Pablo Mora Benavides & Daniela Ramírez Castaño 
# PUNTO 1: Importación y exploración inicial de los datos

library(readxl)    
library(dplyr)     
library(ggplot2)
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
