# Caso 1: Web Analytics at Quality Alloys, Inc. 
# Integrantes del grupo: Juan Pablo Mora Benavides, Natalia Valencia Casallas & Daniela Ramírez Castaño.
library(readxl)
library(ggplot2)
library(dplyr)
library(moments)
ruta <- "C:/Users/natal/OneDrive/Personal/Universidad (1)/Noveno semestre/Analítica de datos/Caso_1/Web_Analytics.xls"
excel_sheets(ruta)
weekly_visits <- read_excel(ruta, sheet = "Weekly Visits")
financials    <- read_excel(ruta, sheet = "Financials")
lbs_sold_full <- read_excel(ruta, sheet = "Lbs. Sold")
daily_visits  <- read_excel(ruta, sheet = "Daily Visits")
head(weekly_visits)
head(financials)
weekly_visits <- read_excel(ruta, sheet = "Weekly Visits", skip = 3)
financials    <- read_excel(ruta, sheet = "Financials", skip = 3)
lbs_sold_full <- read_excel(ruta, sheet = "Lbs. Sold", skip = 1)
daily_visits  <- read_excel(ruta, sheet = "Daily Visits", skip = 1)
head(weekly_visits)
head(financials)
colnames(weekly_visits) <- c("Week", "Visits", "Unique_Visits", "Pageviews", 
                             "Pages_Visit", "Avg_Time", "Bounce_Rate", "New_Visits")
colnames(financials) <- c("Week", "Revenue", "Profit", "Lbs_Sold", "Inquiries")
datos <- cbind(weekly_visits, financials[, -1])
datos$Semana <- 1:nrow(datos)

datos$Periodo <- case_when(
  datos$Semana <= 14  ~ "1_Initial",
  datos$Semana <= 32  ~ "2_Pre_Promotion",
  datos$Semana <= 40  ~ "3_Promotion",
  TRUE                ~ "4_Post_Promotion"
)
table(datos$Periodo)
head(datos)
fechas_orden <- c(
  "May 25 - May 31", "Jun 1 - Jun 7", "Jun 8 - Jun 14", "Jun 15 - Jun 21",
  "Jun 22 - Jun 28", "Jun 29 - Jul 5", "Jul 6 - Jul 12", "Jul 13 - Jul 19",
  "Jul 20 - Jul 26", "Jul 27 - Aug 2", "Aug 3 - Aug 9", "Aug 10 - Aug 16",
  "Aug 17 - Aug 23", "Aug 24 - Aug 30", "Aug 31 - Sep 6", "Sep 7 - Sep 13",
  "Sep 14 - Sep 20", "Sep 21 - Sep 27", "Sep 28 - Oct 4", "Oct 5 - Oct 11",
  "Oct 12 - Oct 18", "Oct 19 - Oct 25", "Oct 26 - Nov 1", "Nov 2 - Nov 8",
  "Nov 9 - Nov 15", "Nov 16 - Nov 22", "Nov 23 - Nov 29", "Nov 30 - Dec 6",
  "Dec 7 - Dec 13", "Dec 14 - Dec 20", "Dec 21 - Dec 27", "Dec 28 - Jan 3",
  "Jan 4 - Jan 10", "Jan 11 - Jan 17", "Jan 18 - Jan 24", "Jan 25 - Jan 31",
  "Feb 1 - Feb 7", "Feb 8 - Feb 14", "Feb 15 - Feb 21", "Feb 22 - Feb 28",
  "Mar 1 - Mar 7", "Mar 8 - Mar 14", "Mar 15 - Mar 21", "Mar 22 - Mar 28",
  "Mar 29 - Apr 4", "Apr 5 - Apr 11", "Apr 12 - Apr 18", "Apr 19 - Apr 25",
  "Apr 26 - May 2", "May 3 - May 9", "May 10 - May 16", "May 17 - May 23",
  "May 24 - May 30", "May 31 - Jun 6", "Jun 7 - Jun 13", "Jun 14 - Jun 20",
  "Jun 21 - Jun 27", "Jun 28 - Jul 4", "Jul 5 - Jul 11", "Jul 12 - Jul 18",
  "Jul 19 - Jul 25", "Jul 26 - Aug 1", "Aug 2 - Aug 8", "Aug 9 - Aug 15",
  "Aug 16 - Aug 22", "Aug 23 - Aug 29"
)
datos$Week <- factor(datos$Week, levels = fechas_orden)

# Gráfico 1 . Unique Visits por Semana
ggplot(datos, aes(x = Week, y = Unique_Visits)) +
  geom_col(fill = "darkred") +
  scale_y_continuous(breaks = seq(0, 4000, by = 500), limits = c(0, 4000)) +
  labs(title = "Unique Visits por Semana", x = "Semana", y = "Unique Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
#Gráfico 2 : Revenue por Semana
ggplot(datos, aes(x = Week, y = Revenue)) +
  geom_col(fill = "darkgreen") +
  scale_y_continuous(breaks = seq(0, 1000000, by = 100000)) +
  labs(title = "Revenue por Semana", x = "Semana", y = "Revenue (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
#Gráfico 3: Profit por semana
options(scipen = 999)
ggplot(datos, aes(x = Week, y = Profit)) +
  geom_col(fill = "orange") +
  scale_y_continuous(
    breaks = seq(0, 400000, by = 50000),
    labels = scales::comma
  ) +
  labs(title = "Profit por Semana", x = "Semana", y = "Profit (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
# Gráfico 4 : Lbs sold por Semana
ggplot(datos, aes(x = Week, y = Lbs_Sold)) +
  geom_col(fill = "darkblue") +
  scale_y_continuous(breaks = seq(0, 35000, by = 5000)) +
  labs(title = "Lbs. Sold por Semana", x = "Semana", y = "Lbs. Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

# Estadísticas descriptivas por período
estadisticas <- datos %>%
  group_by(Periodo) %>%
  summarise(
    # Visits
    Visits_mean   = mean(Visits),
    Visits_median = median(Visits),
    Visits_sd     = sd(Visits),
    Visits_min    = min(Visits),
    Visits_max    = max(Visits),
    
    # Unique Visits
    UV_mean   = mean(Unique_Visits),
    UV_median = median(Unique_Visits),
    UV_sd     = sd(Unique_Visits),
    UV_min    = min(Unique_Visits),
    UV_max    = max(Unique_Visits),
    
    # Revenue
    Rev_mean   = mean(Revenue),
    Rev_median = median(Revenue),
    Rev_sd     = sd(Revenue),
    Rev_min    = min(Revenue),
    Rev_max    = max(Revenue),
    
    # Profit
    Prof_mean   = mean(Profit),
    Prof_median = median(Profit),
    Prof_sd     = sd(Profit),
    Prof_min    = min(Profit),
    Prof_max    = max(Profit),
    
    # Lbs Sold
    Lbs_mean   = mean(Lbs_Sold),
    Lbs_median = median(Lbs_Sold),
    Lbs_sd     = sd(Lbs_Sold),
    Lbs_min    = min(Lbs_Sold),
    Lbs_max    = max(Lbs_Sold)
  )
print(estadisticas)

estadisticas %>% 
  select(Periodo, Visits_mean, Visits_median, Visits_sd, Visits_min, Visits_max) %>%
  print()

estadisticas %>% 
  select(Periodo, UV_mean, UV_median, UV_sd, UV_min, UV_max) %>%
  print()

estadisticas %>% 
  select(Periodo, Rev_mean, Rev_median, Rev_sd, Rev_min, Rev_max) %>%
  print()

estadisticas %>% 
  select(Periodo, Prof_mean, Prof_median, Prof_sd, Prof_min, Prof_max) %>%
  print()

estadisticas %>% 
  select(Periodo, Lbs_mean, Lbs_median, Lbs_sd, Lbs_min, Lbs_max) %>%
  print()
install.packages("writexl")
library(writexl)
tabla_descriptiva <- function(periodo_nombre) {
  d <- estadisticas %>% filter(Periodo == periodo_nombre)
  data.frame(
    Medida         = c("mean", "median", "std. dev.", "minimum", "maximum"),
    Visits         = round(c(d$Visits_mean, d$Visits_median, d$Visits_sd, d$Visits_min, d$Visits_max), 2),
    Unique_Visits  = round(c(d$UV_mean, d$UV_median, d$UV_sd, d$UV_min, d$UV_max), 2),
    Revenue        = round(c(d$Rev_mean, d$Rev_median, d$Rev_sd, d$Rev_min, d$Rev_max), 2),
    Profit         = round(c(d$Prof_mean, d$Prof_median, d$Prof_sd, d$Prof_min, d$Prof_max), 2),
    Lbs_Sold       = round(c(d$Lbs_mean, d$Lbs_median, d$Lbs_sd, d$Lbs_min, d$Lbs_max), 2)
  )
}

write_xlsx(
  list(
    "Initial"        = tabla_descriptiva("1_Initial"),
    "Pre-Promotion"  = tabla_descriptiva("2_Pre_Promotion"),
    "Promotion"      = tabla_descriptiva("3_Promotion"),
    "Post-Promotion" = tabla_descriptiva("4_Post_Promotion")
  ),
  path = "C:/Users/natal/Downloads/Estadisticas_QA.xlsx"
)
# Medias por tiempo (punto 3)
medias <- data.frame(
Periodo        = c("Initial", "Pre-Promotion", "Promotion", "Post-Promotion"),
Visits         = estadisticas$Visits_mean,
Unique_Visits  = estadisticas$UV_mean,
Revenue        = estadisticas$Rev_mean,
Profit         = estadisticas$Prof_mean,
Lbs_Sold       = estadisticas$Lbs_mean
)
medias$Periodo <- factor(medias$Periodo, 
                         levels = c("Initial", "Pre-Promotion", "Promotion", "Post-Promotion"))
# Gráfica 1: Media de Visitas por período
ggplot(medias, aes(x = Periodo, y = Visits)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Visits, 0)), vjust = -0.5, size = 4) +
  labs(title = "Media de Visitas por Período", x = "Período", y = "Visits") +
  theme_minimal()
# Gráfica 2: Media de Unique Visits por período
ggplot(medias, aes(x = Periodo, y = Unique_Visits)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = round(Unique_Visits, 0)), vjust = -0.5, size = 4) +
  labs(title = "Media de Unique Visits por Período", x = "Período", y = "Unique Visits") +
  theme_minimal()
# Gráfica 3: Media de Revenue por período
ggplot(medias, aes(x = Periodo, y = Revenue)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(Revenue, 0)), vjust = -0.5, size = 4) +
  labs(title = "Media de Revenue por Período", x = "Período", y = "Revenue (USD)") +
  theme_minimal()
# Gráfica 4:Media de Profit por período
ggplot(medias, aes(x = Periodo, y = Profit)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = round(Profit, 0)), vjust = -0.5, size = 4) +
  labs(title = "Media de Profit por Período", x = "Período", y = "Profit (USD)") +
  theme_minimal()
# Gráfica 5: Media de Lbs. Sold por período
ggplot(medias, aes(x = Periodo, y = Lbs_Sold)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = round(Lbs_Sold, 0)), vjust = -0.5, size = 4) +
  labs(title = "Media de Lbs. Sold por Período", x = "Período", y = "Lbs. Sold") +
  theme_minimal()

# Punto 8 -------------------------------------------------------------------------------------------------------------------------------------------
# Leer la hoja Lbs. Sold completa
lbs_full <- read_excel(ruta, sheet = "Lbs. Sold", skip = 1)
colnames(lbs_full) <- c("Week", "Lbs_Sold")


# Convertir el número serial de Excel a fecha real
lbs_full$Week <- as.Date(lbs_full$Week, origin = "1899-12-30")

# Verificar
head(lbs_full, 5)
nrow(lbs_full)

# Eliminar filas con NA
lbs_full <- lbs_full %>% filter(!is.na(Lbs_Sold))

# ---- 8a: Estadísticas descriptivas ----
lbs_mean <- mean(lbs_full$Lbs_Sold)
lbs_sd   <- sd(lbs_full$Lbs_Sold)
lbs_n    <- nrow(lbs_full)

cat("=== Estadísticas Lbs. Sold (2005-2010) ===\n")
cat("Media:            ", round(lbs_mean, 2), "\n")
cat("Mediana:          ", round(median(lbs_full$Lbs_Sold), 2), "\n")
cat("Desv. Estándar:   ", round(lbs_sd, 2), "\n")
cat("Mínimo:           ", min(lbs_full$Lbs_Sold), "\n")
cat("Máximo:           ", max(lbs_full$Lbs_Sold), "\n")
cat("N observaciones:  ", lbs_n, "\n")

# ---- 8b: Histograma ----
ggplot(lbs_full, aes(x = Lbs_Sold)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "black") +
  labs(title = "Histograma de Lbs. Sold (Ene 2005 - Jul 2010)",
       x = "Lbs. Sold", y = "Frecuencia") +
  theme_minimal()

# ---- 8c: Ver si es bell-shaped (visual) ----
# Se interpreta visualmente desde el histograma anterior

# ---- 8d: Regla Empírica - Tabla general ±1, ±2, ±3 desv. est. ----

# Calcular z-scores
lbs_full <- lbs_full %>%
  mutate(z_score = (Lbs_Sold - lbs_mean) / lbs_sd)

# Conteos reales
obs_1sd <- sum(abs(lbs_full$z_score) <= 1)
obs_2sd <- sum(abs(lbs_full$z_score) <= 2)
obs_3sd <- sum(abs(lbs_full$z_score) <= 3)

tabla_empirica <- data.frame(
  Intervalo            = c("mean ± 1 std. dev.", "mean ± 2 std. dev.", "mean ± 3 std. dev."),
  Teorico_pct          = c("68%", "95%", "99%"),
  Teorico_obs          = round(c(0.68 * lbs_n, 0.95 * lbs_n, 0.99 * lbs_n), 0),
  Real_obs             = c(obs_1sd, obs_2sd, obs_3sd)
)

cat("\n=== 8d: Regla Empírica ===\n")
print(tabla_empirica)

# ---- 8e: Tabla detallada por intervalos unilaterales ----

obs_pos1  <- sum(lbs_full$z_score > 0  & lbs_full$z_score <= 1)   # 0 a +1
obs_neg1  <- sum(lbs_full$z_score >= -1 & lbs_full$z_score < 0)   # -1 a 0
obs_p1p2  <- sum(lbs_full$z_score > 1  & lbs_full$z_score <= 2)   # +1 a +2
obs_n2n1  <- sum(lbs_full$z_score >= -2 & lbs_full$z_score < -1)  # -2 a -1
obs_p2p3  <- sum(lbs_full$z_score > 2  & lbs_full$z_score <= 3)   # +2 a +3
obs_n3n2  <- sum(lbs_full$z_score >= -3 & lbs_full$z_score < -2)  # -3 a -2

# Teórico para cada mitad (la distribución normal es simétrica)
teo_mitad1sd  <- round(0.34 * lbs_n, 0)   # ~34% a cada lado de la media
teo_1a2       <- round(0.135 * lbs_n, 0)  # ~13.5% entre 1 y 2 SD
teo_2a3       <- round(0.021 * lbs_n, 0)  # ~2.1% entre 2 y 3 SD

tabla_detalle <- data.frame(
  Intervalo     = c("mean a mean + 1 std. dev.",
                    "mean - 1 std. dev. a mean",
                    "1 std. dev. a 2 std. dev.",
                    "-1 std. dev. a -2 std. dev.",
                    "2 std. dev. a 3 std. dev.",
                    "-2 std. dev. a -3 std. dev."),
  Teorico_obs   = c(teo_mitad1sd, teo_mitad1sd,
                    teo_1a2, teo_1a2,
                    teo_2a3, teo_2a3),
  Real_obs      = c(obs_pos1, obs_neg1,
                    obs_p1p2, obs_n2n1,
                    obs_p2p3, obs_n3n2)
)

print(tabla_detalle)

#8g: Skewness y Kurtosis
install.packages("moments")
library(moments)

lbs_skew <- skewness(lbs_full$Lbs_Sold)
lbs_kurt  <- kurtosis(lbs_full$Lbs_Sold) - 3  # exceso de kurtosis (R da kurtosis normal = 3)

print(lbs_skew)
print(lbs_kurt)

# Punto 9------------------------------

# Ver qué hay en la hoja Demographics
demographics <- read_excel(ruta, sheet = "Demographics")
head(demographics, 20)
colnames(demographics)
nrow(demographics)

print(demographics, n = 77)

# ----- Gráfico 1: All Traffic Sources -----
trafico <- data.frame(
  Fuente = c("Referring Sites", "Search Engines", "Direct Traffic", "Other"),
  Visits = c(38754, 20964, 9709, 4)
)

ggplot(trafico, aes(x = reorder(Fuente, -Visits), y = Visits)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(Visits)), vjust = -0.5, size = 4) +
  labs(title = "1) All Traffic Sources", x = "Fuente", y = "Visits") +
  theme_minimal()

# ----- Gráfico 2: Top 10 Referring Sites -----
referring <- data.frame(
  Sitio  = c("googleads", "pagead2.google", "sedoparking.com", "globalspec.com",
             "searchportal", "freepatentsonline", "thomasnet.com",
             "mu.com", "mail.google.com", "psicofxp.com"),
  Visits = c(15626, 8044, 3138, 693, 582, 389, 379, 344, 337, 310)
)

ggplot(referring, aes(x = reorder(Sitio, Visits), y = Visits)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = scales::comma(Visits)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "2) Top 10 Referring Sites", x = "Sitio", y = "Visits") +
  theme_minimal()

# ----- Gráfico 3: Top 10 Search Engine Sources -----
buscadores <- data.frame(
  Motor  = c("google", "yahoo", "search", "msn", "aol",
             "ask", "live", "bing", "voila", "netscape"),
  Visits = c(17681, 1250, 592, 424, 309, 268, 145, 122, 63, 26)
)

ggplot(buscadores, aes(x = reorder(Motor, Visits), y = Visits)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = scales::comma(Visits)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "3) Top 10 Search Engine Sources", x = "Motor de Búsqueda", y = "Visits") +
  theme_minimal()

# ----- Gráfico 4: Top 10 Geographic Sources -----
geografico <- data.frame(
  Region = c("South America", "Northern America", "Central America",
             "Western Europe", "Eastern Asia", "Northern Europe",
             "Southern Asia", "South-Eastern Asia", "Southern Europe", "Eastern Europe"),
  Visits = c(22616, 17509, 6776, 5214, 3228, 2721, 2589, 1968, 1538, 1427)
)

ggplot(geografico, aes(x = reorder(Region, Visits), y = Visits)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = scales::comma(Visits)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "4) Top 10 Geographic Sources", x = "Región", y = "Visits") +
  theme_minimal()

# ----- Gráfico 5: Top 10 Browsers -----
browsers <- data.frame(
  Browser = c("Internet Explorer", "Firefox", "Opera", "Safari", "Chrome",
              "Mozilla", "Netscape", "Konqueror", "SeaMonkey", "Camino"),
  Visits  = c(53080, 13142, 938, 850, 792, 478, 47, 31, 24, 9)
)

ggplot(browsers, aes(x = reorder(Browser, Visits), y = Visits)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = scales::comma(Visits)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "5) Top 10 Browsers Used", x = "Navegador", y = "Visits") +
  theme_minimal()

# ----- Gráfico 6: Top 10 Operating Systems -----
sistemas <- data.frame(
  OS     = c("Windows", "Macintosh", "Linux", "(not set)", "iPhone",
             "SymbianOS", "FreeBSD", "iPod", "Playstation 3", "Playstation Portable"),
  Visits = c(67063, 1184, 1045, 48, 29, 20, 18, 8, 4, 3)
)

ggplot(sistemas, aes(x = reorder(OS, Visits), y = Visits)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = scales::comma(Visits)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "6) Top 10 Operating Systems", x = "Sistema Operativo", y = "Visits") +
  theme_minimal()



