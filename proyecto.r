############################################################
# PROYECTO FINAL
############################################################

install.packages(c(
  "readxl", "tidyverse", "psych", "corrplot", "car",
  "FactoMineR", "factoextra", "cluster", "nortest",
  "reshape2", "janitor", "scales", "ggcorrplot"
))

library(readxl)
library(tidyverse)
library(psych)
library(corrplot)
library(ggcorrplot)
library(car)
library(FactoMineR)
library(factoextra)
library(cluster)
library(nortest)
library(reshape2)
library(janitor)
library(scales)

dir.create("graficas", showWarnings = FALSE)

############################################################
# TEMA PERSONALIZADO (aplica a todas las gráficas)
############################################################

COLOR_AZUL    <- "#2C5F8A"
COLOR_NARANJA <- "#E67E22"
COLOR_GRIS    <- "#4A4A4A"
COLOR_FONDO   <- "white"
COLOR_GRID    <- "#EEEEEE"

tema_proyecto <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 14, color = COLOR_GRIS,
                                     margin = margin(b = 10)),
    plot.subtitle     = element_text(size = 10, color = "#777777",
                                     margin = margin(b = 10)),
    axis.title        = element_text(size = 11, color = COLOR_GRIS),
    axis.text         = element_text(size = 9,  color = COLOR_GRIS),
    panel.grid.major  = element_line(color = COLOR_GRID, linewidth = 0.5),
    panel.grid.minor  = element_blank(),
    panel.background  = element_rect(fill = COLOR_FONDO, color = NA),
    plot.background   = element_rect(fill = COLOR_FONDO, color = NA),
    legend.position   = "none",
    plot.caption      = element_text(size = 8, color = "#999999", hjust = 0,
                                     margin = margin(t = 8))
  )

theme_set(tema_proyecto)

############################################################
# CARGA DE DATOS
############################################################

datos <- read_excel("netflix_large_user_data.xlsx")

names(datos) <- c(
  "customer_id", "subscription_length", "satisfaction", "watch_time",
  "engagement", "device", "genre", "region", "payment_history",
  "subscription_plan", "churn", "support_queries", "age",
  "income", "promo_offers", "profiles"
)

datos <- clean_names(datos)

datos$device            <- as.factor(datos$device)
datos$genre             <- as.factor(datos$genre)
datos$region            <- as.factor(datos$region)
datos$payment_history   <- as.factor(datos$payment_history)
datos$subscription_plan <- as.factor(datos$subscription_plan)
datos$churn             <- as.factor(datos$churn)

############################################################
# INFORMACIÓN GENERAL
############################################################

str(datos)
summary(datos)
colSums(is.na(datos))

############################################################
# ESTADÍSTICOS DESCRIPTIVOS
############################################################

estadisticos <- describe(
  datos %>% select(subscription_length, satisfaction, watch_time,
                   engagement, support_queries, age, income,
                   promo_offers, profiles)
)

write.csv(estadisticos, "estadisticos_descriptivos.csv")

############################################################
# VARIABLES NUMÉRICAS
############################################################

numericas <- c(
  "subscription_length", "satisfaction", "watch_time", "engagement",
  "support_queries", "age", "income", "promo_offers", "profiles"
)

etiquetas <- c(
  "Duración de suscripción (meses)", "Satisfacción (1–10)",
  "Tiempo de uso diario (horas)", "Engagement (1–10)",
  "Consultas de soporte", "Edad", "Ingreso mensual ($)",
  "Ofertas promocionales usadas", "Perfiles creados"
)

names(etiquetas) <- numericas

############################################################
# HISTOGRAMAS
############################################################

for (v in numericas) {
  p <- ggplot(datos, aes_string(x = v)) +
    geom_histogram(bins = 30, fill = COLOR_AZUL, color = "white", linewidth = 0.3) +
    geom_vline(aes_string(xintercept = paste0("mean(", v, ")")),
               color = COLOR_NARANJA, linewidth = 1, linetype = "dashed") +
    labs(
      title    = paste("Distribución de", etiquetas[v]),
      subtitle = paste("Media indicada en naranja"),
      x        = etiquetas[v],
      y        = "Frecuencia"
    )

  ggsave(paste0("graficas/hist_", v, ".png"), p, width = 8, height = 5, dpi = 150)
}

############################################################
# DENSIDADES
############################################################

for (v in numericas) {
  p <- ggplot(datos, aes_string(x = v)) +
    geom_density(fill = COLOR_AZUL, alpha = 0.35, color = COLOR_AZUL, linewidth = 0.8) +
    labs(
      title = paste("Densidad de", etiquetas[v]),
      x     = etiquetas[v],
      y     = "Densidad"
    )

  ggsave(paste0("graficas/densidad_", v, ".png"), p, width = 8, height = 5, dpi = 150)
}

############################################################
# BOXPLOTS POR CHURN
############################################################

for (v in numericas) {
  p <- ggplot(datos, aes_string(x = "churn", y = v, fill = "churn")) +
    geom_boxplot(width = 0.45, outlier.size = 0.8, outlier.alpha = 0.3,
                 color = COLOR_GRIS, linewidth = 0.5) +
    scale_fill_manual(values = c("No" = COLOR_AZUL, "Yes" = COLOR_NARANJA)) +
    scale_x_discrete(labels = c("No" = "No (Retención)", "Yes" = "Sí (Abandono)")) +
    labs(
      title = paste(etiquetas[v], "según estado de churn"),
      x     = "Estado de churn",
      y     = etiquetas[v]
    ) +
    theme(legend.position = "none")

  ggsave(paste0("graficas/boxplot_", v, ".png"), p, width = 7, height = 5, dpi = 150)
}

############################################################
# VARIABLES CATEGÓRICAS
############################################################

etiq_cat <- list(
  device            = "Dispositivo principal",
  genre             = "Género de preferencia",
  region            = "Región geográfica",
  payment_history   = "Historial de pagos",
  subscription_plan = "Plan de suscripción",
  churn             = "Estado de abandono (churn)"
)

colores_cat <- c(
  "#2C5F8A", "#3A7DBF", "#E67E22", "#C0392B",
  "#27AE60", "#8E44AD", "#16A085", "#D35400"
)

for (v in names(etiq_cat)) {
  conteo <- datos %>%
    count(.data[[v]]) %>%
    mutate(pct = n / sum(n),
           etiq = paste0(round(pct * 100, 1), "%"))

  p <- ggplot(conteo, aes_string(x = v, y = "n", fill = v)) +
    geom_col(width = 0.55, color = "white", linewidth = 0.3) +
    geom_text(aes(label = etiq, y = n + max(conteo$n) * 0.02),
              size = 3.5, color = COLOR_GRIS, fontface = "bold") +
    scale_fill_manual(values = colores_cat[seq_len(nlevels(datos[[v]]))]) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = paste("Distribución de", etiq_cat[[v]]),
      x     = etiq_cat[[v]],
      y     = "Número de usuarios"
    ) +
    theme(legend.position = "none")

  ggsave(paste0("graficas/barra_", v, ".png"), p, width = 8, height = 5, dpi = 150)
}

############################################################
# MATRIZ DE CORRELACIÓN
############################################################

datos_num <- datos %>%
  select(subscription_length, satisfaction, watch_time, engagement,
         support_queries, age, income, promo_offers, profiles)

cor_matrix <- cor(datos_num)

write.csv(cor_matrix, "correlaciones.csv")

# Versión mejorada con ggcorrplot
p_corr <- ggcorrplot(
  cor_matrix,
  method    = "square",
  type      = "upper",
  lab       = TRUE,
  lab_size  = 3,
  colors    = c("#C0392B", "white", "#2C5F8A"),
  outline.color = "white",
  ggtheme   = theme_minimal(base_size = 11)
) +
  labs(title = "Matriz de Correlación entre Variables Numéricas") +
  theme(
    plot.title      = element_text(face = "bold", size = 13, color = COLOR_GRIS,
                                   hjust = 0.5, margin = margin(b = 10)),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9, color = COLOR_GRIS),
    axis.text.y     = element_text(size = 9, color = COLOR_GRIS),
    legend.title    = element_text(size = 9),
    plot.background = element_rect(fill = COLOR_FONDO, color = NA),
    panel.background = element_rect(fill = COLOR_FONDO, color = NA)
  )

ggsave("graficas/correlacion.png", p_corr, width = 9, height = 8, dpi = 150)

############################################################
# NORMALIDAD
############################################################

shapiro.test(datos$satisfaction)
shapiro.test(datos$engagement)
shapiro.test(datos$subscription_length)
shapiro.test(datos$watch_time)

############################################################
# T TEST
############################################################

t_satisfaction  <- t.test(satisfaction        ~ churn, data = datos)
t_engagement    <- t.test(engagement          ~ churn, data = datos)
t_subscription  <- t.test(subscription_length ~ churn, data = datos)
t_watch         <- t.test(watch_time          ~ churn, data = datos)
t_support       <- t.test(support_queries     ~ churn, data = datos)

t_satisfaction
t_engagement
t_subscription
t_watch
t_support

############################################################
# ANOVA
############################################################

anova_plan <- aov(satisfaction ~ subscription_plan, data = datos)
summary(anova_plan)
TukeyHSD(anova_plan)

############################################################
# CHI CUADRADO
############################################################

chisq.test(table(datos$subscription_plan, datos$churn))
chisq.test(table(datos$payment_history,   datos$churn))
chisq.test(table(datos$device,            datos$churn))
chisq.test(table(datos$region,            datos$churn))

############################################################
# REGRESIÓN LOGÍSTICA
############################################################

datos$churn_bin <- ifelse(datos$churn == "Yes", 1, 0)

modelo <- glm(
  churn_bin ~
    subscription_length + satisfaction + watch_time +
    engagement + support_queries + income +
    payment_history + subscription_plan,
  family = "binomial",
  data   = datos
)

summary(modelo)

############################################################
# ODDS RATIOS
############################################################

odds_ratios <- exp(coef(modelo))
odds_ratios
write.csv(odds_ratios, "odds_ratios.csv")
exp(confint(modelo))

############################################################
# PREDICCIONES
############################################################

datos$prob_churn <- predict(modelo, type = "response")

############################################################
# PCA
############################################################

pca <- PCA(datos_num, graph = FALSE)

# Gráfico de individuos
p_ind <- fviz_pca_ind(
  pca,
  geom.ind    = "point",
  col.ind     = COLOR_AZUL,
  alpha.ind   = 0.4,
  pointsize   = 1.2,
  title       = "PCA – Distribución de Individuos"
) +
  tema_proyecto +
  theme(legend.position = "none")

ggsave("graficas/pca_individuos.png", p_ind, width = 8, height = 7, dpi = 150)

# Gráfico de variables (círculo de correlación)
p_var <- fviz_pca_var(
  pca,
  col.var     = "contrib",
  gradient.cols = c("#D6E4F0", "#2C5F8A"),
  repel       = TRUE,
  title       = "PCA – Círculo de Correlación de Variables"
) +
  tema_proyecto +
  theme(legend.position = "right")

ggsave("graficas/pca_variables.png", p_var, width = 8, height = 7, dpi = 150)

############################################################
# CLUSTERING K-MEANS
############################################################

set.seed(123)
datos_cluster  <- scale(datos_num)
kmeans_result  <- kmeans(datos_cluster, centers = 3, nstart = 25)

p_cluster <- fviz_cluster(
  kmeans_result,
  data         = datos_cluster,
  geom         = "point",
  pointsize    = 1,
  alpha        = 0.4,
  ellipse.type = "convex",
  palette      = c(COLOR_AZUL, COLOR_NARANJA, "#27AE60"),
  title        = "Clustering K-Means (k = 3)"
) +
  tema_proyecto +
  theme(legend.position = "right")

ggsave("graficas/cluster_kmeans.png", p_cluster, width = 9, height = 7, dpi = 150)

############################################################
# TABLA FINAL
############################################################

tabla_final <- datos %>%
  group_by(churn) %>%
  summarise(
    satisfaction        = mean(satisfaction),
    engagement          = mean(engagement),
    subscription_length = mean(subscription_length),
    watch_time          = mean(watch_time),
    support_queries     = mean(support_queries),
    income              = mean(income)
  )

tabla_final
write.csv(tabla_final, "tabla_resumen_churn.csv")
