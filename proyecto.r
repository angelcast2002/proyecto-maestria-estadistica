############################################################
# PROYECTO FINAL
# Statistical Analysis
# Retención y abandono de usuarios en servicios digitales
############################################################
install.packages(c(
  "readxl",
  "tidyverse",
  "psych",
  "corrplot",
  "car",
  "FactoMineR",
  "factoextra",
  "cluster",
  "nortest",
  "reshape2",
  "janitor"
))

library(readxl)
library(tidyverse)
library(psych)
library(corrplot)
library(car)
library(FactoMineR)
library(factoextra)
library(cluster)
library(nortest)
library(reshape2)
library(janitor)

dir.create("graficas", showWarnings = FALSE)

############################################################
# CARGA DE DATOS
############################################################

datos <- read_excel("netflix_large_user_data.xlsx")

names(datos) <- c(
  "customer_id",
  "subscription_length",
  "satisfaction",
  "watch_time",
  "engagement",
  "device",
  "genre",
  "region",
  "payment_history",
  "subscription_plan",
  "churn",
  "support_queries",
  "age",
  "income",
  "promo_offers",
  "profiles"
)

datos <- clean_names(datos)

datos$device <- as.factor(datos$device)
datos$genre <- as.factor(datos$genre)
datos$region <- as.factor(datos$region)
datos$payment_history <- as.factor(datos$payment_history)
datos$subscription_plan <- as.factor(datos$subscription_plan)
datos$churn <- as.factor(datos$churn)

############################################################
# INFORMACION GENERAL
############################################################

str(datos)

summary(datos)

colSums(is.na(datos))

############################################################
# ESTADISTICOS DESCRIPTIVOS
############################################################

estadisticos <- describe(
  datos %>%
    select(
      subscription_length,
      satisfaction,
      watch_time,
      engagement,
      support_queries,
      age,
      income,
      promo_offers,
      profiles
    )
)

write.csv(
  estadisticos,
  "estadisticos_descriptivos.csv"
)

############################################################
# HISTOGRAMAS
############################################################

numericas <- c(
  "subscription_length",
  "satisfaction",
  "watch_time",
  "engagement",
  "support_queries",
  "age",
  "income",
  "promo_offers",
  "profiles"
)

for(v in numericas){

  p <- ggplot(
    datos,
    aes_string(x = v)
  ) +
    geom_histogram(
      bins = 30
    ) +
    theme_minimal() +
    ggtitle(paste("Distribución de", v))

  ggsave(
    paste0(
      "graficas/hist_",
      v,
      ".png"
    ),
    p,
    width = 8,
    height = 5
  )

}

############################################################
# DENSIDADES
############################################################

for(v in numericas){

  p <- ggplot(
    datos,
    aes_string(x = v)
  ) +
    geom_density(
      fill = "steelblue",
      alpha = 0.4
    ) +
    theme_minimal()

  ggsave(
    paste0(
      "graficas/densidad_",
      v,
      ".png"
    ),
    p,
    width = 8,
    height = 5
  )

}

############################################################
# BOXPLOTS
############################################################

for(v in numericas){

  p <- ggplot(
    datos,
    aes_string(
      x = "churn",
      y = v
    )
  ) +
    geom_boxplot() +
    theme_minimal()

  ggsave(
    paste0(
      "graficas/boxplot_",
      v,
      ".png"
    ),
    p,
    width = 8,
    height = 5
  )

}

############################################################
# VARIABLES CATEGORICAS
############################################################

categoricas <- c(
  "device",
  "genre",
  "region",
  "payment_history",
  "subscription_plan",
  "churn"
)

for(v in categoricas){

  p <- ggplot(
    datos,
    aes_string(x = v)
  ) +
    geom_bar() +
    theme_minimal()

  ggsave(
    paste0(
      "graficas/barra_",
      v,
      ".png"
    ),
    p,
    width = 8,
    height = 5
  )

}

############################################################
# MATRIZ DE CORRELACION
############################################################

datos_num <- datos %>%
  select(
    subscription_length,
    satisfaction,
    watch_time,
    engagement,
    support_queries,
    age,
    income,
    promo_offers,
    profiles
  )

cor_matrix <- cor(datos_num)

write.csv(
  cor_matrix,
  "correlaciones.csv"
)

png(
  "graficas/correlacion.png",
  width = 1200,
  height = 900
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper"
)

dev.off()

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

t_satisfaction <- t.test(
  satisfaction ~ churn,
  data = datos
)

t_engagement <- t.test(
  engagement ~ churn,
  data = datos
)

t_subscription <- t.test(
  subscription_length ~ churn,
  data = datos
)

t_watch <- t.test(
  watch_time ~ churn,
  data = datos
)

t_support <- t.test(
  support_queries ~ churn,
  data = datos
)

t_satisfaction
t_engagement
t_subscription
t_watch
t_support

############################################################
# ANOVA
############################################################

anova_plan <- aov(
  satisfaction ~ subscription_plan,
  data = datos
)

summary(anova_plan)

TukeyHSD(anova_plan)

############################################################
# CHI CUADRADO
############################################################

chisq.test(
  table(
    datos$subscription_plan,
    datos$churn
  )
)

chisq.test(
  table(
    datos$payment_history,
    datos$churn
  )
)

chisq.test(
  table(
    datos$device,
    datos$churn
  )
)

chisq.test(
  table(
    datos$region,
    datos$churn
  )
)

############################################################
# REGRESION LOGISTICA
############################################################

datos$churn_bin <- ifelse(
  datos$churn == "Yes",
  1,
  0
)

modelo <- glm(
  churn_bin ~
    subscription_length +
    satisfaction +
    watch_time +
    engagement +
    support_queries +
    income +
    payment_history +
    subscription_plan,
  family = "binomial",
  data = datos
)

summary(modelo)

############################################################
# ODDS RATIOS
############################################################

odds_ratios <- exp(
  coef(modelo)
)

odds_ratios

write.csv(
  odds_ratios,
  "odds_ratios.csv"
)

############################################################
# INTERVALOS DE CONFIANZA
############################################################

exp(confint(modelo))

############################################################
# PREDICCIONES
############################################################

datos$prob_churn <- predict(
  modelo,
  type = "response"
)

############################################################
# PCA
############################################################

pca <- PCA(
  datos_num,
  graph = FALSE
)

fviz_pca_ind(pca)

fviz_pca_var(pca)

############################################################
# CLUSTERING
############################################################

set.seed(123)

datos_cluster <- scale(
  datos_num
)

kmeans_result <- kmeans(
  datos_cluster,
  centers = 3
)

fviz_cluster(
  kmeans_result,
  data = datos_cluster
)

############################################################
# TABLA FINAL PARA EL INFORME
############################################################

tabla_final <- datos %>%
  group_by(churn) %>%
  summarise(
    satisfaction = mean(satisfaction),
    engagement = mean(engagement),
    subscription_length = mean(subscription_length),
    watch_time = mean(watch_time),
    support_queries = mean(support_queries),
    income = mean(income)
  )

tabla_final

write.csv(
  tabla_final,
  "tabla_resumen_churn.csv"
)

