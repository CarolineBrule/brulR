# Chargement des librairies
library(ggplot2)
library(ggeffects)

# Création du jeu de données fictif pour plusieurs années
donnees_glm_2019 <- data.frame(
  Annee = 2019,
  Mois = rep(6:10, each = 10),
  Volume_DO = pmax(0.5, rnorm(50, mean = 11 - rep(6:10, each = 10) * 0.8, sd = runif(1, 0.3, 0.7)))
)

donnees_glm_2020 <- data.frame(
  Annee = 2020,
  Mois = rep(6:10, each = 10),
  Volume_DO = pmax(0.5, rnorm(50, mean = 10 - rep(6:10, each = 10) * 1.2, sd = runif(1, 0.1, 0.8)))
)

donnees_glm_2021 <- data.frame(
  Annee = 2021,
  Mois = rep(6:10, each = 10),
  Volume_DO = pmax(0.5, rnorm(50, mean = 14 - rep(6:10, each = 10) * 1.6, sd = runif(1, 0.8, 1.5)))
)

# Combiner les données des différentes années
donnees_glm <- rbind(donnees_glm_2019, donnees_glm_2020, donnees_glm_2021)

# Ajustement du modèle GLM
model <- glm(Volume_DO ~ Mois + Annee, data = donnees_glm, family = gaussian())

# Prédictions avec ggpredict
predictions <- ggpredict(model, c("Mois [6:10]", "Annee"), ci_level = 0.95)

# Création du graphique avec les prédictions
plot <- ggplot(predictions, aes(x = x, y = predicted, color = group)) +
  scale_x_continuous(breaks = 6:10, labels = month.abb[6:10]) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1) +
  labs(
    y = "Volume hypolimnétique DO (mg/L)",
    color = "Année",
    fill = "Année"
  ) +
  theme_minimal() + scale_y_continuous(limits = c(0, 14))

# Affichage du graphique
plot
