# =============================================================================
# Exemple : Afficher une icône de point différente entre le graphique et la légende
# Objectif : Afficher des symboles pleins (shape 15/16) sur la carte,
#            mais afficher des symboles vides (shape 0/1) dans la légende
# =============================================================================

# Chargement du package
library(ggplot2)

# ---- Données simulées ----
# Deux points correspondant à deux classes d’épaisseur de périphyton
df <- data.frame(
  x = 1:2,
  y = 1:2,
  classe = c("≥ 4 mm", "< 4 mm")  # classes d’interprétation
)

# ---- Définition des formes ----
# Formes utilisées pour afficher les points sur la carte
shape_vraie <- c("≥ 4 mm" = 15, "< 4 mm" = 16)  # carré plein, cercle plein

# Formes à afficher dans la légende seulement (formes vides)
shape_legende <- c("≥ 4 mm" = 0, "< 4 mm" = 1)  # carré vide, cercle vide

# ---- Création du graphique ----
ggplot(df, aes(x, y, shape = classe)) +
  geom_point(size = 6) +
  scale_shape_manual(
    values = shape_vraie,  # formes visibles sur la carte
    guide = guide_legend(
      override.aes = list(
        shape = unname(shape_legende)  # formes spécifiques dans la légende
      )
    )
  ) +
  labs(shape = "Épaisseur") +
  theme_minimal()
