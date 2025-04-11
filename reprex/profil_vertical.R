## ----profil-vertical----
# Importer les bibliothèques nécessaires
library(ggplot2)  # Pour créer des graphiques avec ggplot2
library(grid)  # Pour manipuler des éléments graphiques (comme les marges, les lignes)
library(showtext)  # Pour utiliser des polices Google Fonts dans les graphiques
library(sysfonts)  # Pour charger des polices supplémentaires

# Charger la police Google Open Sans ---------------------------------------------------------
# Ici, nous utilisons la police "Open Sans" de Google Fonts, qui est bien adaptée pour un rendu clair
# et propre dans les graphiques formels.
font_add_google("Open Sans", "open-sans")
showtext_auto()  # Activer showtext pour que la police soit correctement rendue dans les graphiques

# Fonction utilitaire pour convertir des pixels en points ------------------------------------
# Cette fonction convertit des pixels en points, car ggplot fonctionne souvent avec des unités
# en points. (1 point = 0.75 pixel).
px_to_pt <- function(px) {
  return(px / 0.75)
}

# Création d'un jeu de données simulé --------------------------------------------------------
donnees <- data.frame(
  Profondeur = c(8, 7, 6, 5, 4, 3, 2, 1, 0),
  Oxygene = c(0, 5, 10, 20, 40, 60, 80, 90, 100)
)

# Création du thème personnalisé pour le graphique ------------------------------------------
# Nous créons un thème personnalisé pour harmoniser les éléments graphiques du diagramme.
custom_theme <- theme_void() +  # Commence par un thème vide sans éléments par défaut
  theme(
    # Marges autour du graphique (haut, droite, bas, gauche) en points
    plot.margin = margin(t = px_to_pt(48), r = px_to_pt(40), b = px_to_pt(48), l = px_to_pt(40), unit = "pt"),
    
    # Définir un fond blanc avec une bordure grise autour du graphique
    plot.background = element_rect(fill =  "#FFFFFF", colour = "#c5cad2", linewidth = 1),
    # Fond blanc pour le panneau du graphique
    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),  
    # Grille pour l'axe Y en ligne fine grise
    panel.grid.major.y = element_line(linewidth = 0.5, colour = "#C5CAD2"),
    # Grille pour l'axe X en ligne fine grise
    panel.grid.major.x = element_line(linewidth = 0.5, colour = "#C5CAD2"),
    # Ligne de l'axe X avec une couleur grise
    axis.line.x = element_line(linewidth = 1, colour = "#6B778A"),
    # Position de la légende en bas du graphique
    legend.position = "bottom",
    legend.justification = c(0, 0),  # Alignement à gauche
    
    
    # Style des étiquettes de l'axe des X
    axis.text.x = element_text(
      family = "open-sans", size = 14, color = "#6b778a",  # Police Open Sans, couleur grise
      margin = margin(t = px_to_pt(8), b = px_to_pt(16), unit = "pt"),  # Marges autour des étiquettes
      hjust = 0.5, vjust = 1
    ),
    
    # Style des étiquettes de l'axe des Y
    axis.text.y = element_text(
      family = "open-sans", size = 14, color = "#6b778a",  # Police Open Sans, couleur grise
      margin = margin(r = px_to_pt(8), l = px_to_pt(16), unit = "pt"),  # Marges autour des étiquettes
      hjust = 0.5  # Centrer les étiquettes de l'axe Y
    ),
    
    # Style du titre de l'axe des X
    axis.title.x = element_text(
      family = "open-sans", size = 14, color = "#6b778a",  # Police Open Sans
      margin = margin(b = px_to_pt(32), unit = "pt"),  # Marges sous le titre
      hjust = 0.5  # Centrer le titre
    ),
    
    # Style du titre de l'axe des Y
    axis.title.y = element_text(
      family = "open-sans", size = 14, color = "#6b778a", angle = 90,  # Angle de 90° pour le titre de l'axe Y
      hjust = 0.5  # Centrer verticalement
    ),
    
    # Style de la légende (sans titre et police Open Sans)
    legend.title = element_blank(),
    legend.text = element_text(family = "open-sans", size = 14, color = "#6b778a",
                               margin = margin(r = px_to_pt(8))  # Marge à droite pour espacer les éléments
    ),
    
    legend.key.size = unit(px_to_pt(16), "pt")  # Taille des éléments de la légende
  ) 

# Générer le graphique avec ggplot2 ---------------------------------------------------------

# Création du graphique
plot <- ggplot(donnees, aes(x = Oxygene, y = Profondeur)) +
  geom_line(linewidth = 1.2, color = "#095797") +
  geom_point(size = 3, color = "#095797") +
  scale_y_reverse(expand = c(0.01, 0.5),# Retirer l'espace en bas
                  limits = c(8,-0),
                  breaks = c(8, 6, 4, 2, 0)
                  ) +
  labs(
    x = "Oxygène dissous (%)",
    y = "Profondeur (m)"
  ) +
 
  scale_x_continuous(breaks = c(0,25,50,75,100), 
                     limits = c(-5,105),
                     expand = c(0,1)
  ) +
  custom_theme 

# Affichage du graphique
plot
