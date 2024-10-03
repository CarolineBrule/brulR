## ----graph-olivier1----
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
# Ce jeu de données est utilisé pour reproduire la figure avec des mesures de carbone organique
# par station.
set.seed(123)  # Fixer une graine pour garantir la reproductibilité des résultats

# Définir les noms des stations
stations <- c("Bree02A", "Bree02B", "Bree03", "Bree04", "Bree05", 
              "Tru01", "Tru02", "Tru03", "Tru04", "Tru05")

# Définir les valeurs de carbone organique total (%)
carbone_organique <- c(2, 6, 1.5, 4, 12, 
                       3, 2.5, 4.5, 5, 4)

# Ajouter des barres d'erreur simulées (écart-type) pour chaque mesure
erreur <- c(0.5, 1.5, 0.2, 0.8, 3.0, 
            0.8, 0.6, 1.2, 1.0, 0.8)

# Localisation des stations (lac Bree ou lac Truite)
localisation <- rep(c("Bree", "Tru"), each = 5)

# Création du data.frame avec les colonnes nécessaires
data <- data.frame(Station = stations, 
                   Carbone_organique = carbone_organique, 
                   Erreur = erreur, 
                   Lac = localisation)

# Afficher les premières lignes du jeu de données pour vérifier sa structure
head(data)

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
    # Ligne de l'axe X avec une couleur grise
    axis.line.x = element_line(linewidth = 1, colour = "#6B778A"),
    # Position de la légende en bas du graphique
    legend.position = "bottom",
    legend.justification = c(0, 0),  # Alignement à gauche
    
    
    # Style des étiquettes de l'axe des X
    axis.text.x = element_text(
      family = "open-sans", size = 14, color = "#6b778a",  # Police Open Sans, couleur grise
      margin = margin(t = px_to_pt(8), b = px_to_pt(16), unit = "pt"),  # Marges autour des étiquettes
      hjust = 0.5  # Centrer les étiquettes sous chaque barre
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
ggplot(data, aes(x = Station, y = Carbone_organique, fill = Lac)) +  # Variable x: Station, y: Carbone organique
  geom_bar(stat = "identity", position = "dodge", width = 0.67) +  # Barres pour chaque station
  geom_errorbar(aes(ymin = Carbone_organique - Erreur, ymax = Carbone_organique + Erreur), 
                width = 0.2, color = "#223654") +  # Ajout des barres d'erreur
  labs(x = "Station", y = "Carbone organique total (%)") +  # Labels pour les axes
  scale_y_continuous(
    limits = c(0, 21),  # Limiter l'axe des Y de 0 à 21
    breaks = c(0, 5, 10, 15, 20),  # Intervalles des graduations de l'axe Y
    expand = c(0, 0)  # Retirer l'espace vide en bas de l'axe
  ) +
  scale_x_discrete(labels = gsub("(Bree|Tru)", "", stations)) +  # Enlever "Bree" et "Tru" des étiquettes de l'axe X
  scale_fill_manual(
    values = c("Bree" = "#D56050", "Tru" = "#68CEBA"),  # Définir les couleurs pour les lacs Bree et Tru
    labels = c("Bree" = "Lac Breeches", "Tru" = "Lac Truite")  # Modifier les labels de la légende
  ) +  
  custom_theme  # Appliquer le thème personnalisé
