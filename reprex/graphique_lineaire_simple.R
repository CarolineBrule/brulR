# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Charger les librairies nécessaires
library(ggplot2)


px_to_pt <- function(px) {
  # Conversion de pixels en points (1 point = 0.75 pixel)
  return(px / 0.75)
}


# Créer le jeu de données
data <- data.frame(
  year = 2002:2013,
  Femmes = c(74.5, 75.0, 75.5, 76.0, 76.5, 77.0, 77.5, 78.0, 78.5, 79.0, 79.5, 80.0),
  Hommes = c(63.5, 64.0, 64.5, 65.0, 65.5, 66.0, 66.5, 67.0, 67.5, 68.0, 68.5, 69.0),
  Total = c(69.0, 69.5, 70.0, 70.5, 71.0, 71.5, 72.0, 72.5, 73.0, 73.5, 74.0, 74.5)
)

# Créer le graphique sans pivot_longer
p <- ggplot(data) +
  geom_line(aes(x = year, y = Femmes, color = "Femmes", linetype = "Femmes"), size = 1.2) +
  geom_line(aes(x = year, y = Hommes, color = "Hommes", linetype = "Hommes"), size = 1.2) +
  geom_line(aes(x = year, y = Total, color = "Total", linetype = "Total"), size = 1.2) +
  scale_color_manual(values = c("Femmes" = "#D0DC03", "Hommes" = "#095797", "Total" = "#DEDEE1")) +
  scale_linetype_manual(values = c("Femmes" = "solid", "Hommes" = "solid", "Total" = "dashed")) +
  scale_y_continuous(expand = c(0, 0),# Retirer l'espace en bas
                     limits = c(30,110),
                     breaks = c(30, 40,50, 60,70, 80, 90, 100)) +  # Positions des labels
  scale_x_continuous(breaks = c(2002:2013), 
                     limits = c(2001,2013),
                     expand = c(0,0)
                     ) +
  
  labs(
    title = "Taux de diplomation et de qualification 7 ans après \nl'entrée au secondaire",
    x = "",
    y = "",
    caption = "Notes, références et sources du graphique",
    color = "",
    linetype = ""
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = px_to_pt(0), r = px_to_pt(40), b = px_to_pt(0), l = px_to_pt(40), unit = "pt"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    plot.background = element_rect(fill = NA, colour = "#c5cad2", linewidth = 1),  # Bordure grise autour du graphique
    legend.position = "bottom",
    legend.justification = c(0, 0),  # Aligne la légende à gauche
    legend.box.just = "left",  # Assure que la boîte de la légende est à gauche
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(px_to_pt(48), "pt"),  # Espace entre le contenu du graphique et la bordure
    
    panel.grid.major.y = element_line(linewidth = 0.5, colour = "#C5CAD2"), #CSS de la grille de fond border: 0.5px solid #c5cad2;
    axis.line.x = element_line(linewidth = 1, colour = "#6B778A"),  # Ligne de l'axe des X avec une couleur spécifique
    plot.title = element_text(
      family = "open-sans", face = "bold", size = 16, color = "#223654",
      margin = margin(t = 48, r = 0, b = 48, l = 0, unit = "pt")
    ),
    plot.caption = element_text(
      family = "open-sans", face = "italic", size = 14, color = "#6b778a",
      margin = margin(t = px_to_pt(80), r = px_to_pt(40), b = px_to_pt(48), l = 0 , unit = "pt"), hjust = 0
    ),
    axis.ticks = element_line(linewidth = 1, color="#6B778A") , 
    axis.ticks.length.x = unit(px_to_pt(8), "pt"),
    
    axis.text.x = element_text(family = "open-sans", size = 12, color = "#6b778a",
                               margin = margin(t = px_to_pt(7), b = px_to_pt(32), unit = "pt"),  # Marges autour des étiquettes
                               hjust = 0.5 # Alignement des libellés sous chaque barre
    ),
    legend.text = element_text(family = "open-sans", size = 12, color = "#6b778a",
    ),
    legend.key.size = unit(px_to_pt(16), "pt"),
    legend.margin = margin(t = 0, b = 0), # Réduit la marge au-dessus de la légende
  ) +
  
  annotate("text", x = 2001, y = 100, label = "100 % de diplomation et de qualification 7 ans après l'entrée au secondaire",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 90, label = "90",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 80, label = "80",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 70, label = "70",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 60, label = "60",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 30, label = "30",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 40, label = "40",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 2001, y = 50, label = "50",
           hjust = 0, vjust = -0.6, size = 4, family = "open-sans", color = "#6b778a") 
p

# Utilisation de grid pour ajuster les marges externes autour du graphique ----------------
  # Ici, nous ajoutons une marge externe au graphique pour créer un espace supplémentaire en haut et en bas.
  # Cela est utile pour contrôler l'espacement dans des documents ou des présentations.
  
  grid.newpage()  # Crée une nouvelle page graphique avant de dessiner le graphique avec des marges supplémentaires

# Ajouter des marges externes autour du graphique ----------------------------------------
grid.draw(gridExtra::grid.arrange(
  p,  # Le graphique ggplot que nous avons créé
  top = textGrob(" ", gp = gpar(fontsize = px_to_pt(72))),   # Marge en haut de 72 pixels convertis en points
  bottom = textGrob(" ", gp = gpar(fontsize = px_to_pt(72))) # Marge en bas de 72 pixels convertis en points
))
