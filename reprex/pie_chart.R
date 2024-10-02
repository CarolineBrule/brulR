# Add font family ---------------------------------------------------------
# Charger les librairies
library(showtext)
library(sysfonts)

# Charger les polices Google Fonts
font_add_google("Roboto", "roboto")
font_add_google("Open Sans", "open-sans")

# Activer showtext
showtext_auto()


# Create dummy data -------------------------------------------------------

# Créer les données
data <- data.frame(
  tranche_age = c("0 à 19 ans", "20 à 64 ans", "65 ans et plus"),
  proportion = c(20.6, 58.6, 20.8)
)


px_to_pt <- function(px) {
  # Conversion de pixels en points (1 point = 0.75 pixel)
  return(px / 0.75)
}

# Trier les données en ordre décroissant (ou croissant)
data <- data %>%
  arrange(proportion) # Trier en ordre décroissant (ou utiliser 'asc(proportion)' pour croissant)

# Piechart ----------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Créer une marge de 4 pixels en ajustant la proportion
# On introduit une légère réduction de chaque proportion pour créer un espacement


# Créer le graphique en camembert avec étiquettes à l'extérieur
ggplot(data, aes(x = "", y = proportion, fill = tranche_age)) +
  geom_bar(width = 1, stat = "identity", color = "white", linewidth = 1.2) +  # taille de l'espacement
  coord_polar("y", start = pi/2) +  # Camembert en sens horaire, débutant en haut
  
  # coord_polar("y", start =0)  + #8.6
  scale_fill_manual(values = c("#95BFDD" ,"#095797", "#6194BF")) +   # Couleurs personnalisées pour chaque tranche
  labs(
    title = "Répartition de la population québécoise par tranches d'âge"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    # Titre du graphique
    plot.title = element_text(
      family = "open-sans", face = "bold", size = 16, color = "#223654", hjust = 0.5, # Style du titre
      margin = margin(t = px_to_pt(48), unit = "pt")  # Espace au-dessus du titre
    ),
    plot.margin = margin(t = px_to_pt(0), r = px_to_pt(90), b = px_to_pt(0), l = px_to_pt(90), unit = "pt"),
    # panel.spacing.x = unit(px_to_pt(46), "pt"),  # Espace entre le contenu du graphique et la bordure
    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),  # Fond blanc du panneau
    
    # Fond et bordures du graphique
    plot.background = element_rect(fill = NA, colour = "#c5cad2", linewidth = 1)  # Bordure grise autour du graphique
  ) +
  # xlim(-1.5, 2.5) +  # Ajuster les limites de l'axe x pour donner plus d'espace aux étiquettes
  expand_limits(x = 3) +  # Élargir les limites pour donner plus d'espace sur la droite

  geom_text(aes(x = 2.6,label = paste0(tranche_age, " : ", proportion, "%")), 
            position = position_stack(vjust = 0.5), color = "#6b778a", family = "open-sans",  size = 4) 
  


# Create plot w/ ggplot2 -------------------------------------------------------


# Chargement des librairies
library(ggplot2)
library(grid)


# Création du graphique
p <- ggplot(data, aes(x = navigateur, y = part_marche)) +
  geom_bar(stat = "identity", fill = "#095797",
           width = 0.67 #Cela réduit la largeur des colonnes à 67 % de leur taille d'origine, créant ainsi un espacement de 50 % entre les colonnes.
  ) +
  labs(
    title = "Part de marché des navigateurs",
    # subtitle = "Titre de l'axe",
    x = NULL,  # On retire l'axe des X
    y = NULL,
    caption = "Notes, références et sources du graphique"
  ) +
  # scale_x_discrete(limits = c(numeric_levels, non_numeric_levels), breaks = c(numeric_levels, non_numeric_levels), drop = FALSE) +
  scale_y_continuous(expand = c(0, 0),# Retirer l'espace en bas
                     limits = c(0,100),
                     breaks = c(0, 20, 40, 60, 80),  # Positions des labels
                     # labels = c("0", "20", "40", "60", "80 % d'utilisation par navigateur")  # Noms des labels
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(t = 72, r = 40, b = 72, l = 40, unit = "pt"),  # Dégagement de 40 px de chaque côté
    plot.background = element_rect(fill = NA, colour = "#c5cad2", linewidth = 1),  # Contour du plot avec couleur et épaisseur
    
    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5, colour = "#C5CAD2"), #CSS de la grille de fond border: 0.5px solid #c5cad2;
    axis.line.x = element_line(linewidth = 1, colour = "#223654"), #CSS de l'abscisse border: 1px solid #223654;
    axis.line.y = element_blank(), #pas d'axe y
    axis.ticks.y = element_blank(), #pas de ticks de x
    axis.title.x = element_blank(),  # Retrait du titre de l'axe X
    axis.title.y = element_blank(),  # Retrait du titre de l'axe Y
    
    # CSS des libellés et du titre de l'axe : font-family: Open Sans; font-size: 14px;  color: #6b778a;
    axis.text.x = element_text(family = "open-sans", size = 14, color = "#6b778a",
                               margin = margin(t = 8),  # Ajout d'un espace de 8 pixels entre l'axe X et les labels
                               hjust = 0.5 # Alignement des libellés sous chaque barre
    ),
    # axis.text.y = element_text(family = "open-sans", size = 14, color = "#6b778a"),
    
    #Cest bon mais j'en ai pas besoin live    
    # plot.subtitle = element_text( #correspond en fait au titre de l'axe y
    #   #CSS du titre de l'axe font-family: Open Sans; font-weight: semibold;font-size: 16px;line-height: 24px;color: #223654;
    #   family = "open-sans", face = "bold", size = 16, color = "#223654",lineheight = 24,
    #   margin = margin(b = 32)
    # ),
          #CSS du titre de l'axe font-family: Open Sans; font-weight: bold;font-size: 18px;line-height: 24px;color: #223654;

    plot.title = element_text(
      family = "open-sans", face = "bold", size = 16, color = "#223654",
      margin = margin(t = 48, r = 0, b = 48, l = 0, unit = "pt")
    ),      #CSS des informations complémentaires font-family: Open Sans; font-weight: italic;font-size: 14px;color: #6b778a;

    plot.caption = element_text(
      family = "open-sans", face = "italic", size = 14, color = "#6b778a",
      margin = margin(t = 80, r = 40, b = 48, l = 0 , unit = "pt"), hjust = 0
    )
  ) +
  annotate("text", x = 0, y = 80, label = "80 % d'utilisation par navigateur",
           hjust = 0, vjust = -0.5, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 0, y = 60, label = "60",
           hjust = 0, vjust = -0.5, size = 4, family = "open-sans", color = "#6b778a") +
  annotate("text", x = 0, y = 40, label = "40",
           hjust = 0, vjust = -0.5, size = 4, family = "open-sans", color = "#6b778a")+
  annotate("text", x = 0, y = 20, label = "20",
           hjust = 0, vjust = -0.5, size = 4, family = "open-sans", color = "#6b778a")+
  annotate("text", x = 0, y = 0, label = "0",
           hjust = 0, vjust = -0.5, size = 4, family = "open-sans", color = "#6b778a")

p

