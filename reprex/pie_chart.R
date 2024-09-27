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


# Jeu de données fictif
data <- data.frame(
  navigateur = c("Chrome", "Safari", "Edge", "Firefox", "Opera"),
  part_marche = c(65, 15, 5, 7, 3)
)

# Modification du jeu de données pour définir l'ordre de l'axe des X
data$navigateur <- factor(data$navigateur, levels = c("Chrome", "Safari", "Edge", "Firefox", "Opera"))



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
    plot.title = element_text(
      #CSS du titre de l'axe font-family: Open Sans; font-weight: bold;font-size: 18px;line-height: 24px;color: #223654;
      family = "open-sans", face = "bold", size = 16, color = "#223654",
      margin = margin(t = 48, r = 0, b = 48, l = 0, unit = "pt")
    ),
    plot.caption = element_text(
      #CSS des informations complémentaires font-family: Open Sans; font-weight: italic;font-size: 14px;color: #6b778a;
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

