# Script pour la création de dossiers par lac
# Auteur : Caroline Brule
# Date : 4 mars 2024

# Objectif : Ce script crée un dossier pour chaque lac dans un répertoire spécifique,
# afin de faciliter le regroupement et l'organisation des analyses de données par lac.
# Note importante : Ne pas exécuter ce script plusieurs fois afin d'éviter d'écraser 
# les dossiers existants et d'en supprimer le contenu.

# Chargement des bibliothèques nécessaires
library(dplyr)
library(fs) # Pour une gestion des fichiers plus robuste

# 1. Préparer les données des lacs ----
# Création d'un dataframe contenant les informations de base pour chaque lac
lacs <- tibble::tibble(
  no_lce = c("00001", "00002", "00003"),
  nom_du_lac = c("Lac A", "Lac B", "Lac C"),
  no_rsvl = c("0001", "0002", "0003")
)

# 2. Générer les noms de dossiers ----
# Combiner les informations pour créer un nom unique pour chaque dossier
lacs <- lacs %>%
  mutate(
    name_dossier = paste0(no_lce, "_", nom_du_lac, "_RSVL_", no_rsvl)
  )

# 3. Définir le répertoire cible ----
# Utilisation de file.path pour créer un chemin de fichier de manière robuste
repertoire <- file.path(
  "X:", "DOCUM", "4500_Produits_val_ajoutee", 
  "Eau_Suivi", "Lacs", "Périphyton", "Analyses de données"
)

# 4. Créer les dossiers ----
# Créer chaque dossier en utilisant fs::dir_create pour une gestion sécurisée
lacs %>%
  pull(name_dossier) %>%
  walk(~ fs::dir_create(path = file.path(repertoire, .x)))

# 5. Vérification finale ----
# Afficher la structure du dataframe pour vérifier les données traitées
str(lacs)
