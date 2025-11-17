# Documentation Fonctionnelle de *GreenDPE Isère*

## 1. Objectif général de l’application

**GreenDPE Isère** permet d'analyser et visualiser à l'aide de KPI les **Diagnostics de Performance Énergétique (DPE)** de l’Isère.  

On y retrouve des outils pour :

- Explorer les performances énergétiques des logements.
- Comparer les logements existants avec les logements neufs.
- Visualiser les données sur une carte.
- Étudier des corrélations statistiques entre les indicateurs (consommation, émissions, surface…).
- Télécharger les données et graphiques selon les filtres appliqués.

---

# 2. Parcours utilisateur et intérêt de chaque page

## 2.1. Page **DPE en Isère**  
**Objectif : fournir une vue d’ensemble des DPE disponibles et permettre une analyse détaillée par année et par commune.**

### Cette page permet :
###  Filtrer les DPE  
- **Par année** : Choix d'une année précise ou tout l’historique (de 2021 à aujourd'hui).  
- **Par communes** : sélectionner une ou plusieurs communes (ou tous les DPE de l'Isère).  
Les filtres s’appliquent à l’ensemble des graphiques et indicateurs.

###  Consulter les indicateurs clés (KPI)
- Nombre total de DPE
- Répartition Existant / Neuf (en %)
- Étiquette DPE moyenne (A=1 … G=7)
- GES moyen (kg CO₂/m²/an)
- Consommation moyenne (kWh/m²/an)
- KPIs dédiés aux logements existants et neufs

Ces indicateurs permettent une première compréhension du niveau de performance énergétique du territoire.

###  Visualiser les statistiques globales
- **Évolution cumulative du nombre de DPE** par année  
  Avantage : Suivre le volume de diagnostics réalisés dans le temps.
- **Diagramme circulaire Existant / Neuf**  
  Avantage : vue rapide de la proportion des deux types de logements.

### Analyser les étiquettes DPE & GES
- Histogrammes comparant Existant Neuf et Total
- Téléchargement PNG disponible pour les deux histogrammes

###  Comprendre la distribution des consommations
- Deux boîtes à moustache (une pour les logements existants et la seconde pour les logements neufs) 
  Objectif : observer les écarts de performances entre les deux types de logements.

###  Télécharger les données filtrées
- Export CSV des données visibles (en fonction des filtres choisis)

---

##  2.2. Page **Cartographie et corrélations**

###  Objectif : visualiser les logements géolocalisés et analyser les relations entre variables.

---

###  Carte interactive Leaflet

La carte représente :
- chaque logement via une **épingle colorée** selon l’étiquette DPE (de A à G), l'étiquette respecte le code couleur des DPE (vert foncé à rouge foncé)
- des clusters pour améliorer la visibilité de la carte

Chaque point de la carte permet d’afficher :
- l’adresse
- le type de bâtiment
- les étiquettes DPE et GES
- la consommation énergétique
- les émissions GES

La carte permet de localiser d'éventuelles zones énergivores.

---

###  Module de corrélation et régression linéaire

Permet de sélectionner deux variables quantitatives parmi :
- consommation énergétique
- émissions GES totales
- surface habitable
- émissions GES chauffage

L’outil fournit :
- un nuage de points
- une régression linéaire et sa droite de régression
- l’équation du modèle
- la corrélation (r) et le R²
- une interprétation automatique de la force de corrélation (des outils d'interprétations plus précis devraient être développés).

L'outil est utile remarquer des corrélations entre les indicateurs DPE (par exemple : consommation et émissions).

---

##  2.3. Page **Contexte**

### Objectif : fournir les informations nécessaires à la compréhension des données et afficher le tableau des données.

Cette page contient :
- Un texte explicatif sur les sources, les calculs (ex. moyenne des étiquettes), la logique de filtrage et l’utilisation générale.
- Le tableau fusionné des DPE existants et neufs avec la possibilité de trier les données par colonnes et aussi de rechercher des données.

---

# 3. Fonctionnalités majeures de l’application

##  Gestion dynamique des filtres
- Sélection de plusieurs communes
- Sélection des années
- Rechargement automatique des données manquantes (lors d'un changement de filtre)
- Rafraîchissement manuel (bouton refresh)

##  Mode sombre / clair
- Changement possible via un bouton dédié
- Thème appliqué aux graphiques

##  Production automatique d’indicateurs et graphiques
- KPI globaux
- KPI par type de logement
- Histogrammes DPE & GES
- Boîtes à moustache
- Courbe temporelle cumulative
- Diagramme circulaire

##  Cartographie avancée
- Icônes SVG personnalisées par étiquette DPE
- Popup détaillée
- Clusters
- Légende en pop-up

##  Statistiques
- Régression linéaire sur variables choisies
- Coefficient de Pearson
- R²
- Interprétation automatique 

##  Téléchargements
- Données filtrées (en .csv)
- Histogrammes (en .png)

##  Rafraîchissment des données
- Appels API ADEME
- Ajout automatique des communes supplémentaires à la demande
- Possibilité de raffraîchir les données via le bouton dédié

---

# 4. Conclusion

**GreenDPE Isère** fournit un ensemble complet d’outils permettant :  
  - d’explorer les performances énergétiques du territoire,  
  - de comparer les logements existants et neufs,  
  - de visualiser sur une carte les DPE,  
  - et d’étudier des relations statistiques entre variables.
