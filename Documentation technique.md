# Documentation Technique – Application *GreenDPE Isère*

## 1. Présentation générale

L'application **GreenDPE Isère** est une application permettant de récupérer les **Diagnostics de Performance Énergétique (DPE)** des communes de l'Isère. Il s'en suit un traitement de ces données afin de produire des KPI incluant des tableaux de bord ainsi que des aides visuelles (graphiques et carte).  

Les données de *GreenDPE Isère* sont extraites de l’API **ADEME** mais provienne aussi d'un fichier local contenant des informations administratives quant aux adresses (commune, code commune BAN, coordonnées au format WGS84 etc).

---

## 2. Architecture de l’application

### 🔧 Structure générale

```


├Application/
│ ├── app.R                  # Fichier principal Shiny (UI + Server)
│ ├── data/
│ │ └── adresses_38.csv    # Coordonnées BAN des adresses
│ ├── www/
│ │ ├── logo.png
│ │ ├── pin_A.svg … pin_G.svg

```

### 🧩 Architecture logique

```
+----------------------------------------------------------+
|                        UI Shiny                          |
|  - Sélecteurs année & communes                            |
|  - KPI & visualisations (ggplot2)                         |
|  - Carte Leaflet                                          |
|  - Tableau DT                                             |
|  - Téléchargements / mode sombre                          |
+----------------------------------------------------------+
                         |
                         v
+----------------------------------------------------------+
|                        SERVER                            |
|  1. Chargement BAN local                                  |
|  2. Appels API ADEME Existant & Neuf                      |
|  3. Stockage réactif (reactiveVal)                        |
|  4. Filtres année/commune                                 |
|  5. Graphiques & KPI                                      |
|  6. Données cartographiques                               |
|  7. Régression linéaire                                   |
+----------------------------------------------------------+
                         |
                         v
+----------------------------------------------------------+
|                     Sortie vers UI                        |
+----------------------------------------------------------+
```

---

## 3. Installation locale

### Prérequis

- **R ≥ 4.0**
- **RStudio** (recommandé)
- Connexion Internet (pour les requêtes)

### 1) Installer les packages

```r
install.packages(c(
  "shiny", "jsonlite", "httr",
  "ggplot2", "dplyr", "leaflet", "DT"
))
```

### 2) Lancer l'application

```r
shiny::runApp()
```

## 4. Packages nécessaires

| Package | Rôle |
|--------|------|
| **shiny** | Interface web |
| **jsonlite** | Décodage JSON |
| **httr** | Appels API |
| **ggplot2** | Graphiques |
| **dplyr** | Manipulation des données |
| **leaflet** | Cartographie |
| **DT** | Tableau interactif |

---

## 5. Fonctionnement interne

### Chargement des données
- Récupération des coordonnées via `adresses_38.csv` et le code BAN de l'adresse
- Récupération des DPE via l’API de l'ADEME
- Stockage dans `rv_existant` et `rv_neuf`

### Filtres
- Année (`date_reception_dpe`)
- Communes (avec possibilité de les changer pour avoir d'autres communes que celles du lancement)

### Analyses
- Histogrammes (DPE / GES)
- Boîtes à moustache (pour visualiser la différence de répartition des consommations entre logements neufs et existants)
- KPI globaux et par type (neuf / existant)
- Régression linéaire + corrélation Pearson

### Cartographie
- Jointure sur le code BAN
- Icônes SVG personnalisées A → G
- Mise en place de cluster pour ne pas saturer la carte
- Fenêtre pop-up

---

## 6. Export

- **CSV** : données filtrées
- **PNG** : histogrammes DPE & GES
