library(shiny)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)

######################### Chargement des Données ##############################

adresses_38 = read.csv("data/adresses_38.csv", sep = ";", header = TRUE)

########### Recuperation et traitement des coordonnees
adresses_38$lon = as.numeric(gsub(",", ".", adresses_38$lon))
adresses_38$lat = as.numeric(gsub(",", ".", adresses_38$lat))

######### Recuperer code INSEE des commune de l'Isere
all_codes = sort(unique(as.character(adresses_38$code_insee)))

############################## Donnees nécessaires au processus de demarage (sur 5 communes)
codes_initial = c("38185", "38421", "38151", "38544", "38053")
liste_code_insee = codes_initial

annee_courante = as.numeric(format(Sys.Date(), "%Y"))
annees = 2021:annee_courante

palette_ch = c("#F6F5F2", "#A72703", "#F9DCC4", "#6FB6B4")

########### Necessaire pour convertir les etiquette DPE en nombre et calculer des etiquettes moyennes
convert_dpe = function(x){
  recode = c(A=1,B=2,C=3,D=4,E=5,F=6,G=7)
  recode[x]
}

# Reference commune en vue du filtre
communes_ref = adresses_38 %>%
  select(code_insee, nom_commune) %>%
  distinct()

# Communes chargées au démarrage
initial_communes = communes_ref %>%
  filter(code_insee %in% codes_initial) %>%
  pull(nom_commune)


communes_choices = sort(unique(communes_ref$nom_commune))

################# ZONE Ui #######################

ui = fluidPage(
  ########################################## Page CSS pour les styles ###############
  tags$head(tags$style(HTML("
    .metric-box {
      background:#f8f9fa; padding:20px; border-radius:10px;
      text-align:center; box-shadow:0 0 5px #ccc;
      margin-bottom:20px; min-height:160px;
      display:flex; flex-direction:column; justify-content:center;
    }
    body.dark-mode { background-color:#1e1e1e !important; color:#f1f1f1 !important; }
    body.dark-mode .metric-box {
      background:#2c2c2c !important; color:#f1f1f1 !important;
      box-shadow:0 2px 6px rgba(255,255,255,0.1) !important;
    }
    .theme-switch, .refresh-button {
      cursor:pointer; padding:8px 15px; border-radius:8px;
      background:#e9ecef; margin-left:10px;
    }
    body.dark-mode .theme-switch, body.dark-mode .refresh-button {
      background:#444 !important; color:white !important;
    }
  "))),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggle-dark-mode', function(message) {
        document.body.classList.toggle('dark-mode');
    });
  ")),
  
  ####### Logo a gauche et son titre
  div(
    style = "display:flex; align-items:center; gap:15px; margin:20px 0 10px 10px;",
    tags$img(src="logo.png", height="55px"),
    tags$h1("GreenDPE Isère", style="margin:0;")
  ),
  
  tabsetPanel(
    
    ############################################################################################
    ################################### Premier onglet##########################################
    tabPanel(
      "DPE en Isère",
      br(),
      
      fluidRow(
        column(9, h3(strong("Vue d'ensemble"))),
        column(3, div(style="text-align:right;",
                      actionButton("refresh_data", label=NULL, icon=icon("rotate-right"), class="refresh-button"),
                      actionButton("toggle_theme", label=NULL, class="theme-switch", icon=icon("sun"))
        ))
      ),
      
      br(),
      
      #########Zone selection annee et commune et telechargement csv
      # Filtre année
      fluidRow(
        column(
          3,
          selectInput(
            "annee_filtre",
            "Année à analyser :",
            choices = c("Tout", as.character(2021:annee_courante)),
            selected = "Tout"
          )
        ),
        
        #filtre commune
        column(
          5,
          selectInput(
            "communes_filtre",
            "Communes à inclure :",
            choices  = c("Tout", communes_choices),
            selected = initial_communes,  # = les 5 communes chargées
            multiple = TRUE
          )
        ),
        # Telechargement donnees selectionnees csv
        column(
          4,
          br(),
          downloadButton("download_filtered_data", "Télécharger les données filtrées")
        )
      ),
      
      br(),
      
      ######### Mon premier KPI pour les infos generales
      fluidRow(
        column(4, div(class="metric-box", h3("Nombre total de DPE"), textOutput("nb_total_dpe"))),
        column(4, div(class="metric-box", h3("Part logements existants"), textOutput("pct_anciens"))),
        column(4, div(class="metric-box", h3("Part logements neufs"), textOutput("pct_neufs"))),
        
        column(4, div(class="metric-box", h3("Étiquette moyenne"), textOutput("dpe_moyen"))),
        column(4, div(class="metric-box", h3("GES moyen"), textOutput("ges_moyen"))),
        column(4, div(class="metric-box", h3("Consommation moyenne"), textOutput("conso_moyen")))
      ),
      
      br(), br(),
      ################## Premier graphique sur les donnees generales #####€#############
      h3(strong("Visualisation générale")),
      br(),
      
      #Zone graphique
      fluidRow(
        column(6, div(class="metric-box",
                      h4("Évolution cumulative du nombre de DPE"),
                      plotOutput("plot_dpe_annees", height="300px"))),
        column(6, div(class="metric-box",
                      h4("Répartition"),
                      plotOutput("plot_pie_ex_neuf", height="300px")))
      ),
      
      br(), br(),
      
      ############## Mon KPI pour les logements existants
      h3(strong("Vue logements existants")),
      br(),
      fluidRow(
        column(4, div(class="metric-box", h3("Consommation"), textOutput("conso_existant"))),
        column(4, div(class="metric-box", h3("GES"), textOutput("ges_existant"))),
        column(4, div(class="metric-box", h3("Étiquette moyenne"), textOutput("dpe_existant")))
      ),
      
      br(), br(),
      
      ################ Mon KPI (consommation, emission CO2 et etiquette moyenne) pour les logements neufs
      h3(strong("Vue logements neufs")),
      br(),
      fluidRow(
        column(4, div(class="metric-box", h3("Consommation"), textOutput("conso_neuf"))),
        column(4, div(class="metric-box", h3("GES"), textOutput("ges_neuf"))),
        column(4, div(class="metric-box", h3("Étiquette moyenne"), textOutput("dpe_neuf")))
      ),
      
      br(), br(),
      
      h3(strong("Répartition des étiquettes DPE et GES")),
      br(),
      
      ############################# ZONE HISTOGRAMMES ######################
      fluidRow(
        column(
          6,
          div(class="metric-box",
              h4("Histogramme des étiquettes DPE"),
              plotOutput("plot_histogram_dpe", height="400px"),
              br(),
              downloadButton("download_hist_dpe", "Télécharger l'histogramme DPE") #Bouton telechargeant le graphique
          )
        ),
        #Histogramme GES
        column(
          6,
          div(class="metric-box",
              h4("Histogramme des étiquettes GES"),
              plotOutput("plot_histogram_ges", height="400px"),
              br(),
              downloadButton("download_hist_ges", "Télécharger l'histogramme GES") #Btn pour png
          )
        )
      ),
      
      br(), br(),
      
      ########################### ZONE BOITE A MOUSTACHE (voir la difference de distribution)
      h3(strong("Distribution des consommations énergétiques")),
      br(),
      
      fluidRow(
        column(
          6,
          div(class="metric-box",
              h4("Consommation — Logements existants"),
              plotOutput("plot_box_conso_existant", height="350px")
          )
        ),
        column(
          6,
          div(class="metric-box",
              h4("Consommation — Logements neufs"),
              plotOutput("plot_box_conso_neuf", height="350px")
          )
        )
      )
    ),
    
    ################################################## CARTOGRAPHIE ET REGRESSIONS ###########
    
    tabPanel(
      "Cartographie et corrélations",
      br(),
      
      h3(strong("Cartographie DPE")),
      br(),
      ################### Zone carte ###################################"
      div(
        class = "metric-box",
        leafletOutput("map_dpe", height = "600px")
      ),
      
      br(), br(),
      h3(strong("Corrélations & régressions linéaires")),
      br(),
      
      ################### Zone regression ################
      fluidRow(
        column(
          4,
          selectInput(
            "var_x",
            "Variable en abscisse (X) :",
            choices = c(
              "Conso énergétique"         = "conso_5_usages_par_m2_ep",
              "Émissions GES totales"     = "emission_ges_5_usages_par_m2",
              "Surface habitable"         = "surface_habitable_logement",
              "Émissions GES chauffage"   = "emission_ges_chauffage"
            ),
            selected = "conso_5_usages_par_m2_ep"
          )
        ),
        column(
          4,
          selectInput(
            "var_y",
            "Variable en ordonnée (Y) :",
            choices = c(
              "Conso énergétique"         = "conso_5_usages_par_m2_ep",
              "Émissions GES totales"     = "emission_ges_5_usages_par_m2",
              "Surface habitable"         = "surface_habitable_logement",
              "Émissions GES chauffage"   = "emission_ges_chauffage"
            ),
            selected = "emission_ges_5_usages_par_m2"
          )
        )
      ),
      
      br(),
      div(
        class = "metric-box",
        plotOutput("plot_corr", height = "450px")
      ),
      
      br(),
      div(
        class = "metric-box",
        htmlOutput("resultats_reg")
      )
    ),
    
    ########################################## Onglet explication ##########################
    
    tabPanel(
      "Contexte",
      br(),
      h3(strong("Explications")),
      br(),
      div(
        class = "metric-box",
        HTML("
          <p>
          Cette application GreenDPE Isère permet d'explorer les diagnostics de performance énergétique (DPE)
          des logements existants et neufs sur une sélection de communes de l'Isère.
          </p>
          <p>
          L'onglet <b>“DPE en Isère”</b> présente des indicateurs globaux (KPI), des histogrammes d'étiquettes
          DPE/GES et des distributions de consommation. Un sélecteur d'année permet de filtrer les résultats
          par année de réalisation du DPE. 
          Les vignettes <b>étiquettes moyennes</b> sont à comprendre grâce à la manipulation suivante : on converti chaque étiquette par un nombre (exemple : A devient 1, B devient 2 etc) et on calcule la moyenne. </br> Il est aussi possible de sélectionner les communes analysées ainsi que les années souhaitées.</br><b>Dans le cas d'un chargement massif de données, il faut attendre un certain délai de traitement aussi bien pour le rafraîchissement des KPI que pour l'actualisation de la carte.</b>
          </p>
          <p>
          L'onglet <b>“Cartographie et corrélations”</b> affiche les DPE géolocalisés sur une carte interactive,
          ainsi qu'un module de corrélation entre différentes variables quantitatives (consommation, émissions, surface, etc.)
          avec une régression linéaire et des indicateurs de corrélation.
          </p>
          <p>
          Enfin, cette page <b>“Contexte”</b> documente les données utilisées et permet de visualiser le tableau fusionné
          des DPE existants et neufs.
          </p>
          <p>
          Des boutons permettent de télécharger en .csv le jeu de donner ou de télécharger les graphiques. Il est possible d'utiliser un filtre afin de choisir les données de certaines années.
          </p>
        ")
      ),
      
      br(),
      h3(strong("Données fusionnées (Existant + Neuf)")),
      br(),
      div(
        class = "metric-box",
        DTOutput("table_dpe")
      )
    )
  )
)

#########################################################################################
#########################################################################################
#########################################################################################
############################## SERVER ###################################################
##########################################################################################

server = function(input, output, session) {
  
  #Liens API
  url_existant = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
  url_neuf     = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"
  
  #ReactiveVals
  rv_existant = reactiveVal(data.frame())
  rv_neuf     = reactiveVal(data.frame())
  
  getPlotTheme = function(toggle_value) {
    if (toggle_value %% 2 == 1) theme_dark(base_size = 14)
    else theme_minimal(base_size = 14)
  }
  
  ########################################### Récupération des données via l'API ###############
  
  # Fonction qui recupere les DPE en fonction de neuf ou existant (lien), ainsi que de la commune et de l'annee
  dpe_commune_annee = function(url, code_insee, annee) {
    
    all_data = list()
    page = 1
    size = 10000
    execute = FALSE
    
    while (!execute) {
      params = list(
        page = page, size = size,
        select = paste(
          "numero_dpe,nom_commune_brut,code_insee_ban,etiquette_dpe,date_reception_dpe,",
          "conso_5_usages_par_m2_ep,emission_ges_5_usages_par_m2,cout_total_5_usages,",
          "etiquette_ges,identifiant_ban,type_batiment,adresse_ban,",
          "surface_habitable_logement,emission_ges_chauffage",
          sep = ""
        ),
        qs = paste0(
          'code_insee_ban:"', code_insee,
          '" AND date_reception_dpe:[', annee, '-01-01 TO ', annee, '-12-31]'
        )
      )
      
      url_encoded = modify_url(url, query = params)
      response = GET(url_encoded)
      if (response$status_code != 200) break
      
      content = fromJSON(rawToChar(response$content), flatten = TRUE)
      lignes = content$result
      
      if (length(lignes) == 0) { 
        execute = TRUE 
      } else {
        all_data[[page]] = lignes
        page = page + 1
        if (nrow(lignes) < size) execute = TRUE
      }
    }
    
    if (length(all_data) == 0) return(NULL)
    
    df = do.call(rbind, all_data)
    
    if (!"emission_ges_5_usages_par_m2" %in% names(df))
      df$emission_ges_5_usages_par_m2 = NA_real_
    if (!"conso_5_usages_par_m2_ep" %in% names(df))
      df$conso_5_usages_par_m2_ep = NA_real_
    if (!"surface_habitable_logement" %in% names(df))
      df$surface_habitable_logement = NA_real_
    if (!"emission_ges_chauffage" %in% names(df))
      df$emission_ges_chauffage = NA_real_
    
    return(df)
  }
  
  # Cette fonction est chargee de telecharger tous les codes DPE pour une liste de communes et pour toutes les annees dispos
  get_dpe = function(version = "existant", codes) {
    codes = unique(as.character(codes))
    if (length(codes) == 0) return(data.frame())
    
    url = if (version == "existant") url_existant else url_neuf
    big_list = list()
    
    for (code_insee in codes) {
      for (annee in annees) {
        lignes = dpe_commune_annee(url, code_insee, annee)
        if (!is.null(lignes)) {
          big_list[[paste0(code_insee, "_", annee, "_", version)]] = lignes
        }
      }
    }
    
    big_list = big_list[!sapply(big_list, is.null)]
    if (length(big_list) == 0) return(data.frame())
    
    do.call(rbind, big_list)
  }
  
  ##################### Partie de chargement des données (barre) #############
  #################### Probleme : latence rencontree #########################
  
  load_all_dpe = function() {
    withProgress(message = "Données en cours de chargement...", value = 0, {
      incProgress(0.1, detail = "Logements existants")
      df_existant = get_dpe("existant", liste_code_insee)
      incProgress(0.6, detail = "Logements neufs")
      df_neuf = get_dpe("neuf", liste_code_insee)
      incProgress(0.2, detail = "Finalisation")
      
      rv_existant(df_existant)
      rv_neuf(df_neuf)
    })
    showNotification("Chargement des données terminé !", duration = 4, type = "message")
  }
  
  #Chargement automatique au démarrage
  observeEvent(TRUE, {
    load_all_dpe()
  }, once = TRUE)
  
  # Bouton de rafraîchissement
  observeEvent(input$refresh_data, {
    showNotification("Actualisation des données...", id="refresh_note", duration=NULL)
    load_all_dpe()
    removeNotification("refresh_note")
    showNotification("Mise à jour terminée !", duration=3)
  })
  
  ######################## Mode sombre ou clair #################
  
  observeEvent(input$toggle_theme, {
    session$sendCustomMessage("toggle-dark-mode", list())
  })
  
  ######################### Ajout au df des nouvelles communes selectionnees #########
  
  load_dpe_for_codes = function(codes_to_load) {
    codes_to_load = unique(as.character(codes_to_load))
    if (length(codes_to_load) == 0) return(invisible(NULL))
    
    ex_cur = rv_existant()
    nf_cur = rv_neuf()
    
    codes_ex_deja = if (nrow(ex_cur) > 0) unique(as.character(ex_cur$code_insee_ban)) else character(0)
    codes_nf_deja = if (nrow(nf_cur) > 0) unique(as.character(nf_cur$code_insee_ban)) else character(0)
    
    codes_ex_a_charger = setdiff(codes_to_load, codes_ex_deja)
    codes_nf_a_charger = setdiff(codes_to_load, codes_nf_deja)
    
    withProgress(message = "Chargement de nouvelles communes...", value = 0, {
      if (length(codes_ex_a_charger) > 0) {
        incProgress(0.4, detail = "Logements existants")
        nouv_ex = get_dpe("existant", codes_ex_a_charger)
        if (nrow(nouv_ex) > 0) {
          rv_existant(bind_rows(ex_cur, nouv_ex))
        }
      }
      if (length(codes_nf_a_charger) > 0) {
        incProgress(0.4, detail = "Logements neufs")
        nouv_nf = get_dpe("neuf", codes_nf_a_charger)
        if (nrow(nouv_nf) > 0) {
          rv_neuf(bind_rows(nf_cur, nouv_nf))
        }
      }
    })
    
    showNotification("Nouvelles communes chargées.", duration = 4)
  }
  
  ############################# Mise en place filtre par commune ##############
  
  codes_communes_selectionnees = reactive({
    #Si "Tout" est coché ou rien n'est sélectionné on ne met pas de filtre de communes
    if (is.null(input$communes_filtre) || "Tout" %in% input$communes_filtre) {
      return(NULL)
    }
    
    communes_ref %>%
      filter(nom_commune %in% input$communes_filtre) %>%
      pull(code_insee) %>%
      unique()
  })
  
  #Chargement des donnees manquantes
  observeEvent(input$communes_filtre, {
    if (is.null(input$communes_filtre)) return(NULL)
    
    #Toutes les donnees du 38
    if ("Tout" %in% input$communes_filtre) {
      codes_voulu = all_codes
    } else {
      codes_voulu = communes_ref %>%
        filter(nom_commune %in% input$communes_filtre) %>%
        pull(code_insee) %>%
        as.character() %>%
        unique()
    }
    
    load_dpe_for_codes(codes_voulu)
  }, ignoreInit = TRUE)
  
  ############################ Fusion au tableau
  
  dpe_all = reactive({
    ex = rv_existant()
    nf = rv_neuf()
    if (nrow(ex) == 0 & nrow(nf) == 0) return(data.frame())
    
    #Filtre communes
    codes_sel = codes_communes_selectionnees()
    if (!is.null(codes_sel)) {
      ex = ex %>% filter(as.character(code_insee_ban) %in% as.character(codes_sel))
      nf = nf %>% filter(as.character(code_insee_ban) %in% as.character(codes_sel))
    }
    
    ex2 = if (nrow(ex) > 0) ex %>% mutate(type_logement = "Existant") else NULL
    nf2 = if (nrow(nf) > 0) nf %>% mutate(type_logement = "Neuf") else NULL
    
    bind_rows(ex2, nf2)
  })
  
  output$table_dpe = renderDT({
    datatable(
      dpe_all(),
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })
  
  ############################# Filtre sur les années
  
  filtered_existant = reactive({
    df = rv_existant()
    if (nrow(df) == 0) return(df)
    
    #Filtre année
    if (!is.null(input$annee_filtre) && input$annee_filtre != "Tout") {
      an = suppressWarnings(as.numeric(substr(df$date_reception_dpe, 1, 4)))
      df = df[an == as.numeric(input$annee_filtre), , drop = FALSE]
    }
    
    #Filtre communes (par l'intermediaire du code_insee_ban)
    codes_sel = codes_communes_selectionnees()
    if (!is.null(codes_sel)) {
      df = df %>% filter(as.character(code_insee_ban) %in% as.character(codes_sel))
    }
    
    df
  })
  
  filtered_neuf = reactive({
    df = rv_neuf()
    if (nrow(df) == 0) return(df)
    
    #Filtre année
    if (!is.null(input$annee_filtre) && input$annee_filtre != "Tout") {
      an = suppressWarnings(as.numeric(substr(df$date_reception_dpe, 1, 4)))
      df = df[an == as.numeric(input$annee_filtre), , drop = FALSE]
    }
    
    #Filtre communes
    codes_sel = codes_communes_selectionnees()
    if (!is.null(codes_sel)) {
      df = df %>% filter(as.character(code_insee_ban) %in% as.character(codes_sel))
    }
    
    df
  })
  
  ###################### Telechargement des donnees filtrees €€€#####################
  
  output$download_filtered_data = downloadHandler(
    filename = function() {
      if (input$annee_filtre == "Tout") {
        "DPE_Isere_toutes_annees.csv"
      } else {
        paste0("DPE_Isere_", input$annee_filtre, ".csv")
      }
    },
    content = function(file) {
      df = bind_rows(
        filtered_existant() %>% mutate(type_logement = "Existant"),
        filtered_neuf()     %>% mutate(type_logement = "Neuf")
      )
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  #########################################################################################
  ################################ ZONE KPI ###############################################
  
  # KPI generaux
  output$nb_total_dpe = renderText(nrow(filtered_existant()) + nrow(filtered_neuf()))
  
  output$pct_anciens = renderText({
    n_ex = nrow(filtered_existant())
    n_nf = nrow(filtered_neuf())
    if ((n_ex + n_nf) == 0) return("NA")
    pct = n_ex / (n_ex + n_nf) * 100
    paste0(round(pct,2), " %")
  })
  
  output$pct_neufs = renderText({
    n_ex = nrow(filtered_existant())
    n_nf = nrow(filtered_neuf())
    if ((n_ex + n_nf) == 0) return("NA")
    pct = n_nf / (n_ex + n_nf) * 100
    paste0(round(pct,2), " %")
  })
  
  output$dpe_moyen = renderText({
    df = rbind(filtered_existant(), filtered_neuf())
    if (nrow(df) == 0) return("NA")
    df$score = convert_dpe(df$etiquette_dpe)
    round(mean(df$score, na.rm=TRUE),2)
  })
  
  output$ges_moyen = renderText({
    x = suppressWarnings(as.numeric(c(filtered_existant()$emission_ges_5_usages_par_m2,
                                      filtered_neuf()$emission_ges_5_usages_par_m2)))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm = TRUE),2), " kg CO₂/m².an")
  })
  
  output$conso_moyen = renderText({
    x = suppressWarnings(as.numeric(c(filtered_existant()$conso_5_usages_par_m2_ep,
                                      filtered_neuf()$conso_5_usages_par_m2_ep)))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm = TRUE),2), " kWh/m².an")
  })
  
  ############## KPI des logements existants #####################################
  
  output$conso_existant = renderText({
    x = suppressWarnings(as.numeric(filtered_existant()$conso_5_usages_par_m2_ep))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm=TRUE),2), " kWh/m².an")
  })
  
  output$ges_existant = renderText({
    x = suppressWarnings(as.numeric(filtered_existant()$emission_ges_5_usages_par_m2))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm=TRUE),2), " kg CO₂/m²/an")
  })
  
  output$dpe_existant = renderText({
    df = filtered_existant()
    if (nrow(df) == 0) return("NA")
    df$score = convert_dpe(df$etiquette_dpe)
    round(mean(df$score, na.rm=TRUE),2)
  })
  
  ################################ KPI des logements neufs #############################
  
  output$conso_neuf = renderText({
    x = suppressWarnings(as.numeric(filtered_neuf()$conso_5_usages_par_m2_ep))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm=TRUE),2), " kWh/m².an")
  })
  
  output$ges_neuf = renderText({
    x = suppressWarnings(as.numeric(filtered_neuf()$emission_ges_5_usages_par_m2))
    if (length(x) == 0 || all(is.na(x))) return("NA")
    paste0(round(mean(x, na.rm=TRUE),2), " kg CO₂/m².an")
  })
  
  output$dpe_neuf = renderText({
    df = filtered_neuf()
    if (nrow(df) == 0) return("NA")
    df$score = convert_dpe(df$etiquette_dpe)
    round(mean(df$score, na.rm=TRUE),2)
  })
  
  ######################################### Partie graphiques €"#######################
  
  output$plot_dpe_annees = renderPlot({
    df_all = rbind(rv_existant(), rv_neuf())
    if (nrow(df_all) == 0) {
      return(ggplot() + ggtitle("Pas de données"))
    }
    
    df_all = df_all %>%
      mutate(annee = as.numeric(substr(date_reception_dpe, 1, 4))) %>%
      filter(!is.na(annee) & annee >= 2021)
    
    if (!is.null(input$annee_filtre) && input$annee_filtre != "Tout") {
      limite = as.numeric(input$annee_filtre)
      df_all = df_all %>% filter(annee <= limite)
    }
    
    if (nrow(df_all) == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection d'année"))
    }
    
    df_group = df_all %>%
      group_by(annee) %>%
      summarise(n=n()) %>%
      arrange(annee) %>%
      mutate(cumul = cumsum(n))
    
    ggplot(df_group, aes(x = annee, y = cumul)) +
      geom_area(fill = palette_ch[4], alpha=.45) +
      geom_line(size = 1.2, color = palette_ch[4]) +
      getPlotTheme(input$toggle_theme) +
      labs(x = "Années", y = "Nombre DPE") +
      scale_x_continuous(breaks=df_group$annee) +
      theme(plot.title=element_text(face="bold", size=18))
  })
  
  output$plot_pie_ex_neuf = renderPlot({
    
    n_ex = nrow(filtered_existant())
    n_nf = nrow(filtered_neuf())
    if (n_ex + n_nf == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection d'année"))
    }
    
    df = data.frame(
      type = c("Existant", "Neuf"),
      n    = c(n_ex, n_nf)
    ) %>%
      mutate(pct = n / sum(n),
             label = paste0(type, "\n", n))
    
    ggplot(df, aes(x = "", y = pct, fill = type)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_text(aes(label=label),
                position=position_stack(vjust=0.5),
                color="black", size=5) +
      scale_fill_manual(values=c(palette_ch[3], palette_ch[4])) +
      getPlotTheme(input$toggle_theme) +
      theme_void() + theme(plot.title=element_text(face="bold", size=18, hjust=.5))
  })
  
  ############################# Attention histogrammes et boutons png ##############
  
  output$plot_histogram_dpe = renderPlot({
    
    df_ex = filtered_existant()
    df_nf = filtered_neuf()
    
    df_ex = df_ex %>% filter(etiquette_dpe %in% LETTERS[1:7])
    df_nf = df_nf %>% filter(etiquette_dpe %in% LETTERS[1:7])
    
    if (nrow(df_ex) + nrow(df_nf) == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection"))
    }
    
    total = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_ex$etiquette_dpe == e, na.rm=TRUE) +
          sum(df_nf$etiquette_dpe == e, na.rm=TRUE)),
      type = "Total"
    )
    
    existant = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_ex$etiquette_dpe == e, na.rm=TRUE)),
      type = "Existant"
    )
    
    neuf = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_nf$etiquette_dpe == e, na.rm=TRUE)),
      type = "Neuf"
    )
    
    df_plot = rbind(total, existant, neuf)
    
    df_plot$type = factor(df_plot$type, levels = c("Neuf", "Existant", "Total"))
    
    ggplot(df_plot, aes(x = etiquette, y = n, fill = type)) +
      geom_bar(stat="identity",
               position=position_dodge(width = 0.6),
               width=0.5) +
      labs(x = "Étiquette DPE",
           y = "Nombre de DPE") +
      scale_fill_manual(values = c(
        "Total" = palette_ch[3],
        "Existant" = palette_ch[4],
        "Neuf" = palette_ch[2]
      )) +
      scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
      geom_vline(xintercept = seq(1.5, 6.5, by = 1),
                 linetype="dashed", color="grey70", alpha=0.6) +
      getPlotTheme(input$toggle_theme) +
      theme(
        plot.title = element_text(face="bold", size=18, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  })
  
  output$plot_histogram_ges = renderPlot({
    
    df_ex = filtered_existant()
    df_nf = filtered_neuf()
    
    df_ex = df_ex %>% filter(etiquette_ges %in% LETTERS[1:7])
    df_nf = df_nf %>% filter(etiquette_ges %in% LETTERS[1:7])
    
    if (nrow(df_ex) + nrow(df_nf) == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection"))
    }
    
    total = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_ex$etiquette_ges == e, na.rm=TRUE) +
          sum(df_nf$etiquette_ges == e, na.rm=TRUE)),
      type = "Total"
    )
    
    existant = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_ex$etiquette_ges == e, na.rm=TRUE)),
      type = "Existant"
    )
    
    neuf = data.frame(
      etiquette = LETTERS[1:7],
      n = sapply(LETTERS[1:7], function(e)
        sum(df_nf$etiquette_ges == e, na.rm=TRUE)),
      type = "Neuf"
    )
    
    df_plot = rbind(total, existant, neuf)
    
    df_plot$type = factor(df_plot$type, levels = c("Neuf", "Existant", "Total"))
    
    ggplot(df_plot, aes(x = etiquette, y = n, fill = type)) +
      geom_bar(stat="identity",
               position=position_dodge(width = 0.6),
               width=0.5) +
      labs(x = "Étiquette GES",
           y = "Nombre de diagnostics") +
      scale_fill_manual(values = c(
        "Total" = palette_ch[3],
        "Existant" = palette_ch[4],
        "Neuf" = palette_ch[2]
      )) +
      scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
      geom_vline(xintercept = seq(1.5, 6.5, by = 1),
                 linetype="dashed", color="grey70", alpha=0.6) +
      getPlotTheme(input$toggle_theme) +
      theme(
        plot.title = element_text(face="bold", size=18, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  })
  
  ######### Boites à moustache
  
  output$plot_box_conso_existant = renderPlot({
    
    df = filtered_existant()
    conso = suppressWarnings(as.numeric(df$conso_5_usages_par_m2_ep))
    df_plot = data.frame(conso = conso) %>% filter(!is.na(conso))
    
    if (nrow(df_plot) == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection d'année"))
    }
    
    ggplot(df_plot, aes(y = conso)) +
      geom_boxplot(
        fill = palette_ch[4],
        alpha = 0.5,
        color = "black",
        outlier.color = "red",
        outlier.size = 2
      ) +
      labs(y = "kWh/m²/an",
           x = ""
      ) +
      getPlotTheme(input$toggle_theme) +
      theme(
        plot.title = element_text(face="bold", size=18, hjust=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  })
  
  output$plot_box_conso_neuf = renderPlot({
    
    df = filtered_neuf()
    conso = suppressWarnings(as.numeric(df$conso_5_usages_par_m2_ep))
    df_plot = data.frame(conso = conso) %>% filter(!is.na(conso))
    
    if (nrow(df_plot) == 0) {
      return(ggplot() + ggtitle("Pas de données pour cette sélection d'année"))
    }
    
    ggplot(df_plot, aes(y = conso)) +
      geom_boxplot(
        fill = palette_ch[2],
        alpha = 0.5,
        color = "black",
        outlier.color = "red",
        outlier.size = 2
      ) +
      labs(y = "kWh/m²/an",
           x = ""
      ) +
      getPlotTheme(input$toggle_theme) +
      theme(
        plot.title = element_text(face="bold", size=18, hjust=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  })
  
  ######################################################################################
  ############################################ ZONE CARTE ##############################
  ######################################################################################
  dpe_with_coords = reactive({
    df_all = dpe_all()
    if (nrow(df_all) == 0) return(df_all[0, ])
    
    if (!"identifiant_ban" %in% names(df_all)) {
      return(df_all[0, ])
    }
    
    df_all$identifiant_ban = as.character(df_all$identifiant_ban)
    
    df_join = df_all %>%
      left_join(
        adresses_38 %>% 
          mutate(id = as.character(id)) %>% 
          select(id, lon, lat),
        by = c("identifiant_ban" = "id")
      ) %>%
      filter(!is.na(lon), !is.na(lat))
    
    df_join
  })
  
  output$map_dpe = renderLeaflet({
    
    df = dpe_with_coords()
    
    if (nrow(df) == 0) {
      return(leaflet() %>% addTiles())
    }
    # Zone des épinglettes
    icon_files = c(
      "A" = "pin_A.svg",
      "B" = "pin_B.svg",
      "C" = "pin_C.svg",
      "D" = "pin_D.svg",
      "E" = "pin_E.svg",
      "F" = "pin_F.svg",
      "G" = "pin_G.svg"
    )
    
    df$etiquette_dpe_clean = ifelse(df$etiquette_dpe %in% names(icon_files),
                                    df$etiquette_dpe,
                                    "G")
    
    icons_dpe = icons(
      iconUrl = icon_files[df$etiquette_dpe_clean],
      iconWidth  = 40,
      iconHeight = 40,
      iconAnchorX = 20,
      iconAnchorY = 40
    )
    
    #Zone fenêtre info
    df$popup = paste0(
      "<b>", df$adresse_ban, "</b><br/>",
      "Type de bâtiment : <b>", df$type_batiment, "</b><br/>",
      "DPE : <b>", df$etiquette_dpe, "</b><br/>",
      "GES : <b>", df$etiquette_ges, "</b><br/>",
      "Conso : ", df$conso_5_usages_par_m2_ep, " kWh/m².an<br/>",
      "Émissions GES : ", df$emission_ges_5_usages_par_m2, " kg CO₂/m².an"
    )
    
    leaflet(df) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        icon = icons_dpe,
        popup = ~popup,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c(
          "#009E3B", "#55B948", "#A3D977",
          "#FFF200", "#F9B233", "#EB7223", "#E30613"
        ),
        labels = c("A", "B", "C", "D", "E", "F", "G"),
        title = "Étiquette DPE"
      )
  })
  
  ################################# CORRELATIONS ###############################################"
  
  observe({
    if (!is.null(input$var_x) && !is.null(input$var_y) && input$var_x == input$var_y) {
      showNotification("X et Y doivent être différentes.", type="error", duration=3)
    }
  })
  
  output$plot_corr = renderPlot({
    
    df = dpe_with_coords()
    
    if (nrow(df) == 0) {
      return(ggplot() + ggtitle("Pas de données"))
    }
    
    if (is.null(input$var_x) || is.null(input$var_y) || input$var_x == input$var_y) {
      return(ggplot() + ggtitle("Choisissez deux variables différentes pour X et Y"))
    }
    
    df$X = suppressWarnings(as.numeric(df[[input$var_x]]))
    df$Y = suppressWarnings(as.numeric(df[[input$var_y]]))
    
    df_clean = df %>% filter(!is.na(X), !is.na(Y))
    
    if (nrow(df_clean) < 10) {
      ggplot() + ggtitle("Pas assez de données pour afficher la régression")
    } else {
      ggplot(df_clean, aes(x = X, y = Y)) +
        geom_point(alpha = 0.4, size = 2, color = "#0073C2FF") +
        geom_smooth(method = "lm", color = "darkred", se = TRUE, linewidth = 1.1) +
        labs(
          x = input$var_x,
          y = input$var_y,
          title = "Corrélation et régression linéaire"
        ) +
        theme_minimal(base_size = 15)
    }
  })
  
  output$resultats_reg = renderUI({
    
    df = dpe_with_coords()
    if (nrow(df) == 0) {
      return(HTML("<b>Pas assez de données pour calculer la régression.</b>"))
    }
    
    if (is.null(input$var_x) || is.null(input$var_y) || input$var_x == input$var_y) {
      return(HTML("<b>Choisissez deux variables différentes pour X et Y.</b>"))
    }
    
    df$X = suppressWarnings(as.numeric(df[[input$var_x]]))
    df$Y = suppressWarnings(as.numeric(df[[input$var_y]]))
    df_clean = df %>% filter(!is.na(X), !is.na(Y))
    
    if (nrow(df_clean) < 10) {
      return(HTML("<b>Pas assez de données pour calculer la régression.</b>"))
    }
    
    modele = lm(Y ~ X, data = df_clean)
    coef = coef(modele)
    a = round(coef[2], 4)
    b = round(coef[1], 4)
    
    r = suppressWarnings(cor(df_clean$X, df_clean$Y))
    r2 = round(summary(modele)$r.squared, 4)
    r_round = round(r, 4)
    
    ##################### Indice de correlation basé sur le R ###########
    interpretation =
      if (abs(r) >= 0.80) {
        "Corrélation <b>très forte</b>"
      } else if (abs(r) >= 0.60) {
        "Corrélation <b>forte</b>"
      } else if (abs(r) >= 0.30) {
        "Corrélation <b>modérée</b>"
      } else if (abs(r) >= 0.10) {
        "Corrélation <b>faible</b>"
      } else {
        "Corrélation <b>très faible ou nulle</b>"
      }
    
    HTML(paste0(
      "<h4><b>Résultats de la régression linéaire</b></h4>",
      "<ul>",
      "<li><b>Équation :</b> Y = ", a, " × X + ", b, "</li>",
      "<li><b>Coefficient de corrélation (r) :</b> ", r_round, "</li>",
      "<li><b>R² :</b> ", r2, "</li>",
      "<li><b>Coefficient de Pearson :</b> ", r_round, "</li>",
      "<li><b>Interprétation :</b> ", interpretation, "</li>",
      "</ul>"
    ))
  })
  
  ##################################### traiter probleme export png ####################
  ############################################# ZONE PNG ###############################
  
  output$download_hist_dpe = downloadHandler(
    filename = function() {
      paste0("Histogramme_DPE_", input$annee_filtre, ".png")
    },
    content = function(file) {
      
      df_ex = filtered_existant()
      df_nf = filtered_neuf()
      
      df_ex = df_ex %>% filter(etiquette_dpe %in% LETTERS[1:7])
      df_nf = df_nf %>% filter(etiquette_dpe %in% LETTERS[1:7])
      
      total = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_ex$etiquette_dpe == e, na.rm=TRUE) +
            sum(df_nf$etiquette_dpe == e, na.rm=TRUE)),
        type = "Total"
      )
      
      existant = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_ex$etiquette_dpe == e, na.rm=TRUE)),
        type = "Existant"
      )
      
      neuf = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_nf$etiquette_dpe == e, na.rm=TRUE)),
        type = "Neuf"
      )
      
      df_plot = rbind(total, existant, neuf)
      df_plot$type = factor(df_plot$type, levels = c("Neuf", "Existant", "Total"))
      
      p = ggplot(df_plot, aes(x = etiquette, y = n, fill = type)) +
        geom_bar(stat="identity",
                 position=position_dodge(width = 0.6),
                 width=0.5) +
        labs(title = "Nombre de DPE par étiquette",
             x = "Étiquette DPE",
             y = "Nombre de DPE") +
        scale_fill_manual(values = c(
          "Total" = palette_ch[3],
          "Existant" = palette_ch[4],
          "Neuf" = palette_ch[2]
        )) +
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        geom_vline(xintercept = seq(1.5, 6.5, by = 1),
                   linetype="dashed", color="grey70", alpha=0.6) +
        getPlotTheme(input$toggle_theme) +
        theme(
          plot.title = element_text(face="bold", size=18, hjust=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      ggsave(file, plot = p, device = "png", width = 10, height = 7)
    }
  )
  
  # Export Png histogramme GES
  output$download_hist_ges = downloadHandler(
    filename = function() {
      paste0("Histogramme_GES_", input$annee_filtre, ".png")
    },
    content = function(file) {
      
      df_ex = filtered_existant()
      df_nf = filtered_neuf()
      
      df_ex = df_ex %>% filter(etiquette_ges %in% LETTERS[1:7])
      df_nf = df_nf %>% filter(etiquette_ges %in% LETTERS[1:7])
      
      total = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_ex$etiquette_ges == e, na.rm=TRUE) +
            sum(df_nf$etiquette_ges == e, na.rm=TRUE)),
        type = "Total"
      )
      
      existant = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_ex$etiquette_ges == e, na.rm=TRUE)),
        type = "Existant"
      )
      
      neuf = data.frame(
        etiquette = LETTERS[1:7],
        n = sapply(LETTERS[1:7], function(e)
          sum(df_nf$etiquette_ges == e, na.rm=TRUE)),
        type = "Neuf"
      )
      
      df_plot = rbind(total, existant, neuf)
      df_plot$type = factor(df_plot$type, levels = c("Neuf", "Existant", "Total"))
      
      p = ggplot(df_plot, aes(x = etiquette, y = n, fill = type)) +
        geom_bar(stat="identity",
                 position=position_dodge(width = 0.6),
                 width=0.5) +
        labs(title = "Nombre de diagnostics GES par étiquette",
             x = "Étiquette GES",
             y = "Nombre de diagnostics") +
        scale_fill_manual(values = c(
          "Total" = palette_ch[3],
          "Existant" = palette_ch[4],
          "Neuf" = palette_ch[2]
        )) +
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        geom_vline(xintercept = seq(1.5, 6.5, by = 1),
                   linetype="dashed", color="grey70", alpha=0.6) +
        getPlotTheme(input$toggle_theme) +
        theme(
          plot.title = element_text(face="bold", size=18, hjust=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      ggsave(file, plot = p, device = "png", width = 10, height = 7)
    }
  )
}

shinyApp(ui = ui, server = server)