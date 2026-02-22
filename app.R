
# getwd()
# #setwd("/Users/drpaco/ShinyApps/AnalyseRSVL")
# setwd("/cloud/project/AnalyseRSVL")


##############################################
# Analyse RSVL - Application Shiny (FR)
# Données : RSVL + BDGA (Région / MRC / Municipalité)
##############################################

library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(ggplot2)
library(stringi)

options(encoding = "UTF-8")

##############################################
# 1. Fonction pour télécharger le GPKG RSVL
##############################################

download_rsvl_data <- function() {
  
  url <- "https://pab.donneesquebec.ca/recherche/dataset/da90ed32-e5f8-4b1f-b522-2eeadbfb5682/resource/8b11b0a5-8491-4a85-8683-02bc1c516807/download/rsvl_20231219.gpkg.zip"
  
  dest_zip <- "rsvl_latest.zip"
  
  download.file(url, destfile = dest_zip, mode = "wb")
  
  contenu_zip <- unzip(dest_zip, list = TRUE)$Name
  gpkg_file <- contenu_zip[grepl("\\.gpkg$", contenu_zip)]
  unzip(dest_zip, files = gpkg_file, exdir = ".")
  
  return(gpkg_file)
}

##############################################
# 2. Chargement sécurisé
##############################################

safe_load_rsvl <- function() {
  
  fichiers <- list.files(pattern = "\\.gpkg$")
  if (length(fichiers) == 0) return(NULL)
  
  data <- tryCatch(
    st_read(fichiers[1], layer = "rsvl_stations_p", quiet = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(data)) return(NULL)
  
  # Normalisation Unicode
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      data[[col]] <- enc2utf8(data[[col]])
      data[[col]] <- stringi::stri_trans_general(data[[col]], "NFC")
    }
  }
  
  return(data)
}

##############################################
# 3. Chargement initial : RSVL
##############################################

rsvl <- safe_load_rsvl()

if (is.null(rsvl)) {
  gpkg_file <- download_rsvl_data()
  rsvl <- st_read(gpkg_file, layer = "rsvl_stations_p", quiet = TRUE)
  
  for (col in names(rsvl)) {
    if (is.character(rsvl[[col]])) {
      rsvl[[col]] <- enc2utf8(rsvl[[col]])
      rsvl[[col]] <- stringi::stri_trans_general(rsvl[[col]], "NFC")
    }
  }
}

if (is.null(rsvl)) stop("Impossible de charger les données RSVL.")

##############################################
# 4. JOINT SPATIAL avec BDGA : Région → MRC → Municipalité
##############################################

# Charger municipalités avec surfaces (_s)
mun <- st_read("Bdga1m/SHP/munic_s.shp", quiet = TRUE)

# Harmoniser le CRS
mun <- st_transform(mun, st_crs(rsvl))

# Garder seulement les colonnes nécessaires
mun <- mun[, c("MUS_NM_REG", "MUS_NM_MRC", "MUS_NM_MUN")]

# Joint spatial
rsvl_adm <- st_join(rsvl, mun)

# Retirer geometrie
rsvl_df <- rsvl_adm %>% st_drop_geometry()

##############################################
# Préparation des listes de filtres
##############################################

# Correction accents UTF‑8
cols <- c("MUS_NM_REG", "MUS_NM_MRC", "MUS_NM_MUN", "nom_lac")
for (c in cols) rsvl_df[[c]] <- enc2utf8(rsvl_df[[c]])

# Listes pour les menus
choix_reg  <- sort(unique(rsvl_df$MUS_NM_REG))
choix_mrc  <- sort(unique(rsvl_df$MUS_NM_MRC))
choix_mun  <- sort(unique(rsvl_df$MUS_NM_MUN))
choix_lacs <- sort(unique(rsvl_df$nom_lac))

##############################################
# 4. Interface utilisateur
##############################################
# Correction accents pour Shiny
rsvl_df$nom_lac <- enc2utf8(rsvl_df$nom_lac)
rsvl_df$bv_n1   <- enc2utf8(rsvl_df$bv_n1)
rsvl_df$bv_n2   <- enc2utf8(rsvl_df$bv_n2)

choix_lacs <- sort(unique(rsvl_df$nom_lac))
choix_bv1  <- sort(unique(rsvl_df$bv_n1))
choix_bv2  <- sort(unique(rsvl_df$bv_n2))

ui <- dashboardPage(
  dashboardHeader(title = "Analyse RSVL"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Évolution annuelle", tabName = "timeseries", icon = icon("chart-line"))
    ),
    
    selectizeInput("region", "Région administrative :", choices = c("Toutes", choix_reg), selected = "Toutes"),
    selectizeInput("mrc", "MRC :", choices = c("Toutes", choix_mrc), selected = "Toutes"),
    selectizeInput("municip", "Municipalité :", choices = c("Toutes", choix_mun), selected = "Toutes"),
    selectizeInput("bv1", "Bassin versant niveau 1 :", choices = c("Tous", choix_bv1), selected = "Tous"),
    selectizeInput("bv2", "Bassin versant niveau 2 :", choices = c("Tous", choix_bv2), selected = "Tous"),
    
    selectizeInput("lac", "Lac :", choices = choix_lacs, selected = NULL,
                   options = list(placeholder = "Tapez pour chercher…", maxOptions = 5000)),
    
    selectInput(
      "variable", "Variable :",
      choices = c(
        "Phosphore total"           = "p_tot_moy",
        "Chlorophylle a"            = "chlo_moy",
        "Carbone organique dissous" = "cod_moy",
        "Transparence (Secchi)"     = "transp_moy"
      )
    ),
    
    checkboxInput("show_lm", "Afficher la tendance", value = FALSE),
    actionButton("update_data", "Mettre à jour la base de données", icon = icon("download"), class = "btn-primary")
  ),
  
  dashboardBody(
    tabItem(
      tabName = "timeseries",
      fluidRow(
        box(
          width = 12,
          title = "Évolution annuelle",
          status = "primary",
          solidHeader = TRUE,
          
          selectInput(
            "scale_level",
            "Échelle du sommaire des données :",
            choices = c(
              "Année" = "annee_prel",
              "Région" = "MUS_NM_REG",
              "MRC" = "MUS_NM_MRC",
              "Municipalité" = "MUS_NM_MUN",
              "BV1" = "bv_n1",
              "BV2" = "bv_n2",
              "Lac" = "nom_lac"
            ),
            selected = "annee_prel"
          ),
          
          plotOutput("timeplot", height = "450px",click = "plot_click"),
          verbatimTextOutput("clicked_lake"),
          
          checkboxInput("show_raw", "Afficher les données brutes", FALSE),
          checkboxInput("show_summary", "Afficher le tableau résumé", FALSE),
          
          conditionalPanel(
            condition = "input.show_summary == true",
            DT::dataTableOutput("summary_table")
          ),
          
          downloadButton("download_csv", "Télécharger les données"),
          
          
          conditionalPanel(
            condition = "input.show_raw == true",
            DT::dataTableOutput("raw_table")
          )
        )
      )
    )
  )
)


##############################################
# 5. Serveur
##############################################

server <- function(input, output, session) {
  
  output$clicked_lake <- renderPrint({
    req(input$plot_click)
    
    df <- donnees_filtrees()
    
    # On cherche le point le plus proche du clic
    near <- nearPoints(
      df,
      input$plot_click,
      xvar = "annee_prel",
      yvar = input$variable,
      maxpoints = 1
    )
    
    if (nrow(near) == 0) {
      return("Aucun point sélectionné")
    }
    
    paste("Lac sélectionné :", near$nom_lac)
  })
  
  # ---------- 1. Données filtrées ----------
  donnees_filtrees <- reactive({
    df <- rsvl_df
    
    if (input$region != "Toutes")
      df <- df %>% filter(!is.na(MUS_NM_REG) & MUS_NM_REG == input$region)
    
    if (input$scale_level == "MUS_NM_REG") return(df)
    
    if (input$mrc != "Toutes")
      df <- df %>% filter(!is.na(MUS_NM_MRC) & MUS_NM_MRC == input$mrc)
    
    if (input$scale_level == "MUS_NM_MRC") return(df)
    
    if (input$municip != "Toutes")
      df <- df %>% filter(!is.na(MUS_NM_MUN) & MUS_NM_MUN == input$municip)
    
    if (input$scale_level == "MUS_NM_MUN") return(df)
    
    if (input$bv1 != "Tous")
      df <- df %>% filter(!is.na(bv_n1) & bv_n1 == input$bv1)
    
    if (input$scale_level == "bv_n1") return(df)
    
    if (input$bv2 != "Tous")
      df <- df %>% filter(!is.na(bv_n2) & bv_n2 == input$bv2)
    
    if (input$scale_level == "bv_n2") return(df)
    
    if (!is.null(input$lac) && input$lac != "")
      df <- df %>% filter(!is.na(nom_lac) & nom_lac == input$lac)
    
    df
  })
  
  observeEvent(input$scale_level, {
    if (input$scale_level != "annee_prel") {
      updateCheckboxInput(session, "show_lm", value = FALSE)
    }
  })
  
  
  # ---------- 2. Menus dépendants ----------
  observeEvent(
    list(input$region, input$mrc, input$municip, input$bv1, input$bv2),
    {
      df <- rsvl_df
      
      # Appliquer les filtres hiérarchiques
      if (input$region != "Toutes")
        df <- df %>% filter(MUS_NM_REG == input$region)
      if (input$mrc != "Toutes")
        df <- df %>% filter(MUS_NM_MRC == input$mrc)
      if (input$municip != "Toutes")
        df <- df %>% filter(MUS_NM_MUN == input$municip)
      if (input$bv1 != "Tous")
        df <- df %>% filter(bv_n1 == input$bv1)
      if (input$bv2 != "Tous")
        df <- df %>% filter(bv_n2 == input$bv2)
      
      # Mettre à jour les choix disponibles à chaque niveau
      updateSelectizeInput(
        session, "mrc",
        choices  = c("Toutes", sort(unique(df$MUS_NM_MRC))),
        selected = if (input$region == "Toutes") "Toutes" else input$mrc
      )
      
      updateSelectizeInput(
        session, "municip",
        choices  = c("Toutes", sort(unique(df$MUS_NM_MUN))),
        selected = if (input$mrc == "Toutes") "Toutes" else input$municip
      )
      
      updateSelectizeInput(
        session, "bv1",
        choices  = c("Tous", sort(unique(df$bv_n1))),
        selected = if (input$municip == "Toutes") "Tous" else input$bv1
      )
      
      updateSelectizeInput(
        session, "bv2",
        choices  = c("Tous", sort(unique(df$bv_n2))),
        selected = if (input$bv1 == "Tous") "Tous" else input$bv2
      )
      
      updateSelectizeInput(
        session, "lac",
        choices  = sort(unique(df$nom_lac)),
        selected = NULL
      )
    }
  )
  
  observeEvent(input$scale_level, {
    if (input$scale_level != "annee_prel") {
      updateCheckboxInput(session, "show_lm", value = FALSE)
    }
  })
  
  # ---------- 3. Tableau résumé ----------
  safe_min <- function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  safe_max <- function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  
  output$summary_table <- DT::renderDataTable({
    df <- donnees_filtrees()
    req(nrow(df) > 0)
    
    df %>%
      group_by(annee_prel) %>%
      summarise(
        Moyenne = mean(.data[[input$variable]], na.rm = TRUE),
        Médiane = median(.data[[input$variable]], na.rm = TRUE),
        Min     = safe_min(.data[[input$variable]]),
        Max     = safe_max(.data[[input$variable]]),
        N       = sum(!is.na(.data[[input$variable]])),
        SD      = sd(.data[[input$variable]], na.rm = TRUE),
        SE      = SD / sqrt(N),
        IC_bas  = Moyenne - 1.96 * SE,
        IC_haut = Moyenne + 1.96 * SE
      )
  })
  
  
  # ---------- 4. Tableau brut ----------
  output$raw_table <- DT::renderDataTable({
    req(input$show_raw)
    donnees_filtrees() %>%
      select(
        annee_prel, nom_lac, MUS_NM_REG, MUS_NM_MRC, MUS_NM_MUN,
        bv_n1, bv_n2, all_of(input$variable)
      )
  })
  
  # ---------- 5. Téléchargement CSV ----------
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("rsvl_resume_", input$scale_level, "_", input$variable, ".csv")
    },
    content = function(file) {
      df <- donnees_filtrees()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---------- 6. Graphique ----------
  output$timeplot <- renderPlot({
    df <- donnees_filtrees()
    req(nrow(df) > 0)
    
    # Label humain de la variable
    var_label <- names(which(c(
      "Phosphore total"           = "p_tot_moy",
      "Chlorophylle a"            = "chlo_moy",
      "Carbone organique dissous" = "cod_moy",
      "Transparence (Secchi)"     = "transp_moy"
    ) == input$variable))
    
    # Résumé annuel
    df_summary <- df %>%
      group_by(annee_prel) %>%
      summarise(
        mean_value = mean(.data[[input$variable]], na.rm = TRUE),
        sd_value   = sd(.data[[input$variable]], na.rm = TRUE),
        n          = sum(!is.na(.data[[input$variable]])),
        se         = sd_value / sqrt(n),
        ci_low     = mean_value - 1.96 * se,
        ci_high    = mean_value + 1.96 * se
      )
    
    # Moyenne globale
    global_mean <- mean(df[[input$variable]], na.rm = TRUE)
    global_sd   <- sd(df[[input$variable]], na.rm = TRUE)
    global_n    <- sum(!is.na(df[[input$variable]]))
    global_se   <- global_sd / sqrt(global_n)
    global_low  <- global_mean - 1.96 * global_se
    global_high <- global_mean + 1.96 * global_se
    
    n_lacs <- n_distinct(df$nom_lac)
    n_data <- global_n
    
    p <- ggplot()
    
    # Moyennes annuelles
    if (!input$show_lm) {
      p <- p +
        geom_ribbon(
          data = df_summary,
          aes(x = annee_prel, ymin = ci_low, ymax = ci_high),
          fill = "#95A5A6", alpha = 0.3
        ) +
        geom_line(
          data = df_summary,
          aes(x = annee_prel, y = mean_value),
          color = "#2C3E50", size = 1
        ) +
        geom_point(
          data = df_summary,
          aes(x = annee_prel, y = mean_value),
          color = "#E74C3C", size = 2
        )
      
      trend_msg <- ""
    }
    
    # Régression
    if (input$show_lm) {
      lm_model <- lm(df[[input$variable]] ~ df$annee_prel)
      slope   <- coef(lm_model)[2]
      p_value <- summary(lm_model)$coefficients[2, 4]
      
      unit <- dplyr::case_when(
        input$variable == "p_tot_moy"  ~ "µg/L",
        input$variable == "chlo_moy"   ~ "µg/L",
        input$variable == "cod_moy"    ~ "mg/L",
        input$variable == "transp_moy" ~ "m",
        TRUE ~ ""
      )
      
      trend_msg <- if (p_value < 0.05) {
        sprintf(
          "Les données montrent une tendance significative avec un changement de %.3f %s par année.",
          slope, unit
        )
      } else {
        "Les données ne démontrent aucune tendance significative."
      }
      
      p <- p +
        geom_point(
          data = df,
          aes(x = annee_prel, y = .data[[input$variable]]),
          color = "#34495E", size = 2, alpha = 0.7
        ) +
        geom_smooth(
          data = df,
          aes(x = annee_prel, y = .data[[input$variable]]),
          method = "lm",
          se = TRUE,
          color = "#1F618D",
          fill = "#1F618D",
          alpha = 0.2
        )
    }
    
    # Moyenne globale
    p <- p +
      geom_hline(
        yintercept = global_mean,
        linetype = "dotted",
        color = "#7D3C98",
        size = 1
      ) +
      geom_rect(
        aes(
          xmin = -Inf, xmax = Inf,
          ymin = global_low, ymax = global_high
        ),
        fill = "#7D3C98", alpha = 0.08
      ) +
      annotate(
        "text",
        x = max(df_summary$annee_prel),
        y = global_mean,
        label = "Moyenne globale",
        hjust = -0.1,
        vjust = -0.5,
        color = "#7D3C98",
        size = 5
      )
    
    # Label d’échelle
    scale_label <- switch(
      input$scale_level,
      "annee_prel" = "Année",
      "MUS_NM_REG" = paste("Région :", input$region),
      "MUS_NM_MRC" = paste("MRC :", input$mrc),
      "MUS_NM_MUN" = paste("Municipalité :", input$municip),
      "bv_n1"      = paste("BV1 :", input$bv1),
      "bv_n2"      = paste("BV2 :", input$bv2),
      "nom_lac"    = paste("Lac :", input$lac),
      "Groupe"
    )
    
    p +
      theme_minimal(base_size = 16) +
      labs(
        x = "Année",
        y = var_label,
        title = paste("Évolution -", var_label),
        subtitle = paste0(
          scale_label,
          " (", n_lacs, " lacs, ", n_data, " données)",
          if (input$show_lm) paste(" |", trend_msg) else ""
        )
      )
  })
}


##############################################
# 6. Lancer l’application
##############################################

local({
  Sys.setenv(
    LANG = "fr_FR.UTF-8",
    LC_ALL = "fr_FR.UTF-8",
    LC_CTYPE = "fr_FR.UTF-8",
    LC_NUMERIC = "fr_FR.UTF-8",
    LC_TIME = "fr_FR.UTF-8",
    LC_COLLATE = "fr_FR.UTF-8",
    LC_MONETARY = "fr_FR.UTF-8",
    LC_MESSAGES = "fr_FR.UTF-8"
  )
  
  shinyApp(ui, server)
})
