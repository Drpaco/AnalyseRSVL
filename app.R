
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
library(plotly)

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
    # Exemple : actionButton pour mise à jour de la base (UI)
    actionButton("update_data",
                 label = HTML("Mettre à jour<br>la base de données"),
                 class = "btn-primary")
    
  ),
  
  dashboardBody(
    tabItems(
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
          
          # inside the box that contains the plotlyOutput
          plotlyOutput("timeplot", height = "450px"),
          
          # Toggle to show raw datapoints inside the plot (new)
          checkboxInput("show_raw_plot", "Afficher les données brutes dans le graphique", value = FALSE),
          
          # Checkbox to show the raw data table (label updated)
          checkboxInput("show_raw", "Afficher le tableau des données brutes ci-dessous", FALSE),
          
          # Checkbox to show the summary table (label updated)
          checkboxInput("show_summary", "Afficher le tableau résumé ci-dessous", FALSE),
          
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
)


##############################################
# 5. Serveur
##############################################

server <- function(input, output, session) {
  
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
  output$timeplot <- plotly::renderPlotly({
    # packages requis
    require(dplyr)
    require(plotly)
    require(tibble)
    
    # données filtrées (doit exister dans votre app)
    df <- donnees_filtrees()
    req(is.data.frame(df) && nrow(df) > 0)
    
    # préparer variables de base
    df <- df %>% mutate(annee_prel = as.numeric(annee_prel)) %>% arrange(annee_prel)
    df$yvar <- df[[input$variable]]
    
    # résumé par année (protéger les calculs)
    df_summary <- df %>%
      group_by(annee_prel) %>%
      summarise(
        mean_value = mean(yvar, na.rm = TRUE),
        sd_value   = sd(yvar, na.rm = TRUE),
        n          = sum(!is.na(yvar)),
        se         = ifelse(n > 0, sd_value / sqrt(n), NA_real_),
        ci_low     = mean_value - 1.96 * se,
        ci_high    = mean_value + 1.96 * se,
        .groups = "drop"
      ) %>%
      arrange(annee_prel)
    
    if (nrow(df_summary) == 0) {
      return(plot_ly() %>% layout(title = "Aucune donnée annuelle disponible"))
    }
    
    # interpolation sur la séquence d'années pour bande continue
    years_full <- seq(min(df_summary$annee_prel, na.rm = TRUE),
                      max(df_summary$annee_prel, na.rm = TRUE), by = 1)
    df_summary_full <- tibble(annee_prel = years_full) %>%
      left_join(df_summary, by = "annee_prel") %>%
      arrange(annee_prel)
    
    if (sum(!is.na(df_summary$mean_value)) >= 2) {
      df_summary_full$mean_value <- approx(
        x = df_summary$annee_prel[!is.na(df_summary$mean_value)],
        y = df_summary$mean_value[!is.na(df_summary$mean_value)],
        xout = df_summary_full$annee_prel, rule = 2
      )$y
      df_summary_full$ci_low <- approx(
        x = df_summary$annee_prel[!is.na(df_summary$ci_low)],
        y = df_summary$ci_low[!is.na(df_summary$ci_low)],
        xout = df_summary_full$annee_prel, rule = 2
      )$y
      df_summary_full$ci_high <- approx(
        x = df_summary$annee_prel[!is.na(df_summary$ci_high)],
        y = df_summary$ci_high[!is.na(df_summary$ci_high)],
        xout = df_summary_full$annee_prel, rule = 2
      )$y
    } else {
      df_summary_full <- df_summary
    }
    df_summary <- df_summary_full
    
    # statistiques globales
    global_mean <- mean(df$yvar, na.rm = TRUE)
    global_sd   <- sd(df$yvar, na.rm = TRUE)
    global_n    <- sum(!is.na(df$yvar))
    global_se   <- ifelse(global_n > 0, global_sd / sqrt(global_n), NA_real_)
    global_low  <- global_mean - 1.96 * global_se
    global_high <- global_mean + 1.96 * global_se
    
    # protéger global_low/high si non finis
    if (!is.finite(global_low) || !is.finite(global_high)) {
      global_low <- min(df$yvar, na.rm = TRUE)
      global_high <- max(df$yvar, na.rm = TRUE)
    }
    
    # bornes x pour la shape
    x_min <- min(df$annee_prel, na.rm = TRUE)
    x_max <- max(df$annee_prel, na.rm = TRUE)
    pad <- ifelse(is.finite(x_min) & is.finite(x_max), max(1, (x_max - x_min) * 0.05), 1)
    rect_x0 <- x_min - pad
    rect_x1 <- x_max + pad
    
    # compteurs pour le sous-titre
    n_lacs <- n_distinct(df$nom_lac)
    n_data <- global_n
    
    # libellé variable lisible
    var_label <- names(which(c(
      "Phosphore total"           = "p_tot_moy",
      "Chlorophylle a"            = "chlo_moy",
      "Carbone organique dissous" = "cod_moy",
      "Transparence (Secchi)"     = "transp_moy"
    ) == input$variable))
    if (length(var_label) == 0) var_label <- input$variable
    
    # régression et message de tendance (calculer AVANT subtitle_text)
    trend_msg <- ""
    fit_df <- NULL
    if (isTRUE(input$show_lm)) {
      lm_try <- try(lm(yvar ~ annee_prel, data = df), silent = TRUE)
      if (!inherits(lm_try, "try-error") && length(coef(lm_try)) >= 2) {
        slope <- coef(lm_try)[2]
        p_value <- summary(lm_try)$coefficients[2, 4]
        unit <- dplyr::case_when(
          input$variable == "p_tot_moy"  ~ "µg/L",
          input$variable == "chlo_moy"   ~ "µg/L",
          input$variable == "cod_moy"    ~ "mg/L",
          input$variable == "transp_moy" ~ "m",
          TRUE ~ ""
        )
        if (p_value < 0.05) {
          trend_msg <- sprintf("Tendance significative : changement de %.3f %s par année.", slope, unit)
        } else {
          trend_msg <- "Aucune tendance significative détectée"
        }
        newx <- seq(x_min, x_max, length.out = 200)
        pred <- predict(lm_try, newdata = data.frame(annee_prel = newx), se.fit = TRUE)
        fit_df <- data.frame(x = newx, y = pred$fit, y_lo = pred$fit - 1.96 * pred$se.fit, y_hi = pred$fit + 1.96 * pred$se.fit)
      } else {
        fit_df <- NULL
      }
    }
    
    # construire le sous-titre : libellé + compteurs ; trend_msg sur 2e ligne si présent
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
    if (is.null(scale_label) || !nzchar(scale_label)) scale_label <- "Niveau"
    
    subtitle_text <- paste0(scale_label, " (", n_lacs, " lacs, ", n_data, " données)")
    if (isTRUE(input$show_lm) && nzchar(trend_msg)) {
      subtitle_text <- paste0(subtitle_text, "<br><span style='font-size:12px;color:#444;'>", trend_msg, "</span>")
    }
    
    # démarrer plotly (initialisation obligatoire)
    plt <- plot_ly()
    
    # ajouter traces : régression ou points bruts + bande CI annuelle + moyenne annuelle
    if (isTRUE(input$show_lm) && !is.null(fit_df)) {
      plt <- plt %>%
        add_markers(
          data = df,
          x = ~annee_prel, y = ~yvar,
          marker = list(color = "#34495E", size = 6, opacity = 0.7),
          text = ~paste0("Lac: ", nom_lac, "<br>Année: ", annee_prel, "<br>Valeur: ", round(yvar, 3)),
          hoverinfo = "text",
          showlegend = FALSE
        ) %>%
        add_lines(
          data = fit_df,
          x = ~x, y = ~y,
          line = list(color = "#1F618D", width = 2),
          hoverinfo = "none",
          showlegend = FALSE
        ) %>%
        add_ribbons(
          data = fit_df,
          x = ~x, ymin = ~y_lo, ymax = ~y_hi,
          fillcolor = "#1F618D", opacity = 0.15, line = list(width = 0),
          hoverinfo = "none",
          showlegend = FALSE
        )
    } else {
      if (isTRUE(input$show_raw_plot)) {
        plt <- plt %>%
          add_markers(
            data = df,
            x = ~annee_prel, y = ~yvar,
            marker = list(color = "#1F618D", size = 6, opacity = 0.7),
            text = ~paste0("Lac: ", nom_lac, "<br>Année: ", annee_prel, "<br>Valeur: ", round(yvar, 3)),
            hoverinfo = "text",
            showlegend = FALSE
          )
      }
      
      plt <- plt %>%
        add_ribbons(
          data = df_summary,
          x = ~annee_prel, ymin = ~ci_low, ymax = ~ci_high,
          fillcolor = "#95A5A6", opacity = 0.3, line = list(width = 0),
          name = "Intervalles de confiance 95%",
          hoverinfo = "none",
          showlegend = TRUE
        ) %>%
        add_lines(
          data = df_summary,
          x = ~annee_prel, y = ~mean_value,
          line = list(color = "#2C3E50", width = 2),
          name = "Moyenne (ligne)",
          hoverinfo = "none",
          showlegend = FALSE
        ) %>%
        add_markers(
          data = df_summary,
          x = ~annee_prel, y = ~mean_value,
          marker = list(color = "#E74C3C", size = 7),
          text = ~paste0("Année: ", annee_prel, "<br>Moyenne: ", round(mean_value, 3)),
          hoverinfo = "text",
          name = "Moyenne",
          showlegend = TRUE
        )
    }
    
    # ligne Moyenne globale (trace, pas dans la légende)
    mean_label_text <- paste0("Moyenne<br>globale: ", round(global_mean, 2))
    plt <- plt %>%
      add_lines(
        x = c(rect_x0, rect_x1),
        y = c(global_mean, global_mean),
        line = list(dash = "dot", color = "#7D3C98", width = 2),
        name = "Moyenne globale",
        hoverinfo = "none",
        showlegend = FALSE
      )
    
    # shapes et annotations (une seule fois, après tout calcul)
    shapes_list <- list(
      list(type = "rect", xref = "x", yref = "y",
           x0 = rect_x0, x1 = rect_x1, y0 = global_low, y1 = global_high,
           fillcolor = "#7D3C98", opacity = 0.08, line = list(width = 0), layer = "below")
    )
    
    # positionnement titre / sous-titre : ajustez si nécessaire
    title_y    <- 0.99
    subtitle_y <- 0.96
    top_margin <- if (isTRUE(input$show_lm) && nzchar(trend_msg)) 160 else 120
    
    annotations_list <- list(
      list(text = subtitle_text,
           xref = "paper", x = 0, yref = "paper", y = subtitle_y,
           showarrow = FALSE, xanchor = "left",
           font = list(size = 12, color = "#444")),
      list(x = rect_x1, y = global_mean, xref = "x", yref = "y",
           text = mean_label_text, showarrow = FALSE, xanchor = "left", yanchor = "bottom",
           font = list(color = "#7D3C98"))
    )
    
    # titre principal (sans trend_msg)
    title_text <- paste0("Évolution - ", var_label)
    
    # layout final (une seule fois)
    plt <- plt %>% layout(
      shapes = shapes_list,
      annotations = annotations_list,
      title = list(text = title_text, y = title_y),
      xaxis = list(title = "Année", autorange = TRUE),
      yaxis = list(title = var_label, autorange = TRUE),
      hovermode = "closest",
      margin = list(t = top_margin, b = 120, l = 80, r = 40),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18, yanchor = "top")
    )
    
    plt
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
