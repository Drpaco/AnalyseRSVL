library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(ggplot2)

# -----------------
# 1. Safe loader
# -----------------
safe_load_rsvl <- function() {
  tryCatch({
    message("Trying to read local RSVL database...")
    st_read("rsvl_latest.gpkg",
            layer = "etatsTrophiques_stations",
            quiet = TRUE)
  }, error = function(e) {
    message("Local read failed. Will download new copy...")
    return(NULL)
  })
}

# -----------------
# 2. Downloader
# -----------------
download_rsvl_data <- function() {
  url <- "https://www.donneesquebec.ca/recherche/dataset/92f2189f-630e-4a68-83b7-fd8b0ae42c3c/resource/33ffb872-5d79-4a70-ac44-53420705f728/download/etats-trophiques.gpkg.zip"
  
  dest_zip <- "rsvl_latest.zip"
  dest_gpkg <- "rsvl_latest.gpkg"
  
  download.file(url, destfile = dest_zip, mode = "wb")
  unzip(dest_zip, exdir = ".")
  return(dest_gpkg)
}

# -----------------
# 3. Startup fallback logic
# -----------------
rsvl <- safe_load_rsvl()

if (is.null(rsvl)) {
  gpkg_file <- download_rsvl_data()
  rsvl <- tryCatch({
    st_read(gpkg_file,
            layer = "etatsTrophiques_stations",
            quiet = TRUE)
  }, error = function(e) {
    stop("Unable to load RSVL dataset even after downloading.")
  })
}

# Drop geometry for Shiny usage
rsvl_df <- rsvl %>% st_drop_geometry()


# ===============================
# UI
# ===============================
ui <- dashboardPage(
  dashboardHeader(title = "RSVL Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line"))
    ),
    
    selectInput("lac", "Lake:", 
                choices = c("All", sort(unique(rsvl_df$nom_lac))),
                selected = "All"),
    
    selectInput("bv1", "BV1:",
                choices = c("All", sort(unique(rsvl_df$bv1))),
                selected = "All"),
    
    selectInput("bv2", "BV2:",
                choices = c("All", sort(unique(rsvl_df$bv2))),
                selected = "All"),
    
    selectInput("variable", "Variable:",
                choices = c(
                  "Total phosphorus" = "phosphore_total",
                  "Chlorophyll-a" = "chlorophylle_a",
                  "Transparency (Secchi)" = "transparence",
                  "Trophic state" = "etat_trophique"
                )),
    actionButton("update_data", "Update RSVL Database",
                 icon = icon("download"), class = "btn-primary")
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeseries",
              fluidRow(
                box(width = 12,
                    title = "Change in measurement by year",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("timeplot", height = "450px"))
              )
      )
    )
  )
)

# ===============================
# Server
# ===============================
server <- function(input, output, session) {
  
  observeEvent(input$update_data, {
    showModal(modalDialog("Downloading latest RSVL data...", footer = NULL))
    
    gpkg_file <- download_rsvl_data()
    
    new_data <- st_read(gpkg_file,
                        layer = "etatsTrophiques_stations",
                        quiet = TRUE) %>% 
      st_drop_geometry()
    
    rsvl_df <<- new_data
    
    updateSelectInput(session, "lac",
                      choices = c("All", sort(unique(rsvl_df$nom_lac))))
    updateSelectInput(session, "bv1",
                      choices = c("All", sort(unique(rsvl_df$bv1))))
    updateSelectInput(session, "bv2",
                      choices = c("All", sort(unique(rsvl_df$bv2))))
    
    removeModal()
    showModal(modalDialog("RSVL database updated!", easyClose = TRUE))
  })
  
  # Filtering logic
  filtered <- reactive({
    df <- rsvl_df
    
    if (input$lac != "All") {
      df <- df %>% filter(nom_lac == input$lac)
    }
    if (input$bv1 != "All") {
      df <- df %>% filter(bv1 == input$bv1)
    }
    if (input$bv2 != "All") {
      df <- df %>% filter(bv2 == input$bv2)
    }
    
    df
  })
  
  # Plot time series
  output$timeplot <- renderPlot({
    df <- filtered()
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = annee, y = .data[[input$variable]])) +
      geom_line(colour = "#2C3E50", size = 1) +
      geom_point(colour = "#E74C3C", size = 2) +
      theme_minimal(base_size = 16) +
      labs(
        x = "Year",
        y = input$variable,
        title = paste("Yearly Evolution of", input$variable),
        subtitle = paste(
          if (input$lac != "All") paste("Lake:", input$lac) else "",
          if (input$bv1 != "All") paste("| BV1:", input$bv1) else "",
          if (input$bv2 != "All") paste("| BV2:", input$bv2) else ""
        )
      )
  })
}

# ===============================
# Run App
# ===============================
shinyApp(ui, server)