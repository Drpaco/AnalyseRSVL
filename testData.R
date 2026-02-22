setwd("/cloud/project/AnalyseRSVL")


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

names(rsvl)
df1 <- rsvl[rsvl$nom_lac=='Ã‰meraude, Lac',]
table(rsvl$nom_lac)

library(sf)
st_layers("rsvl_20230815.gpkg")

