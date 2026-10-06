
# Load Packages -----------------------------------------------------------
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(maps)
library(dplyr)

# Load Data ---------------------------------------------------------------

load("combined.RData")

# ============================================================
# Add state boundaries
# ============================================================

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- st_transform(states, st_crs(df_sf))
