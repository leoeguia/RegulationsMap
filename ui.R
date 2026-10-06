library(shiny)
library(shinythemes)
library(leaflet)
library(maps)
library(rintrojs)
library(sf)
library(dplyr)


ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel(
    "Gulf Fisheries Regulations Map",
    windowTitle = "Gulf Regulations Map"
  ),
  
  introjsUI(),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      checkboxInput(
        inputId = "anchoring",
        label = "Highlight areas where anchoring is not allowed",
        value = FALSE
      ),
      
      br(),
      
      # Sector filters
      checkboxGroupInput(
        inputId = "sector",
        label = "Select Fishing Sector:",
        choices = c(
          "Recreational" = "rec",
          "Commercial" = "comm"
        ),
        selected = character(0)
      ),
      
      conditionalPanel(
        condition = "input.sector.includes('comm')",
        checkboxGroupInput(
          inputId = "species_group",
          label = "Fishery:",
          choices = c(
            "Mackerel/Cobia",
            "Reef Fish",
            "Spiny Lobster",
            "Shrimp"
          ),
          selected = character(0)
        )
      ),
      
      conditionalPanel(
        condition = "input.sector.includes('comm')",
        
        checkboxGroupInput(
          inputId = "restriction_type",
          label = "Filter for specific restriction type:",
          choices = c(
            "Gear" = "gear",
            "Possession" = "possession"
          ),
          selected = character(0)
        )
      ),
      
      conditionalPanel(
        condition = "input.sector.includes('rec') || 
               (input.sector.includes('comm') && 
                input.species_group.includes('Reef Fish'))",
        
        checkboxGroupInput(
          inputId = "reef_special_areas",
          label = "Reef Fish Areas:",
          choices = c(
            "Gulf Hogfish Management Area" = "hogfish",
            "Reef Fish Stressed Area" = "stressed"
          ),
          selected = character(0)
        )
      ),
      
      br(),
      
      img(
        src = "logo.jpg",
        width = "75%",
        align = "center",
        style = "display:block; margin:auto;"
      ),
      
      tags$p(
        style = "text-align:center;",
        "4107 W. Spruce St. Suite 200",
        br(),
        "Tampa, FL 33607",
        br(),
        "Email: gulfcouncil@gulfcouncil.org"
      ),
      helpText("This tool only provides information",
               "on regulations made by the Gulf Council",
               "and NOAA NMFS")
    ),
    
    mainPanel(
      width = 10,
      
      leafletOutput(
        "map",
        height = "800px"
      )
    )
  )
)