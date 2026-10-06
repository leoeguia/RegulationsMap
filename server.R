server <- function(input, output, session) {
  
  # output$map <- renderLeaflet({
  #   
  #   leaflet() %>%
  #     addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  #     setView(
  #       lng = -89.5,
  #       lat = 27.5,
  #       zoom = 6
  #     ) %>%
  #     addPolygons(
  #       data = df_sf,
  #       fill = FALSE,
  #       color = "red",
  #       weight = 2,
  #       opacity = 1,
  #       group = "Management Areas",
  #       label = ~Area_Name,
  #       popup = ~paste0(
  #         "<strong>", Area_Name, "</strong><br>",
  #         "<strong>Recreational:</strong> ", Recreational, "<br>",
  #         "<strong>Commercial:</strong> ", Commercial, "<br><br>",
  #         "<strong>Regulation:</strong><br>",
  #         Regulation
  #       )
  #     )
  #   
  # })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addProviderTiles(providers$Stadia.StamenTerrainLabels)%>%
      setView(
        lng = -89.5,
        lat = 27.5,
        zoom = 6
      )%>%
    addPolylines(data = states, color = "black",
                 weight = 1, opacity = 0.5,fill = FALSE) 
  })
  observe({
    
    sector_match <- rep(FALSE, nrow(combined))
    
    special_names <- c(
      "Gulf Hogfish Management Area",
      "Reef Fish Stressed Area"
    )
    
    special_area <- combined$Name %in% special_names
    # Recreational filtering
    if ("rec" %in% input$sector) {
      sector_match <- sector_match |
        (combined$Recreational == "yes" & !special_area)
    }
    
    # Commercial filtering, including species group
    if ("comm" %in% input$sector) {
      
      commercial_match <- 
        combined$Commercial == "yes" &
        !special_area
      
      if (!is.null(input$species_group) &&
          length(input$species_group) > 0) {
        
        commercial_match <- commercial_match &
          vapply(
            combined$Species_Group_Filter,
            function(x) {
              if (is.na(x)) return(FALSE)
              
              any(
                strsplit(x, ";", fixed = TRUE)[[1]] %in%
                  input$species_group
              )
            },
            logical(1)
          )
        
      } else {
        
        commercial_match[] <- FALSE
        
      }
      
      # Commercial restriction type filtering
      if (!is.null(input$restriction_type) &&
          length(input$restriction_type) > 0) {
        
        restriction_match <- rep(FALSE, nrow(combined))
        
        if ("gear" %in% input$restriction_type) {
          restriction_match <- restriction_match |
            !is.na(combined$Fishing_Gear_Prohibition) &
            combined$Fishing_Gear_Prohibition != ""
        }
        
        if ("possession" %in% input$restriction_type) {
          restriction_match <- restriction_match |
            !is.na(combined$Posession) &
            combined$Posession != ""
        }
        
        commercial_match <- commercial_match & restriction_match
      }
      
      sector_match <- sector_match | commercial_match
    }
    
    # Anchoring-prohibited areas remain independent of sector filtering
    anchoring_match <- if (isTRUE(input$anchoring)) {
      combined$Anchoring_Allowed == "no"
    } else {
      rep(FALSE, nrow(combined))
    }
    
   # areas <- combined[sector_match | anchoring_match, ]
    # Special reef fish areas
    special_match <- rep(FALSE, nrow(combined))
    
    if (!is.null(input$reef_special_areas) &&
        length(input$reef_special_areas) > 0) {
      
      if ("hogfish" %in% input$reef_special_areas) {
        special_match <- special_match |
          combined$Name == "Gulf Hogfish Management Area"
      }
      
      if ("stressed" %in% input$reef_special_areas) {
        special_match <- special_match |
          combined$Name == "Reef Fish Stressed Area"
      }
    }
    
    areas <- combined[
      sector_match |
        anchoring_match |
        special_match,
    ]
    
    areas$sector_match <- sector_match[
      sector_match |
        anchoring_match |
        special_match
    ]
    
    areas$anchoring_match <- anchoring_match[
      sector_match |
        anchoring_match |
        special_match
    ]
    
    areas$special_match <- special_match[
      sector_match |
        anchoring_match |
        special_match
    ]
    
    areas <- areas %>%
      arrange(desc(as.numeric(st_area(geometry))))
    
    proxy <- leafletProxy("map") %>%
      clearGroup("Management Areas")
    
    if (nrow(areas) == 0) {
      return()
    }
    
    proxy %>%
      addPolygons(
        data = areas,
        fillColor = ~ifelse(
          input$anchoring & Anchoring_Allowed == "no",
          "#FF0000",
          ifelse(
            appearance %in% c("solid", "gear"),
            "#E69F00",
            "#FFFFFF"
          )
        ),
        fillOpacity = ~ifelse(
          input$anchoring & Anchoring_Allowed == "no",
          0.55,
          ifelse(
            appearance %in% c("solid", "gear"),
            0.30,
            0
          )
        ),
        color = ~ifelse(
          anchoring_match & (sector_match | special_match),
          "#000000",
          ifelse(
            anchoring_match,
            "#FF0000",
            ifelse(
              appearance %in% c(
                "temporary-hatched",
                "temporary - hatched"
              ),
              "#D55E00",
              "#000000"
            )
          )
        ),
        weight = ~ifelse(
          input$anchoring & Anchoring_Allowed == "no",
          3,
          2
        ),
        opacity = 1,
        dashArray = ~ifelse(
          appearance == "line - dashed",
          "8, 6",
          "0"
        ),
        group = "Management Areas",
        label = ~Name,
        popup = ~paste0(
          "<strong>", Name, "</strong><br>",
          "<strong>Recreational:</strong> ", Recreational, "<br>",
          "<strong>Commercial:</strong> ", Commercial, "<br>",
          "<strong>Species Group:</strong> ", Species_Group, "<br>",
          "<strong>Anchoring Allowed:</strong> ",
          Anchoring_Allowed,
          "<br><br>",
          "<strong>Regulation:</strong><br>",
          Regulation
        )
      )
  })
  
}