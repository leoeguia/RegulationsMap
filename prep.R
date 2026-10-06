# library(shiny)
# library(shinythemes)
# library(leaflet)
# library(sf)
# library(maps)
# library(dplyr)
# 
# 
# # ============================================================
# # 1. READ DATA
# # ============================================================
# rm(list=ls())
# df <- read.csv("GOM_Areas_Regulations_Combo.csv")
# 
# newdat <- st_read(
#   "GOM_Fishery/GOM_FMA_Combo.shp",
#   quiet = TRUE
# )
# 
# newdat <- st_transform(newdat, 4326)
# 
# 
# # ============================================================
# # 2. FUNCTION TO PARSE COORDINATES FROM df
# # ============================================================
# 
# parse_coords <- function(x) {
#   
#   if (is.na(x) || trimws(x) == "") {
#     return(NULL)
#   }
#   
#   # Fix mangled degree/minute characters
#   x <- gsub("┬░", "°", x, fixed = TRUE)
#   x <- gsub("ΓÇ▓", "'", x, fixed = TRUE)
#   
#   # ----------------------------------------------------------
#   # DMS format
#   # ----------------------------------------------------------
#   
#   if (grepl("°", x, fixed = TRUE)) {
#     
#     pairs <- unlist(
#       regmatches(
#         x,
#         gregexpr(
#           "[0-9]+\\.?[0-9]*°[0-9]+\\.?[0-9]*'\\s*,\\s*[0-9]+\\.?[0-9]*°[0-9]+\\.?[0-9]*'",
#           x
#         )
#       )
#     )
#     
#     coords <- do.call(
#       rbind,
#       lapply(pairs, function(p) {
#         
#         nums <- as.numeric(
#           unlist(
#             regmatches(
#               p,
#               gregexpr(
#                 "[0-9]+\\.?[0-9]*",
#                 p
#               )
#             )
#           )
#         )
#         
#         lat <- nums[1] + nums[2] / 60
#         lon <- -(nums[3] + nums[4] / 60)
#         
#         c(lon, lat)
#       })
#     )
#     
#   } else {
#     
#     # --------------------------------------------------------
#     # Decimal format
#     # --------------------------------------------------------
#     
#     pairs <- strsplit(x, ",")[[1]]
#     
#     pairs <- gsub(
#       "^[[:space:]]*[A-Z]:[[:space:]]*",
#       "",
#       pairs
#     )
#     
#     nums <- as.numeric(
#       unlist(
#         lapply(pairs, function(p) {
#           
#           as.numeric(
#             unlist(
#               regmatches(
#                 p,
#                 gregexpr(
#                   "-?[0-9]+\\.?[0-9]*",
#                   p
#                 )
#               )
#             )
#           )
#           
#         })
#       )
#     )
#     
#     coords <- matrix(
#       nums,
#       ncol = 2,
#       byrow = TRUE
#     )
#     
#     # Decimal coordinates are west longitude
#     coords[, 1] <- -abs(coords[, 1])
#   }
#   
#   # Close polygon
#   if (!all(coords[1, ] == coords[nrow(coords), ])) {
#     coords <- rbind(
#       coords,
#       coords[1, ]
#     )
#   }
#   
#   coords
# }
# 
# 
# # ============================================================
# # 3. CREATE POLYGONS FROM THE CSV COORDINATES
# # ============================================================
# 
# coords <- lapply(
#   df$BCoords_LAT_LON,
#   parse_coords
# )
# 
# has_coords <- !sapply(
#   coords,
#   is.null
# )
# 
# df_map <- df[has_coords, ]
# coords_map <- coords[has_coords]
# 
# 
# # ============================================================
# # 4. FIX THE THREE INVALID POLYGONS
# # ============================================================
# 
# # ID 5
# coords_map[[5]] <- coords_map[[5]][
#   c(1:8, 9, 11, 10, 12),
# ]
# 
# # ID 7
# coords_map[[7]] <- coords_map[[7]][
#   c(1, 2, 4, 5, 3, 6),
# ]
# 
# # ID 36
# coords_map[[24]] <- coords_map[[24]][
#   c(1, 3, 2, 4, 5),
# ]
# 
# 
# # ============================================================
# # 5. CREATE sf OBJECT FROM CSV COORDINATES
# # ============================================================
# 
# df_sf <- st_sf(
#   df_map,
#   geometry = st_sfc(
#     lapply(
#       coords_map,
#       function(x) {
#         st_polygon(list(x))
#       }
#     ),
#     crs = 4326
#   )
# )
# 
# 
# # Check geometry
# stopifnot(
#   all(st_is_valid(df_sf))
# )
# 
# 
# # ============================================================
# # 6. CREATE MATCHING NAMES
# # ============================================================
# 
# # Names in df_sf
# df_sf <- df_sf %>%
#   mutate(match_name = Name)
# 
# 
# # Names in newdat
# newdat <- newdat %>%
#   mutate(
#     match_name = recode(
#       Name,
#       
#       "East Flower Garden Bank" =
#         "East Flower Garden Banks",
#       
#       "West Flower Garden Bank" =
#         "West Flower Garden Banks",
#       
#       "Pulley Ridge" =
#         "Pulley Ridge HAPC",
#       
#       "KingMackarel Western Zone" =
#         "King Mackarel Western Zone",
#       
#       "KingMackarel Florida West Coast Northern" =
#         "King Mackarel Northern Zone",
#       
#       "KingMackarel Florida West Coast Southern" =
#         "King Mackarel Southern Zone"
#     )
#   )
# 
# df_sf %>%
#   st_drop_geometry() %>%
#   filter(Name %in% c(
#     "Pulley Ridge HAPC",
#     "East Flower Garden Banks",
#     "West Flower Garden Banks"
#   )) %>%
#   select(Name, match_name)
# # ============================================================
# # 7. CLEAN UP THE DISPLAY NAMES IN THE NEW SHAPEFILE
# # ============================================================
# 
# newdat$Name[
#   newdat$Name == "KingMackarel Western Zone"
# ] <- "King Mackerel Western Zone"
# 
# newdat$Name[
#   newdat$Name == "KingMackarel Florida West Coast Northern"
# ] <- "King Mackerel Florida West Coast Northern"
# 
# newdat$Name[
#   newdat$Name == "KingMackarel Florida West Coast Southern"
# ] <- "King Mackerel Florida West Coast Southern"
# 
# 
# # ============================================================
# # 8. IDENTIFY UNIQUE NEW POLYGONS
# # ============================================================
# 
# # Tortugas has five polygons with the same name, so handle
# # those separately below.
# 
# newdat_unique <- newdat %>%
#   filter(
#     Name != "Tortugas Shrimp Sanctuary"
#   )
# 
# 
# # ============================================================
# # 9. REPLACE OLD CSV GEOMETRY WITH NEW SHAPEFILE GEOMETRY
# # ============================================================
# 
# # Start with ALL regulatory records from df
# combined <- df
# 
# # Match each df record to its corrected geometry in df_sf
# # using the unique Name field
# df_index <- match(
#   combined$Name,
#   df_sf$Name
# )
# 
# # Add the corrected df_sf geometry to the matching df rows
# # Rows without coordinates will initially have no geometry
# combined <- st_as_sf(
#   combined,
#   geometry = st_geometry(df_sf)[df_index],
#   crs = 4326
# )
# 
# # Create the matching key
# combined <- combined %>%
#   mutate(
#     match_name = Name
#   )
# 
# 
# # Replace geometry with newdat geometry for areas where
# # there is exactly ONE matching polygon in newdat
# 
# newdat_one <- newdat %>%
#   add_count(match_name) %>%
#   filter(n == 1)
# 
# new_index <- match(
#   combined$match_name,
#   newdat_one$match_name
# )
# 
# has_new_geometry <- !is.na(new_index)
# 
# combined$geometry[has_new_geometry] <-
#   st_geometry(newdat_one)[
#     new_index[has_new_geometry]
#   ]
# # ============================================================
# # 10. ADD MULTI-POLYGON AREAS FROM NEW SHAPEFILE
# # ============================================================
# 
# # These areas have multiple polygons in newdat but only
# # one regulatory record in df.
# 
# multi_names <- c(
#   "Reef Fish Stressed Area",
#   "King Mackarel Western Zone",
#   "King Mackarel Northern Zone",
#   "King Mackarel Southern Zone"
# )
# 
# 
# 
# # Get the df attributes and create the matching key
# 
# 
# multi_attributes <- df %>%
#   filter(Name %in% multi_names) %>%
#   mutate(
#     match_name = Name
#   )
# 
# 
# 
# # Get the newdat polygons
# 
# 
# multi_polygons <- newdat %>%
#   filter(match_name %in% multi_names)
# 
# 
# 
# # Remove the empty df records for these areas from combined
# 
# 
# combined <- combined %>%
#   filter(
#     !Name %in% multi_names
#   )
# 
# 
# 
# # Attach the df attributes to each newdat polygon
# 
# multi_polygons <- multi_polygons %>%
#   left_join(
#     multi_attributes %>%
#       st_drop_geometry(),
#     by = "match_name",
#     suffix = c(".newdat", "")
#   )
# 
# 
# # Keep the df attributes plus the newdat geometry
# 
# multi_polygons <- multi_polygons %>%
#   select(
#     all_of(names(df)),
#     geometry
#   )
# 
# 
# 
# # Add the multi-polygon areas to combined
# 
# combined <- bind_rows(
#   combined,
#   multi_polygons
# )
# 
# 
# # Recreate the matching key
# combined <- combined %>%
#   mutate(
#     match_name = Name
#   )
# combined %>%
#   filter(
#     Name %in% c(
#       "Reef Fish Stressed Area",
#       "King Mackarel Western Zone",
#       "King Mackarel Northern Zone",
#       "King Mackarel Southern Zone"
#     )
#   ) %>%
#   mutate(
#     has_geometry = !st_is_empty(geometry)
#   ) %>%
#   st_drop_geometry() %>%
#   count(Name, has_geometry)
# 
# # ============================================================
# # 11. ADD SPECIES-GROUP FILTER
# # ============================================================
# 
# combined <- combined %>%
#   mutate(
#     Species_Group_Filter = case_when(
#       
#       Species_Group ==
#         "Mackerel/Cobia, Reef Fish, Spiny Lobster, Shrimp" ~
#         
#         "Mackerel/Cobia;Reef Fish;Spiny Lobster;Shrimp",
#       
#       TRUE ~
#         
#         Species_Group
#     )
#   )
# 
# 
# # ============================================================
# # 12. HANDLE THE FIVE TORTUGAS SHRIMP SANCTUARY POLYGONS
# # ============================================================
# 
# # tortugas_new <- newdat %>%
# #   filter(Name == "Tortugas Shrimp Sanctuary") %>%
# #   arrange(desc(st_area(geometry)))
# # 
# # tortugas_new$df_name <- c(
# #   "Tortugas Shrimp Sanctuary NFGHP",
# #   "Tortugas Shrimp Sanctuary WGV",
# #   "Tortugas Shrimp Sanctuary TUVW",
# #   "Tortugas Shrimp Sanctuary FQUT",
# #   "Tortugas Shrimp Sanctuary NFGHP"
# # )
# # 
# # tortugas_attributes <- df %>%
# #   filter(
# #     Name %in% unique(tortugas_new$df_name)
# #   )
# # tortugas_new <- tortugas_new %>%
# #   left_join(
# #     tortugas_attributes,
# #     by = c("df_name" = "Name")
# #   )
# # 
# # tortugas_new %>%
# #   st_drop_geometry() %>%
# #   select(
# #     df_name,
# #     Recreational,
# #     Commercial,
# #     Anchoring_Allowed,
# #     Species_Group,
# #     appearance,
# #     Regulation
# #   )
# # 
# # 
# # for (i in 1:4) {
# #   
# #   idx <- which(combined$Name == tortugas_new$df_name[i])
# #   
# #   combined$geometry[idx] <- tortugas_new$geometry[i]
# # }
# # 
# # nfgp2 <- combined %>%
# #   filter(Name == "Tortugas Shrimp Sanctuary NFGHP") %>%
# #   mutate(
# #     Name = "Tortugas Shrimp Sanctuary NFGHP2",
# #     geometry = tortugas_new$geometry[5]
# #   )
# # combined <- bind_rows(
# #   combined,
# #   nfgp2
# # )
# 
# 
# # New ---------------------------------------------------------------------
# 
# # Get the five Tortugas polygons from newdat
# tortugas_new <- newdat %>%
#   filter(
#     Name == "Tortugas Shrimp Sanctuary"
#   )
# 
# 
# # ------------------------------------------------------------
# # Determine southwest → northeast order
# # ------------------------------------------------------------
# 
# tortugas_centroids <- st_centroid(
#   tortugas_new$geometry
# )
# 
# tortugas_new <- tortugas_new %>%
#   mutate(
#     centroid_lon = st_coordinates(tortugas_centroids)[, 1],
#     centroid_lat = st_coordinates(tortugas_centroids)[, 2]
#   ) %>%
#   arrange(
#     centroid_lon,
#     centroid_lat
#   )
# 
# 
# # ------------------------------------------------------------
# # Assign the df records in southwest → northeast order
# # ------------------------------------------------------------
# 
# tortugas_new$df_name <- c(
#   "Tortugas Shrimp Sanctuary NFGHP",
#   "Tortugas Shrimp Sanctuary WGV",
#   "Tortugas Shrimp Sanctuary TUVW",
#   "Tortugas Shrimp Sanctuary FQUT",
#   "Tortugas Shrimp Sanctuary NFGHP"
# )
# 
# 
# # Check the assignment before proceeding
# tortugas_new %>%
#   st_drop_geometry() %>%
#   select(
#     Name,
#     centroid_lon,
#     centroid_lat,
#     df_name
#   )
# 
# 
# # ------------------------------------------------------------
# # Get the regulatory attributes from df
# # ------------------------------------------------------------
# 
# tortugas_attributes <- df %>%
#   filter(
#     Name %in% unique(tortugas_new$df_name)
#   )
# 
# 
# tortugas_new <- tortugas_new %>%
#   left_join(
#     tortugas_attributes,
#     by = c("df_name" = "Name")
#   )
# 
# 
# # ------------------------------------------------------------
# # Replace geometry for the four existing Tortugas records
# # ------------------------------------------------------------
# 
# for (i in 1:4) {
#   
#   idx <- which(
#     combined$Name == tortugas_new$df_name[i]
#   )
#   
#   combined$geometry[idx] <-
#     tortugas_new$geometry[i]
# }
# 
# 
# # ------------------------------------------------------------
# # Add the fifth polygon as NFGHP2
# # ------------------------------------------------------------
# 
# nfgp2 <- combined %>%
#   filter(
#     Name == "Tortugas Shrimp Sanctuary NFGHP"
#   ) %>%
#   mutate(
#     Name = "Tortugas Shrimp Sanctuary NFGHP2",
#     geometry = tortugas_new$geometry[5]
#   )
# 
# 
# combined <- bind_rows(
#   combined,
#   nfgp2
# )
# 
# ##Check
# tortugas_new %>%
#   st_drop_geometry() %>%
#   select(
#     Name,
#     centroid_lon,
#     centroid_lat,
#     df_name
#   )
# valid <- st_is_valid(combined)
# 
# combined %>%
#   st_drop_geometry() %>%
#   mutate(
#     row = row_number(),
#     valid = valid,
#     reason = st_is_valid(combined, reason = TRUE)
#   ) %>%
#   filter(!valid) %>%
#   select(row, Name, reason)
# 
# # How many records do we have?
# nrow(combined)
# 
# # Compare original and final area names
# setdiff(df$Name, combined$Name)
# 
# # Anything in combined that wasn't in the original df?
# setdiff(combined$Name, df$Name)
# 
# # Check for duplicate names
# combined %>%
#   count(Name) %>%
#   filter(n > 1) %>%
#   arrange(desc(n))
# 
# # ============================================================
# # 13. ADD UNIQUE ID FOR MAP FEATURES
# # ============================================================
# 
# combined <- combined %>%
#   mutate(
#     map_id = row_number()
#   )
# 
# 
# # ============================================================
# # 14. FINAL GEOMETRY CHECK
# # ============================================================
# 
# combined <- st_as_sf(
#   combined,
#   crs = 4326
# )
# 
# valid <- st_is_valid(combined)
# 
# combined %>%
#   st_drop_geometry() %>%
#   mutate(
#     row = row_number(),
#     valid = valid,
#     reason = st_is_valid(
#       combined,
#       reason = TRUE
#     )
#   ) %>%
#   filter(!valid) %>%
#   select(
#     row,
#     Name,
#     reason
#   )
# # ============================================================
# # 15. CHECK THE FINAL DATASET
# # ============================================================
# 
# cat(
#   "Rows in original df:",
#   nrow(df),
#   "\n"
# )
# 
# cat(
#   "Rows in df with coordinates:",
#   nrow(df_sf),
#   "\n"
# )
# 
# cat(
#   "Rows in final combined:",
#   nrow(combined),
#   "\n"
# )
# 
# cat(
#   "Invalid geometries:",
#   sum(!st_is_valid(combined)),
#   "\n"
# )
# 
# 
# # See the final areas
# 
# combined %>%
#   st_drop_geometry() %>%
#   select(
#     map_id,
#     Name,
#     Recreational,
#     Commercial,
#     Anchoring_Allowed,
#     Species_Group,
#     appearance,
#     Regulation
#   )
# 
# ###Fishing Restrictions
# combined$Fishing_Gear_Prohibition[c(39,40,41,42,47,48,49,50)] <- combined$Regulation[c(39,40,41,42,47,48,49,50)]
# combined$Posession[c(51,52,54,55)] <- combined$Regulation[c(51,52,54,55)]
# ###Drop shallow water grouper closure
# combined <- combined[-c(13,15,43),]
# 
# 
# 
# 
# # Polygon Checks ----------------------------------------------------------
# # combined %>%
# #   filter(st_is_empty(geometry) | is.na(st_dimension(geometry))) %>%
# #   st_drop_geometry() %>%
# #   select(Name)
# # 
# # combined %>%
# #   mutate(
# #     has_geometry = !st_is_empty(geometry) &
# #       !is.na(st_dimension(geometry))
# #   ) %>%
# #   st_drop_geometry() %>%
# #   select(Name, has_geometry)
# # 
# # combined %>%
# #   filter(st_is_empty(geometry)) %>%
# #   st_drop_geometry() %>%
# #   select(
# #     Name,
# #     BCoords_LAT_LON
# #   )
# 
# 
# # ============================================================
# # Add state boundaries
# # ============================================================
# 
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# states <- st_transform(states, st_crs(df_sf))
# 
# # ============================================================
# # 16. SAVE FINAL MAP DATA
# # ============================================================
# 
# save(
#   combined,
#   file = "combined.RData"
# )
