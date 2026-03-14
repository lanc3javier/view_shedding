run_antenna_optimization <- function() {
  
  # --- 1. DASHBOARD ---
  antenna_h        <- 8.0               
  robot_h          <- 1.0               
  corn_h           <- 2.5               
  max_slope        <- 5.0                
  top_pct          <- 0.05               
  input_folder     <- "input_fields"    
  export_base      <- "Field_Analysis_Results"
  point_density    <- 15                 
  contour_interval <- 1.0            
  map_zoom_margin  <- 25             
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(terra, sf, elevatr, whitebox, tidyverse, 
                 tidyterra, ggnewscale, ggspatial)
  
  wbt_init()
  main_dir <- getwd()
  all_files <- list.files(path = input_folder, pattern = "\\.(kml|gpkg|shp|geojson)$", 
                          full.names = TRUE, ignore.case = TRUE)
  
  for (i in 1:length(all_files)) {
    file_path  <- all_files[i]
    field_name <- tools::file_path_sans_ext(basename(file_path))
    
    tryCatch({
      message(paste0("\n--- Analyzing: ", field_name, " ---"))
      
      # Dynamic Folder Naming: field_x_results
      field_folder <- file.path(main_dir, export_base, paste0(field_name, "_results"))
      if(!dir.exists(field_folder)) dir.create(field_folder, recursive = TRUE)
      
      # Step A: Prep Spatial Data
      field <- st_read(file_path, quiet = TRUE) %>% st_zm(drop = TRUE) %>% st_make_valid()
      field_gps <- st_transform(field, 4326) 
      lon_center <- st_coordinates(st_centroid(st_union(field_gps)))[1]
      utm_zone   <- floor((lon_center + 180) / 6) + 1
      field_utm  <- st_transform(field_gps, as.integer(paste0("326", utm_zone)))
      
      # Step B: Get DEM (Dynamic Naming)
      dem_raw <- get_elev_raster(field_utm, z = 14, clip = "bbox", progress = FALSE)
      dem <- rast(dem_raw)
      dem_path <- file.path(field_folder, paste0(field_name, "_dem.tif"))
      writeRaster(dem, dem_path, overwrite = TRUE)
      
      # Step C: Visibility Testing
      field_perim <- st_cast(field_utm, "LINESTRING")
      candidates  <- st_line_sample(field_perim, density = 1/point_density) %>% st_cast("POINT") %>% st_as_sf()
      
      rel_h <- antenna_h - (robot_h + corn_h)
      candidates$coverage_pct <- NA
      
      temp_p <- tempfile(fileext = ".shp")
      temp_v <- tempfile(fileext = ".tif")
      
      # Mask for accurate calculations
      f_mask <- mask(dem, vect(field_utm))
      total_px <- sum(!is.na(values(f_mask)))
      
      for(j in 1:nrow(candidates)) {
        st_write(candidates[j,], temp_p, quiet=TRUE, delete_dsn=TRUE)
        wbt_viewshed(dem = dem_path, stations = temp_p, output = temp_v, height = rel_h)
        v_loop <- mask(rast(temp_v), vect(field_utm))
        candidates$coverage_pct[j] <- (sum(values(v_loop) == 1, na.rm=T) / total_px) * 100
      }
      
      # Step D: Final Grid Generation & Top 5%
      best_spot <- candidates[which.max(candidates$coverage_pct), ]
      best_zone <- candidates %>% filter(coverage_pct >= quantile(candidates$coverage_pct, 1 - top_pct, na.rm=TRUE))
      
      st_write(best_spot, temp_p, delete_dsn=TRUE, quiet=TRUE)
      wbt_viewshed(dem_path, temp_p, temp_v, height = rel_h)
      
      # Re-import final result safely
      final_v_rast <- rast(temp_v)
      final_v_rast <- mask(final_v_rast, vect(field_utm))
      
      # Convert to Robot-readable Polygons (QGIS map layer)
      sig_zones <- as.polygons(final_v_rast, aggregate = TRUE) %>% 
        st_as_sf() %>% 
        st_make_valid()
      
      names(sig_zones)[1] <- "Signal_Code"
      sig_zones$Status <- ifelse(sig_zones$Signal_Code == 1, "LIVE", "DEAD")
      
      # --- STEP E: EXPORTS ---
      
      # 1. Map Generation (Hillshade, Contours, Labels, Zoomed In)
      zoom_box <- st_bbox(field_utm)
      
      # Prepare Hillshade using a buffer to ensure it fills the zoomed frame
      dem_cropped <- crop(dem, vect(st_buffer(field_utm, map_zoom_margin * 2)))
      hill_shade  <- shade(terrain(dem_cropped, "slope", unit="radians"), 
                           terrain(dem_cropped, "aspect", unit="radians"))
      
      plot_out <- ggplot() +
        # Background Hillshade
        geom_spatraster(data = hill_shade) +
        scale_fill_gradient(low = "grey20", high = "white", guide = "none") +
        new_scale_fill() + 
        # Signal Overlay
        geom_spatraster(data = as.factor(final_v_rast), alpha = 0.5) +
        scale_fill_manual(values = c("0" = "firebrick", "1" = "springgreen3"), 
                          name = "Robot Link", labels = c("Dead Zone", "Live Link"), na.translate=F) +
        # Contours & Numbers
        geom_spatraster_contour(data = dem_cropped, binwidth = contour_interval, color = "black", linewidth = 0.2) +
        geom_spatraster_contour_text(data = dem_cropped, binwidth = contour_interval, size = 3, color = "black", fontface = "bold") +
        # Field Boundaries and Points
        geom_sf(data = field_utm, fill = NA, color = "black", linewidth = 1) +
        geom_sf(data = best_zone, color = "cyan", size = 2) +
        geom_sf(data = best_spot, color = "blue", size = 4, shape = 17) +
        # North Arrow & Scale Bar
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
        # Zoom to bounding box + margin
        coord_sf(xlim = c(zoom_box[["xmin"]] - map_zoom_margin, zoom_box[["xmax"]] + map_zoom_margin), 
                 ylim = c(zoom_box[["ymin"]] - map_zoom_margin, zoom_box[["ymax"]] + map_zoom_margin), expand = FALSE) +
        theme_minimal() + 
        # Descriptive Labels
        labs(title = paste("Coverage Map:", field_name), 
             subtitle = paste0("Coverage Area: ", round(max(candidates$coverage_pct), 2), "%\n",
                               "Params: Antenna = ", antenna_h, "m | Robot = ", robot_h, "m | Crop = ", corn_h, "m"))
      
      # Save Map silently (No print command)
      map_filename <- paste0(field_name, "_coverage_map.png")
      ggsave(file.path(field_folder, map_filename), plot_out, width = 8, height = 8, bg = "white")
      
      # 2. GeoPackage
      gpkg_path <- file.path(field_folder, paste0(field_name, "_Robot_Analysis.gpkg"))
      st_write(field_utm, gpkg_path, layer = "Boundary", delete_dsn = TRUE, quiet = TRUE)
      st_write(sig_zones, gpkg_path, layer = "Signal_Zones", append = TRUE, quiet = TRUE)
      st_write(best_zone, gpkg_path, layer = "Top_5_Percent_Spots", append = TRUE, quiet = TRUE)
      st_write(best_spot, gpkg_path, layer = "Antenna_Location", append = TRUE, quiet = TRUE)
      
      message(paste("Success! Saved map to:", map_filename))
      
    }, error = function(e) { message(paste("Error in", field_name, ":", e$message)) })
  }
}
run_antenna_optimization()
