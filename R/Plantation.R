#' @export
#' @include Layout.R
Plantation <- R6::R6Class("Plantation",
  public = list(

    # Files
    wd = NULL,
    fconfig = NULL,
    flas = NULL,
    fdatabase = NULL,
    fboundaries = NULL,
    fdtm = NULL,
    fchm = NULL,
    fschm = NULL,
    ftrees = NULL,
    fcrowns = NULL,
    fmeasurements = NULL,

    params = list(),

    # Objects
    las = NULL,             # LAS
    bbox = NULL,            # sf POLYGON
    boundaries = NULL,      # sf POLYGON
    dtm = NULL,             # SpatRaster
    chm = NULL,             # SpatRaster
    schm = NULL,            # SpatRaster
    layout = NULL,          # R6 RPBClayout
    layout_warnings = NULL, # list with 2 sf POLYGON
    trees = NULL,           # sf POINT
    crowns = NULL,          # sf POLYGON
    database = NULL,        # dataframe

    crs = NULL,             # sf::crs

    initialize = function()
    {
    },

    set_crs = function(x, nowrite = FALSE)
    {
      if (is.null(self$crs))
        self$crs = sf::st_crs(x)

      if (!is.null(self$boundaries))
        self$boundaries = sf::st_transform(self$boundaries, self$crs)

      if (!is.null(self$layout))
        self$layout$set_crs(x)

      if (!nowrite)
        self$write_config()
    },

    set_cloud = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$flas = file
      self$las = NULL

      # Compute bounding box
      header = lidR::readLASheader(file)
      PHB = header@PHB
      crs  <- lidR::st_crs(header)
      xmin <- PHB[["Min X"]]
      xmax <- PHB[["Max X"]]
      ymin <- PHB[["Min Y"]]
      ymax <- PHB[["Max Y"]]
      mtx  <- matrix(c(xmin, xmax, ymin, ymax)[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
      geom <- sf::st_polygon(list(mtx))
      geom <- sf::st_sfc(geom)
      sf::st_crs(geom) <- crs
      self$bbox = geom
      self$set_crs(crs, nowrite)

      if (!nowrite)
        self$write_config()
    },

    set_boundaries = function(file, nowrite = FALSE)
    {
      print(file)
      assert_file_exists(file)

      boundaries = sf::st_read(file, quiet = TRUE)

      assert_sf_polygon(boundaries)

      if (!is.null(self$crs))
      {
        if (sf::st_crs(boundaries) != self$crs)
        {
          boundaries = sf::st_transform(boundaries, self$crs)
        }
      }

      self$fboundaries = file
      self$boundaries = boundaries
      self$clip()

      if (!nowrite)
        self$write_config()
    },

    set_chm = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$fchm = file
      self$chm = terra::rast(file)
      self$clip()

      if (!nowrite)
        self$write_config()
    },

    set_dtm = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$fdtm = file
      self$dtm = terra::rast(file)
      self$clip()

      if (!nowrite)
        self$write_config()
    },

    set_schm = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$fschm = file
      self$schm = terra::rast(file)
      self$clip()

      if (!nowrite)
        self$write_config()
    },

    set_database = function(file, nowrite = TRUE)
    {
      assert_file_exists(file)

      sheet_name = "Sorted Linear File"
      sheets <- readxl::excel_sheets(file)
      if (!sheet_name %in% sheets)
        stop(paste("Sheet", sheet_name, "does not exist in the Excel file"))

      self$database = readxl::read_excel(file, sheet = sheet_name)
      self$fdatabase = file

      self$set_layout(file, nowrite)

      if (!nowrite)
        self$write_config()
    },

    set_layout = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$layout = RPBCLayout$new()
      self$layout$read_layout(file)
      self$fdatabase = file

      if (!nowrite)
        self$write_config()
    },

    set_layout_parameter = function(block_size, num_trees, start, orientation, nowrite = FALSE)
    {
      if (is.null(self$layout))
        stop("No 'layout' object yet. Read and Excel file first")

      self$layout$build_layout(block_size, num_trees, start = start, orientation = orientation)

      self$layout$set_crs(self$crs)

      if (all(self$layout$origin == c(0,0)))
      {
        origin = c(0,0)
        if (!is.null(self$boundaries))
        {
          bb = sf::st_bbox(self$boundaries)
          origin = c(bb[1], bb[2])
        } else if (!is.null(self$chm)) {
          bb = sf::st_bbox(self$chm)
          origin = c(bb[1], bb[2])
        } else if (!is.null(self$dtm)) {
          bb = sf::st_bbox(self$dtm)
          origin = c(bb[1], bb[2])
        }

        self$layout$set_origin(origin[1], origin[2])
      }

      if (!nowrite)
        self$write_config()
    },

    set_measurements = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$trees = sf::st_read(file, layer = "trees", quiet = TRUE)
      self$crowns = sf::st_read(file, layer = "crowns", quiet = TRUE)
      self$fmeasurements = file

      if (!nowrite)
        self$write_config()
    },

    process_pointcloud = function(rigidness = 2, cloth_resolution = 0.5, smoothCHM = 2, smoothPasses = 2, res = 0.1, progress = NULL)
    {
      prog <- make_progress(progress, 7)
      on.exit(prog$finalize(), add = TRUE)

      prog$tick(1, detail = "Reading points cloud...")
      self$read_cloud()

      prog$tick(2,  detail = "Classifying ground points...")
      self$classify_ground(rigidness = rigidness, cloth_resolution = cloth_resolution)

      prog$tick(3,  detail = "Computing DTM...")
      self$compute_terrain()

      prog$tick(4,  detail = "Computing CHM...")
      self$compute_chm(res)

      prog$tick(5,  detail = "Computing smooth CHM...")
      self$smooth_chm(smoothCHM, smoothPasses)

      prog$tick(6,  detail = "Clipping...")
      self$clip()

      prog$tick(7,  detail = "Done")
    },

    adjust_layout = function(hmin = 2, progress = NULL)
    {
      if (is.null(self$chm)) stop("CHM is NULL")
      if (is.null(self$schm)) stop("Smooth CHM is NULL")
      if (is.null(self$layout$tree_layout)) stop("Tree layout is NULL")
      if (is.null(self$layout$spacing)) stop("Spacing is NULL")
      if (is.null(hmin)) stop("hmin is NULL")
      if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0)
        stop("hmin must be a single positive numeric value")

      chm = self$chm
      echm = self$schm
      plan = self$layout$tree_layout
      spacing = self$layout$spacing
      echm[is.na(echm)] = 0
      trees = relocate_trees(chm, echm, plan, spacing, hmin, progress)
      self$layout_warnings = validate_tree(trees, plan, spacing, hmin)
      self$layout$tree_layout_adjusted = trees

      self$write_config()
    },

    measure_trees = function(hmin = 2, watershed = FALSE, progress = NULL)
    {
      if (is.null(self$chm)) stop("CHM is NULL")
      if (is.null(self$schm)) stop("Smooth CHM is NULL")
      if (is.null(self$layout$tree_layout_adjusted)) stop("Tree layout is NULL")
      if (is.null(self$layout$spacing)) stop("Spacing is NULL")
      if (is.null(hmin)) stop("hmin is NULL")
      if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0)
        stop("hmin must be a single positive numeric value")

      chm = self$chm
      echm = self$schm
      echm[is.na(echm)] = 0
      trees = self$layout$tree_layout_adjusted
      spacing = self$layout$spacing

      ans <- measure_trees(trees, chm, echm, spacing, hmin, use_dalponte = !watershed, progress = progress)

      self$trees = ans$trees
      self$crowns = ans$crowns

      if (is.null(self$fmeasurements))
        self$fmeasurements = paste0(self$wd,  "/tree_measurements.gpkg")

      sf::st_write(self$trees, dsn = self$fmeasurements, layer = "trees", quiet = TRUE, append = FALSE)
      sf::st_write(self$crowns, dsn = self$fmeasurements, layer = "crowns", quiet = TRUE, append = FALSE)

      self$write_config()
    },

    get_file_table = function()
    {
      # Helper function for safe value
      safe_val <- function(x) if(is.null(x)) NA else x

      # Initialize empty lists
      object_names <- c()
      file_paths <- c()
      object_types <- c()


      if (!is.null(self$fconfig)) {
        object_names <- c(object_names, "Configuration file")
        file_paths <- c(file_paths, safe_val(self$fconfig))
        object_types <- c(object_types, "string")
      }

      if (!is.null(self$las)) {
        object_names <- c(object_names, "Point cloud")
        file_paths <- c(file_paths, safe_val(self$flas))
        object_types <- c(object_types, "LAS")
      }

      if (!is.null(self$bbox)) {
        object_names <- c(object_names, "Point cloud extent")
        file_paths <- c(file_paths, self$flas)
        object_types <- c(object_types, "sf POLYGON")
      }

      if (!is.null(self$boundaries)) {
        object_names <- c(object_names, "Boundaries")
        file_paths <- c(file_paths, safe_val(self$fboundaries))
        object_types <- c(object_types, "sf POLYGON")
      }

      if (!is.null(self$dtm)) {
        object_names <- c(object_names, "DTM")
        file_paths <- c(file_paths, safe_val(self$fdtm))
        object_types <- c(object_types, "SpatRaster")
      }

      if (!is.null(self$chm)) {
        object_names <- c(object_names, "CHM")
        file_paths <- c(file_paths, safe_val(self$fchm))
        object_types <- c(object_types, "SpatRaster")
      }

      if (!is.null(self$schm)) {
        object_names <- c(object_names, "Smooth CHM")
        file_paths <- c(file_paths, safe_val(self$fschm))
        object_types <- c(object_types, "SpatRaster")
      }

      if (!is.null(self$fdatabase)) {
        object_names <- c(object_names, "RPBC database")
        file_paths <- c(file_paths, safe_val(self$fdatabase))
        object_types <- c(object_types, "string")
      }

      if (!is.null(self$layout)) {
        object_names <- c(object_names, "Layout")
        file_paths <- c(file_paths, safe_val(self$fdatabase))
        object_types <- c(object_types, "R6")
      }

      if (!is.null(self$layout_warnings)) {
        object_names <- c(object_names, "Layout warning")
        file_paths <- c(file_paths, NA)
        object_types <- c(object_types, "list of sf POLYGON")
      }

      if (!is.null(self$trees)) {
        object_names <- c(object_names, "Trees")
        file_paths <- c(file_paths, safe_val(self$fmeasurements))
        object_types <- c(object_types, "sf POINT")
      }

      if (!is.null(self$crowns)) {
        object_names <- c(object_names, "Crowns")
        file_paths <- c(file_paths, safe_val(self$fmeasurements))
        object_types <- c(object_types, "sf POLYGON")
      }

      # Return a data.frame
      data.frame(
        Object = object_names,
        Type = object_types,
        Path = short_path(file_paths, 50),
        stringsAsFactors = FALSE
      )
    },

    read_cloud = function()
    {
      if (is.null(self$flas))
        stop("No file point cloud file registered. Select a file first")

      assert_file_exists(self$flas)

      self$las = lidR::readLAS(self$flas, select = "xyzc")
    },

    classify_ground = function(rigidness = 2, cloth_resolution = 0.5)
    {
      assert_point_cloud_loaded(self$las)

      if (!any(self$las$Classification == 2L))
      {
        self$las = lidR::classify_ground(self$las, lidR::csf(class_threshold = 0.05, rigidness = rigidness, cloth_resolution = cloth_resolution), last_returns = FALSE)
      }

      self$write_config()
    },

    compute_terrain = function(res = 0.5)
    {
      assert_point_cloud_loaded(self$las)

      gnd = lidR::filter_ground(self$las)
      if (lidR::npoints(gnd) == 0)
        stop("No ground points. Process aborted")

      gnd = lidR::decimate_points(gnd, lidR::lowest(0.5))
      dtm = lidR::rasterize_terrain(gnd, res, lidR::tin())

      # Save on disk
      if (!is.null(self$wd))
      {
        self$fdtm = paste0(self$wd, "/dtm.tif")
        terra::writeRaster(dtm, self$fdtm, overwrite = TRUE)
        self$dtm = terra::rast(self$fdtm)
      }

      self$write_config()
    },

    compute_chm = function(res = 0.1)
    {
      assert_point_cloud_loaded(self$las)

      chm = lidR::rasterize_canopy(self$las, res)
      chm = lidR::pitfill_stonge2008(chm)

      dtm = self$dtm
      dtm <- terra::resample(dtm, chm, method = "bilinear")  # or "near" if categorical
      chm = chm - dtm

      # Save on disk
      if (!is.null(self$wd))
      {
        self$fchm = paste0(self$wd, "/chm.tif")
        terra::writeRaster(chm, self$fchm, overwrite = TRUE)
        self$chm = terra::rast(self$fchm)
      }

      self$write_config()
    },

    smooth_chm = function(smooth = 2, passes = 2)
    {
      if (is.null(self$chm))
        stop("No CHM yet. Impossible to smooth the CHM")

      #schm = lidR::pitfill_stonge2008(self$chm)
      schm = self$chm
      w = as.integer(smooth/terra::res(schm)[1])
      if (w %% 2 == 0)  w <- w + 1
      if (w <= 1) stop("The 'smooth' parameter is too small compared to the resolution of the CHM")

      for (i in 1:passes)
        schm = terra::focal(schm, w, "mean", na.rm = TRUE)

      names(schm) = names(self$chm)

      # Save on disk
      if (!is.null(self$wd))
      {
        self$fschm = paste0(self$wd, "/schm.tif")
        terra::writeRaster(schm, self$fschm, overwrite = TRUE)
        self$schm = terra::rast(self$fschm)
      }

      self$write_config()
    },

    clip = function(buffer = 5)
    {
      if (!is.null(self$boundaries))
      {
        bound = self$boundaries
        bound = terra::vect(self$boundaries)
        bound = terra::buffer(bound, buffer)
        if (!is.null(self$dtm)) self$dtm  = terra::mask(self$dtm, bound)
        if (!is.null(self$chm)) self$chm  = terra::mask(self$chm, bound)
        if (!is.null(self$schm))self$schm = terra::mask(self$schm, bound)
        if (!is.null(self$las)) self$las  = lidR::clip_roi(self$las, sf::st_as_sf(bound))
      }
    },

    leaflet = function(edit = NULL, dtm = TRUE, chm = TRUE, schm = TRUE, bbox = TRUE, trees = TRUE, layout = TRUE)
    {
      map = leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          options = leaflet::providerTileOptions(minZoom = 2, maxZoom = 22)
        )

      overlayGroups = c()

      # ------ add rasters -----

      if (!is.null(self$dtm) & dtm)
      {
        dtm_prod <- terra::terrain(self$dtm, v = c("slope", "aspect"), unit = "radians")
        dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
        dtm_hillshade <- terra::stretch(dtm_hillshade, minq = 0.02, maxq = 0.98)
        dtm_val = terra::values(dtm_hillshade)
        dtm_pal <- leaflet::colorNumeric(gray.colors(10,0,1), dtm_val, na.color = "transparent")

        map = map |> leaflet::addRasterImage(dtm_hillshade, colors = dtm_pal, group = "DTM", maxBytes = 2000000)
        overlayGroups = c(overlayGroups, "DTM")
      }

      if (!is.null(self$schm) & schm)
      {
        if (terra::inMemory(self$schm))
        {
          o = tempfile(fileext = ".tif")
          terra::writeRaster(self$schm, o, overwrite = TRUE)
          self$schm = terra::rast(o)
        }

        file = terra::sources(self$schm)

        map = map |>
          leafem::addGeotiff(
            file,
            colorOptions = leafem::colorOptions(palette = lidR::height.colors(20), na.color = "transparent"),
            resolution = 128,
            layerId = 1,
            group = "sCHM")
        overlayGroups = c(overlayGroups, "sCHM")
      }

      if (!is.null(self$chm) & chm)
      {
        if (terra::inMemory(self$chm))
        {
          o = tempfile(fileext = ".tif")
          terra::writeRaster(self$chm, o, overwrite = TRUE)
          self$chm = terra::rast(o)
        }

        file = terra::sources(self$chm)

        map = map |>
          leafem::addGeotiff(
            file,
            colorOptions = leafem::colorOptions(palette = lidR::height.colors(20), na.color = "transparent"),
            resolution = 128,
            layerId = 0,
            group = "CHM")
        overlayGroups = c(overlayGroups, "CHM")
      }

      # ------ add polygons and lines -----

      if (!is.null(self$bbox) & bbox)
      {
        data = sf::st_transform(self$bbox, 4326)
        map = map |> leaflet::addPolygons(data = data, group = "Point cloud", color = "red", fill = FALSE)
        overlayGroups = c(overlayGroups, "Point cloud")
      }

      if (!is.null(self$boundaries))
      {
        data = sf::st_transform(self$boundaries, 4326)
        map = map |> leaflet::addPolygons(data = data, group = "Boundaries",  color = "green", fill = FALSE)
        overlayGroups = c(overlayGroups, "Boundaries")
      }

      if (!is.null(self$layout) & layout)
      {
        data = sf::st_transform(self$layout$block_layout, 4326)
        map = map |> leaflet::addPolygons(data = data, group = "Block layout",  color = "black", fill = FALSE, weight = 3)
        overlayGroups = c(overlayGroups, "Block layout")
      }

      if (!is.null(self$layout_warnings) & layout)
      {
        data = sf::st_transform(self$layout_warnings$move, 4326)
        map = map |> leaflet::addPolylines(data = data, group = "Move", color = "white", fill = FALSE, weight = 2, opacity = 0.9)
        overlayGroups = c(overlayGroups, "Move")

        reason = self$layout_warnings$warn$reason
        pal <- leaflet::colorFactor(palette = c('yellow', 'green'), domain = reason)

        data = sf::st_transform(self$layout$tree_layout_adjusted, 4326)
        map = map |> leaflet::addPolygons(data = sf::st_transform(self$layout_warnings$warn, 4326), opacity = 0.9, group = "Warnings", color = ~pal(reason), fill = FALSE, weight = 3)
        overlayGroups = c(overlayGroups, "Warnings")

        map = map |>
          leaflet::addLegend(
            position = "bottomleft",
            pal = pal,
            values = reason,
            title = "Warnings"
          )
      }

      if (!is.null(self$crowns) & trees)
      {
        data = sf::st_transform(self$crowns, 4326)
        data = data[!sf::st_is_empty(data),]
        map = map |> leaflet::addPolygons(data = data, color = "gray", fill = FALSE, opacity = 0.5, weight = 1, group = "Crowns")
        overlayGroups = c(overlayGroups, "Crowns")
      }

      # ------- add markers ------

      if (!is.null(self$layout) & layout)
      {
        data = sf::st_transform(self$layout$tree_layout_adjusted, 4326)

        if (!is.null(data$TreeFound)) {
          col = ifelse(data$TreeFound, "green", "red")

        map =  map |> leaflet::addLegend(
            position = "bottomright",
            colors = c("green", "red"),
            labels = c("Tree found", "Tree not found"),
            title = "Tree status")

        } else {
          col = "#03F"  # fallback if no TreeFound column
        }

        map = map |> leaflet::addCircleMarkers(data = data, color = col, group = "Tree layout", radius = 1)
        overlayGroups = c(overlayGroups, "Tree layout")
      }

      if (!is.null(self$trees) & trees)
      {
        data = sf::st_transform(self$trees, 4326)

        pal <- leaflet::colorNumeric(palette = viridis::viridis(8), domain = data$Height)

        map = map |> leaflet::addCircleMarkers(
          data = data,
          group = "Trees",
          radius = 4,
          color = ~pal(Height),
          fillOpacity = 0.9,
          stroke = FALSE,
          popup = leafpop::popupTable(data, feature.id = FALSE, row.numbers = FALSE))
        overlayGroups = c(overlayGroups, "Trees")

        map |> leaflet::addLegend(
          pal     = pal,
          values  = data$Height,
          title   = "Tree Height (m)",
          opacity = 1
        )
      }


      if (length(overlayGroups) > 0)
      {
        map = map |> leaflet::addLayersControl(
          overlayGroups = overlayGroups,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
      }
      else
      {
        map = map |> leaflet::setView(lng = 174.8, lat = -41.0, zoom = 3)
      }

      if (!is.null(edit))
      {
        map = map |> leaflet.extras::addDrawToolbar(
          targetGroup = edit,
          polylineOptions   = FALSE,
          polygonOptions    = FALSE,
          circleOptions     = FALSE,
          rectangleOptions  = FALSE,
          markerOptions     = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        )
      }

      map
    },

    show_all = function(...)
    {
      self$leaflet(...)
    },

    show_trees = function(...)
    {
      self$leaflet(dtm = FALSE, layout = FALSE, trees = TRUE, ...)
    },

    show_chm = function(...)
    {
      self$leaflet(dtm = FALSE, layout = FALSE, trees = FALSE, ...)
    },

    show_layout = function(...)
    {
      self$leaflet(dtm = FALSE, layout = TRUE, trees = FALSE, ...)
    },

    plot = function(useNULL = FALSE)
    {
      rgl::open3d(useNULL = useNULL)
      rgl::bg3d("black")

      offset = c(0,0,0)
      if (!is.null(self$las))
      {
        xmin = self$las@header[["Min X"]]
        xmax = self$las@header[["Max X"]]
        ymin = self$las@header[["Min Y"]]
        ymax = self$las@header[["Max Y"]]
        zmin = self$las@header[["Min Z"]]
        zmax = self$las@header[["Max Z"]]
        offset = c((xmin+xmax)/2, (ymin+ymax)/2, zmin)
      } else if (!is.null(self$bbox)) {
        coords <- sf::st_coordinates(self$bbox)
        offset = c(mean(coords[,1]), mean(coords[,2]),mean(coords[,3]))
      } else if (!is.null(self$boundaries)) {
        coords <- sf::st_coordinates(self$boundaries)
        offset = c(mean(coords[,1]), mean(coords[,2]),mean(coords[,3]))
      } else if (!is.null(self$chm)) {
        e = terra::ext(self$chm)
        z = min(terra::values(self$chm), na.rm = T)
        offset = c(e[1], e[3], z)
      }

      if (!is.null(self$bbox))
      {
        coords <- sf::st_coordinates(self$bbox)
        z <- rep(0, nrow(coords))
        xyz <- cbind(coords[, 1:2], z)
        xyz[,1] = xyz[,1] - offset[1]
        xyz[,2] = xyz[,2] - offset[2]
        rgl::lines3d(xyz, col = "red", lwd = 2, tag = "bbox")
      }

      if (!is.null(self$dtm))
      {
        dtm = terra::aggregate(self$dtm)
        lidR::add_dtm3d(offset[1:2], dtm-offset[3], tag = "dtm")
      }

      if (!is.null(self$boundaries))
      {
        coords <- sf::st_coordinates(self$boundaries)
        z <- rep(0, nrow(coords))
        xyz <- cbind(coords[, 1:2], z)
        xyz[,1] = xyz[,1] - offset[1]
        xyz[,2] = xyz[,2] - offset[2]
        rgl::lines3d(xyz, col = "green", lwd = 2, tag = "boundaries")
      }

      if (!is.null(self$las))
      {
        las = self$las

        gnd = lidR::filter_ground(las)
        ngnd = lidR::filter_poi(las, Classification != lidR::LASGROUND)
        n = lidR::npoints(ngnd)
        if (n > 1000000)
        {
          r = ceiling(n/1000000)
          ngnd <- ngnd[seq(1, n, by = r)]
        }
        pal = lidR::height.colors(25)
        col = lidR:::set.colors(ngnd$Z, pal)
        rgl::points3d(gnd$X-offset[1], gnd$Y-offset[2], gnd$Z-offset[3], col = "blue", size = 2, tag = "ground")
        rgl::points3d(ngnd$X-offset[1], ngnd$Y-offset[2], ngnd$Z-offset[3], col = col, size = 2, tag = "vegetation")
      }

      # if (!is.null(self$chm))
      # {
      #   r = self$chm
      #   r = terra::aggregate(r, fact = 2)
      #   mat <- terra::as.matrix(r, wide=T)
      #
      #   ext <- ext(r)
      #   x <- seq(ext[1], ext[2], length.out = ncol(mat))
      #   y <- seq(ext[3], ext[4], length.out = nrow(mat))
      #
      #   mat_flip <- t(mat[nrow(mat):1, ])
      #
      #   rgl::surface3d(x-offset[1], y-offset[2], mat_flip-offset[3], lidR::height.colors(50)[cut(mat_flip, 50)], back = "lines")
      # }
    },

    create_config = function(file)
    {
      assert_file_ext(file, "rpbc")

      self$wd = dirname(file)
      self$fconfig = file
      self$write_config()
    },

    read_config = function(file)
    {
      assert_file_ext(file, "rpbc")

      self$wd = dirname(file)
      self$fconfig = file

      config = jsonlite::read_json(file)
      if (!is.null(config$point_cloud)) self$set_cloud(config$point_cloud$file, nowrite = TRUE)
      if (!is.null(config$boundaries)) self$set_boundaries(config$boundaries$file, nowrite = TRUE)
      if (!is.null(config$dtm)) self$set_dtm(config$dtm$file, nowrite = TRUE)
      if (!is.null(config$chm)) self$set_chm(config$chm$file, nowrite = TRUE)
      if (!is.null(config$schm))
      {
        self$set_schm(config$schm$file, nowrite = TRUE)
        self$params$smoothCHM = config$schm$smoothCHM
        self$params$smoothPasses = config$schm$smoothPasses
      }
      if (!is.null(config$layout))
      {
        self$set_layout(config$layout$file, nowrite = TRUE)
        if (!is.null(config$layout$block_size))
        {
          self$set_layout_parameter(
            block_size = config$layout$block_size,
            num_trees = config$layout$num_trees,
            start = config$layout$start,
            orientation = config$layout$orientation,
            nowrite = TRUE)
        }

        if (!is.null(config$layout$origin))
          self$layout$set_origin(config$layout$origin[[1]], config$layout$origin[[2]])

        if (!is.null(config$layout$angle))
          self$layout$set_angle(config$layout$angle)
      }
      if (!is.null(config$measurements)) self$set_measurements(config$measurements$file, nowrite = TRUE)
      if (!is.null(config$database)) self$fdatabase = config$database$file
    },

    write_config = function()
    {
      if (is.null(self$fconfig))
        stop("Impossible to save the project. No project file associated.")

      config = list()
      config$point_cloud = list(file = self$flas)
      config$boundaries = list(file = self$fboundaries)
      config$dtm = list(file = self$fdtm)
      config$chm = list(file = self$fchm)
      config$schm = list(
        file = self$fschm,
        smoothCHM = self$params$smoothCHM,
        smoothPasses = self$params$smoothPasses
      )
      config$layout = list(
        file = self$fdatabase,
        block_size = self$layout$block_size,
        num_trees = self$layout$num_trees,
        start = self$layout$start,
        orientation = self$layout$orientation,
        angle = self$layout$angle,
        origin = self$layout$origin
      )
      config$measurements = list(
        file = self$fmeasurements
      )
      config$database = list(
        file = self$fdatabase
      )
      config$crs = list(
        wkt = self$crs$wkt,
        epsg = self$crs$epsg
      )

      is.NullOb <- function(x) if(!(is.function(x))) is.null(x) | all(sapply(x, is.null)) else FALSE

      rmNullObs <- function(x) {
        if(!(is.function(x))) {
          x = x[!(sapply(x, is.NullOb))]
          lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
        }
      }

      config = rmNullObs(config)

      jsonlite::write_json(config, self$fconfig, pretty = TRUE, auto_unbox = TRUE)
    },

    state = function()
    {
      list(
        chm = !is.null(self$chm),
        schm = !is.null(self$schm),
        layout = !is.null(self$layout),
        trees = !is.null(self$trees),
        crowns = !is.null(self$crowns)
      )
    }
  )
)

assert_file_exists = function(file)
{
  if (!file.exists(file))
    stop(paste0("File not found: ", file))
}

assert_sf_polygon = function(sf)
{
  if (sf::st_geometry_type(sf) != "POLYGON")
    stop(paste0("Entities are expected to be POLYGON not ", sf::st_geometry_type(sf)))
}

assert_point_cloud_loaded = function(las)
{
  if (is.null(las))
    stop("No point cloud loaded yet")
}

assert_file_ext <- function(file, expected_ext) {
  ext <- tools::file_ext(file)
  if (tolower(ext) != tolower(expected_ext)) {
    stop(sprintf(
      "Invalid file extension: '%s'. Expected '.%s'.",
      ext, expected_ext
    ), call. = FALSE)
  }
  invisible(TRUE)
}

short_path <- function(paths, max_chars = 40) {
  sapply(paths, function(p) {
    if (is.na(p)) return(NA)
    # Short path is ok as is
    if (nchar(p) <= max_chars) return(p)

    comps <- strsplit(p, .Platform$file.sep)[[1]]
    n <- length(comps)
    if (n <= 2) return(p)

    first <- paste(comps[1:(n-1)], collapse = .Platform$file.sep) # keep most of start
    last <- comps[n]  # filename
    # Truncate first if still too long
    total_len <- nchar(first) + nchar(last) + nchar("[...]") + 2  # 2 for separators
    if (total_len > max_chars) {
      # keep only first N chars from start
      keep <- max_chars - nchar(last) - nchar("[...]") - 2
      first <- substr(first, 1, keep)
    }

    paste0(first, .Platform$file.sep, "[...]", .Platform$file.sep, last)
  }, USE.NAMES = FALSE)
}
