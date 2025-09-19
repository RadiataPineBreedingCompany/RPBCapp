#' @export
#' @include Layout.R
Plantation <- R6::R6Class("Plantation",
  public = list(

    # Files
    wd = NULL,
    fconfig = NULL,
    flayout = NULL,
    flas = NULL,
    fdatabase = NULL,
    fboundaries = NULL,
    fdebug = NULL,
    fplan = NULL,
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
      fconfig = tempfile(fileext = ".rpbc")
      self$create_config(fconfig)
    },

    set_crs = function(x, nowrite = FALSE)
    {
      if (x == sf::NA_crs_) return()

      self$crs = sf::st_crs(x)

      if (!is.null(self$boundaries))
        self$boundaries = sf::st_transform(self$boundaries, self$crs)

      if (!is.null(self$layout))
        self$layout$set_crs(self$crs)

      if (!is.null(self$chm))
      {
        terra::crs(self$chm) = self$crs$wkt
        self$chm = terra::toMemory(self$chm)
        terra::writeRaster(self$chm, self$fchm, overwrite = TRUE)
        self$chm = terra::rast(self$fchm)
      }

      if (!is.null(self$schm))
      {
        terra::crs(self$schm) = self$crs$wkt
        self$schm = terra::toMemory(self$schm)
        terra::writeRaster(self$schm, self$fschm, overwrite = TRUE)
        self$schm = terra::rast(self$fschm)
      }

      if (!is.null(self$dtm))
      {
        terra::crs(self$dtm) = self$crs$wkt
        self$dtm = terra::toMemory(self$dtm)
        terra::writeRaster(self$dtm, self$fdtm, overwrite = TRUE)
        self$dtm = terra::rast(self$fdtm)
      }

      if (!is.null(self$las))
        sf::st_crs(self$las) = self$crs

      if (!is.null(self$bbox))
        sf::st_crs(self$bbox) = self$crs

      if (!is.null(self$trees))
        sf::st_crs(self$trees) = self$crs

      if (!is.null(self$crowns))
        sf::st_crs(self$crowns) = self$crs

      if (!nowrite)
        self$write_config()
    },

    set_cloud = function(file, nowrite = FALSE)
    {
      assert_file_exists(file)

      self$flas = file
      self$las = NULL

      # Compute bounding box
      header <- lidR::readLASheader(file)
      PHB  <- header@PHB
      crs  <- lidR::st_crs(header)
      xmin <- PHB[["Min X"]]
      xmax <- PHB[["Max X"]]
      ymin <- PHB[["Min Y"]]
      ymax <- PHB[["Max Y"]]
      mtx  <- matrix(c(xmin, xmax, ymin, ymax)[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
      geom <- sf::st_polygon(list(mtx))
      geom <- sf::st_sfc(geom)
      sf::st_crs(geom) <- crs
      self$bbox <- geom

      if (!is.na(crs))
        self$set_crs(crs, nowrite)
      else
        self$set_crs(self$crs, nowrite)

      if (!nowrite)
        self$write_config()
    },

    # Read the boundaries from file
    set_boundaries = function(file, nowrite = FALSE)
    {
      cat("Set Boundaries:", file, "\n")

      assert_file_exists(file)

      # If the input file is an excel file, try to read the boundaries from
      # one of the sheet
      if (tools::file_ext(file) %in% c("xls", "xlsx"))
      {
        # Find a sheet with a valid sheet name
        sheet_name <- xls_find_sheet(file, BOUNDARYSHEETNAMES, mustWork = FALSE)

        # The sheet is missing. We do not have boundaries
        if (is.null(sheet_name))
        {
          return(NULL)
        }

        # Read the content of the sheet
        boundaries = readxl::read_excel(file, sheet = sheet_name)

        # Case when the sheet contains a screenshoot of coordinates...
        if (nrow(boundaries) == 0)
        {
          warning(paste0("'", sheet_name, "' Excel sheet found but contains no data. No boundaries computed."))
          return(NULL)
        }

        message("Detection of an Excel sheet with the boundaries")

        # Now we can try to read the long lat coordinates. Failure is allowed. Then no boundaries
        lon = df_find_column(boundaries, LONGITUDECOLNAMES, mustWork = FALSE)
        lat = df_find_column(boundaries, LATITUDECOLNAMES, mustWork = FALSE)

        east = df_find_column(boundaries, EASTINGCOLNAMES, mustWork = FALSE)
        north = df_find_column(boundaries, NORTHINGCOLNAMES, mustWork = FALSE)

        ans = validate_coordinates(lon, lat, east, north)

        if (ans == "invalid")
        {
          if (!is.null(lon) & is.null(lat))
            warning(paste0("'", sheet_name, "' Excel sheet found with longitude but no latitude. No boundaries computed."))
          else if (is.null(lon) & !is.null(lat))
            warning(paste0("'", sheet_name, "' Excel sheet found with latitude but no longitude. No boundaries computed."))
          else if (!is.null(east) & is.null(north))
            warning(paste0("'", sheet_name, "' Excel sheet found with easting but no northing No boundaries computed."))
          else if (is.null(east) & !is.null(north))
            warning(paste0("'", sheet_name, "' Excel sheet found with northing but no easting. No boundaries computed."))
          else
            warning(paste0("'", sheet_name, "' Excel sheet found but no valid data. No boundaries computed."))
          return(NULL)
        }

        if (ans == "projected")
        {
          x = boundaries[[east]]
          y = boundaries[[north]]
          crs = self$crs
        }

        if (ans == "lonlat")
        {
          x = boundaries[[lon]]
          y = boundaries[[lat]]
          crs = 4326
        }

        if (!is.numeric(x) | !is.numeric(y))
        {
          if (ans == "lonlat")
            msg = paste0("Exacel sheet '", sheet_name, "' found with non numeric longitude and latitude. No boundaries computed.")
          else
            msg = paste0("Exacel sheet '", sheet_name, "' found with non numeric easting and northing. No boundaries computed.")

          warning(msg)
          return(NULL)
        }

        boundaries = cbind(x, y)
        boundaries = rbind(boundaries, boundaries[1,])
        boundaries = sf::st_polygon(list(boundaries))
        boundaries = sf::st_sfc(boundaries)
        sf::st_crs(boundaries) = crs
        #boundaries = sf::st_make_valid(boundaries)
        valid = sf::st_is_valid(boundaries)

        if (!valid)
        {
          warning(paste0("The polygon read from the Excel database in sheet '", sheet_name, "' is not valid. No boundaries computed."))
          return(NULL)
        }

        # Yay!! we have a boundary!
      }
      else
      {
        # It is shapefile or geopackage
        boundaries <- sf::st_read(file, quiet = TRUE)
      }

      assert_sf_polygon(boundaries)

      if (!is.null(self$crs))
      {
        if (sf::st_crs(boundaries) != self$crs)
        {
          boundaries = sf::st_transform(boundaries, self$crs)
        }
      }

      if (!is.null(self$bbox))
      {
        b = sf::st_intersects(boundaries, self$bbox, sparse = FALSE) |> any()

        if (!b)
        {
          if (ans == "projected")
            warning(paste0("The bounding box of the point cloud and the boundaries read from the Excel database in sheet '", sheet_name, "' are not intersecting. Maybe the coordinates are not in the same CRS as the point cloud. No boundaries computed."))
          else
            warning(paste0("The bounding box of the point cloud and the boundaries read from the Excel database in sheet '", sheet_name, "' are not intersecting.  No boundaries computed."))

          return(NULL)
        }
      }

      self$fboundaries = file
      self$boundaries = boundaries
      self$clip()

      if (!nowrite)
        self$write_config()
    },

    # Read the CHM from file
    set_chm = function(file, nowrite = FALSE)
    {
      cat("Set CHM:", file, "\n")

      assert_file_exists(file)

      self$fchm = file
      self$chm = terra::rast(file)
      self$clip()

      if (is.null(self$crs) || self$crs == sf::NA_crs_ )
        self$set_crs(sf::st_crs(self$chm), nowrite)

      if (!nowrite)
        self$write_config()
    },

    # Read the DTM from file
    set_dtm = function(file, nowrite = FALSE)
    {
      cat("Set DTM:", file, "\n")

      assert_file_exists(file)

      self$fdtm = file
      self$dtm = terra::rast(file)
      self$clip()

      if (is.null(self$crs) || self$crs == sf::NA_crs_ )
        self$set_crs(sf::st_crs(self$chm), nowrite)

      if (!nowrite)
        self$write_config()
    },

    # Read the smooth CHM from file
    set_schm = function(file, nowrite = FALSE)
    {
      cat("Set sCHM:", file, "\n")

      assert_file_exists(file)

      self$fschm = file
      self$schm = terra::rast(file)
      self$clip()

      if (is.null(self$crs) || self$crs == sf::NA_crs_ )
        self$set_crs(sf::st_crs(self$chm), nowrite)

      if (!nowrite)
        self$write_config()
    },

    # Read an Excel database
    set_database = function(file, nowrite = FALSE)
    {
      cat("Set database:", file, "\n")

      assert_file_exists(file)

      # Load the tree data base. A table with all the trees. It MUST exist
      sheet_name <- xls_find_sheet(file, TREESHEETNAMES, mustWork = TRUE)
      database = readxl::read_excel(file, sheet = sheet_name)

      # Fix the standard
      names(database)[names(database) == "Pset(Block)"] <- BLOCKNAME

      # Assert standard validity
      df_find_column(database, BLOCKNAME, mustWork = TRUE)
      df_find_column(database, TPOSNAME,  mustWork = TRUE)

      self$database = database
      self$fdatabase = file

      # Load the block layout to build the tree map
      self$set_layout(file, nowrite = nowrite)

      # Load the boundaries of the plantation if it exists
      sheet_name <- xls_find_sheet(file, BOUNDARYSHEETNAMES, mustWork = FALSE)
      if (!is.null(sheet_name))
      {
        self$set_boundaries(file, nowrite = nowrite)
      }

      if (!nowrite)
        self$write_config()
    },

    set_layout = function(file, nowrite = FALSE)
    {
      cat("Set layout", file, "\n")

      assert_file_exists(file)

      self$layout = RPBCLayout$new()
      self$layout$read_layout(file)
      self$flayout = file

      if (self$crs == sf::NA_crs_)
        self$set_crs(sf::st_crs(self$layout$tree_layout_oriented), nowrite)

      if (!nowrite)
        self$write_config()
    },

    set_layout_parameter = function(block_size, num_trees, start, orientation, nowrite = FALSE)
    {
      cat("Set layout parameter\n")

      if (is.null(self$layout))
        stop("No 'layout' object yet. Read and Excel file first")

      stopifnot(!anyNA(block_size), !is.na(num_trees), !is.na(start[1]), !is.na(orientation))

      # Construction of the layout
      self$layout$build_layout(block_size, num_trees, start = start, orientation = orientation)
      self$layout$set_crs(self$crs)

      # The layout is originally at (0,0). Find an arbitrary origin such as it is visible on the map.
      if (self$layout$M[1,3] == 0 & self$layout$M[2,3] == 0)
      {
        origin = c(0,0)
        if (!is.null(self$boundaries)) {
          bb = sf::st_bbox(self$boundaries)
          origin = c(bb[1], bb[2])
        } else if (!is.null(self$chm)) {
          bb = sf::st_bbox(self$chm)
          origin = c(bb[1], bb[2])
        } else if (!is.null(self$dtm)) {
          bb = sf::st_bbox(self$dtm)
          origin = c(bb[1], bb[2])
        } else if (!is.null(self$bbox)) {
          bb = sf::st_bbox(self$bbox)
          origin = c(bb[1], bb[2])
        }

        self$layout$set_origin(origin[1], origin[2])
      }

      # Combine the layout with the database that contains among other
      # the Clone code and Family code of each tree.
      self$layout$tree_layout_raw = self$joint_database(self$layout$tree_layout_raw)

      # Maybe the tree layout contains a column Long Lat because the database has a long lat.
      # In this case this means that some trees have a position recorded.
      tlr = self$layout$tree_layout_raw
      longname = df_find_column(tlr, LONGITUDECOLNAMES, mustWork = FALSE)
      latname = df_find_column(tlr, LATITUDECOLNAMES, mustWork = FALSE)
      has_position = !is.null(longname) & !is.null(latname)

      # Yes we have a position for some trees. It means we can align the layout in local space
      # centered on 0,0 into the real world coordinate system
      if (has_position)
      {
        message("Detection of long lat in the database")

        # Create a sf with long lat
        long = tlr[[longname]]
        lat = tlr[[latname]]
        long = as.numeric(na.omit(long))
        lat = as.numeric(na.omit(lat))
        longlat = data.frame(long, lat)
        longlat = sf::st_as_sf(longlat, coords = c("long", "lat"), crs = 4326)

        # Transform in current CRS
        global = sf::st_transform(longlat, self$crs)
        global = sf::st_coordinates(global)[,1:2]

        # Find the local coordinated of the corresponding trees
        idx = which(!is.na(tlr[[longname]]))
        local = sf::st_coordinates(tlr[idx,])[,1:2]

        # Align with SVD decomposition
        M = layout_alignment_svd(local, global)
        self$layout$set_matrix(M)
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

      self$layout$tree_layout_adjusted = self$trees

      if (!nowrite)
        self$write_config()
    },

    process_pointcloud = function(read_fraction = 1, rigidness = 2, cloth_resolution = 0.5, smoothCHM = 2, smoothPasses = 2, res = 0.1, progress = NULL)
    {
      prog <- make_progress(progress, 7)
      on.exit(prog$finalize(), add = TRUE)

      prog$tick(1, detail = "Reading points cloud...")
      self$read_cloud(read_fraction)

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

    optim_layout = function(progress = NULL)
    {
      if (is.null(self$layout)) stop("Missing: layout")
      if (is.null(self$layout$block_layout_oriented)) stop("Missing: block layout")
      if (is.null(self$layout$tree_layout_oriented))  stop("Missing: tree layout")
      if (is.null(self$schm)) stop("Missing: smooth CHM")
      if (is.null(self$layout$spacing)) stop("Missing: tree layouts spacing")
      if (is.null(self$boundaries)) stop("Missing: plantation boundaries")

      layout = self$layout$tree_layout_oriented
      blocks = self$layout$block_layout_oriented
      boundaries = self$boundaries
      chm = self$schm
      ws = self$layout$spacing

      new_layout = layout_optimize_by_block(layout, blocks, chm, ws, boundaries, progress = progress)

      self$layout$tree_layout_oriented = new_layout
    },

    align_layout = function(progress = NULL)
    {
      if (is.null(self$layout)) stop("Missing: layout")
      if (is.null(self$layout$M)) stop("Missing: affine matrix")
      if (is.null(self$layout$block_layout_raw)) stop("Missing: block layout")
      if (is.null(self$layout$tree_layout_raw))  stop("Missing: tree layout")
      if (is.null(self$schm)) stop("Missing: smooth CHM")
      if (is.null(self$layout$spacing)) stop("Missing: tree layouts spacing")
      if (is.null(self$boundaries)) stop("Missing: plantation boundaries")


      layout = self$layout$tree_layout_raw
      chm = self$schm
      ws = self$layout$spacing*0.75
      boundaries = self$boundaries
      pivot = as.numeric(self$layout$M[1:2,3])

      M = layout_alignment_lm(layout, chm, pivot, ws, boundaries, progress)
      self$layout$set_matrix(M)

      self$trees  <- NULL
      self$crowns <- NULL
      self$layout_warnings <- NULL

      self$write_config()
    },

    adjust_layout = function(hmin = 2, progress = NULL)
    {
      if (is.null(self$chm)) stop("No CHM available")
      if (is.null(self$schm)) stop("No smoothed CHM available")
      if (is.null(self$layout$tree_layout_oriented)) stop("No tree layout available")
      if (is.null(self$layout$spacing)) stop("No spacing available in tree layout")
      if (is.null(hmin)) stop("No 'hmin' argument")
      if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0) stop("'hmin' must be a single positive numeric value")

      chm = self$chm
      echm = self$schm
      plan = self$layout$tree_layout_oriented
      spacing = self$layout$spacing
      echm[is.na(echm)] = 0

      # The layout is hypothetical. Tree might be sightly off the theoretical pattern
      self$layout$tree_layout_adjusted = relocate_trees(chm, echm, plan, spacing, hmin, progress)

      self$layout_warnings = validate_tree(self$layout$tree_layout_adjusted, plan, spacing, hmin)

      if (is.null(self$fdebug))
        self$fdebug = paste0(self$wd,  "/output/debug.gpkg")

      if (file.exists(self$fdebug))
        file.remove(self$fdebug)

      sf::st_write(self$layout_warnings$move, dsn = self$fdebug, layer = "moves", quiet = TRUE, append = FALSE)
      if (!is.null(self$layout_warnings$warn))
        sf::st_write(self$layout_warnings$warn, dsn = self$fdebug, layer = "warnings", quiet = TRUE, append = FALSE)

      if (is.null(self$fplan))
        self$fplan = paste0(self$wd,  "/output/plan_layout.gpkg")

      if (file.exists(self$fplan))
        file.remove(self$fplan)

      sf::st_write(self$layout$tree_layout_adjusted, dsn = self$fplan, layer = "trees", quiet = TRUE, append = FALSE)
      sf::st_write(self$layout$block_layout_oriented, dsn = self$fplan, layer = "block", quiet = TRUE, append = FALSE)

      self$params$treesHmin = hmin
      self$write_config()
    },

    measure_trees = function(hmin = 2, watershed = FALSE, progress = NULL)
    {
      if (is.null(self$chm)) stop("No CHM available")
      if (is.null(self$schm)) stop("No smoothed CHM available")
      if (is.null(self$layout$tree_layout_adjusted)) stop("No tree layout available")
      if (is.null(self$layout$spacing)) stop("No spacing available in tree layout")
      if (is.null(hmin)) stop("No 'hmin' argument")
      if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0) stop("'hmin' must be a single positive numeric value")

      chm = self$chm
      echm = self$schm
      echm[is.na(echm)] = 0
      trees = self$layout$tree_layout_adjusted
      spacing = self$layout$spacing

      ans <- measure_trees(trees, chm, echm, spacing, hmin, use_dalponte = !watershed, progress = progress)

      self$trees  = self$joint_database(ans$trees)
      self$crowns = self$joint_database(ans$crowns)

      if (is.null(self$fmeasurements))
        self$fmeasurements = paste0(self$wd,  "/output/tree_measurements.gpkg")

      if (file.exists(self$fmeasurements))
          file.remove(self$fmeasurements)

      sf::st_write(self$trees, dsn = self$fmeasurements, layer = "trees", quiet = TRUE, append = FALSE)
      sf::st_write(self$crowns, dsn = self$fmeasurements, layer = "crowns", quiet = TRUE, append = FALSE)

      self$params$crownsHmin = hmin
      self$params$crownsWatershed = watershed
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
        Path = file_paths,
        stringsAsFactors = FALSE
      )
    },

    get_ggstats = function()
    {
      if (is.null(self$trees))
        return(list())

      void = ggplot2::ggplot() + ggplot2::theme_void()
      out = vector("list", 6)
      out[[1]] = void
      out[[2]] = void
      out[[3]] = void
      out[[4]] = void
      out[[5]] = void
      out[[6]] = void

      trees = self$trees

      out[[1]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = Height) +
        ggplot2::geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
        ggplot2::geom_vline(
          xintercept = mean(trees$Height, na.rm = TRUE),
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        ggplot2::labs(
          title = "Height Distribution",
          x = "Height",
          y = "Count"
        ) +
        ggplot2::theme_bw()


      out[[2]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = CrownArea) +
        ggplot2::geom_histogram(binwidth = 0.5, fill = "darkgoldenrod2", color = "black") +
        ggplot2::geom_vline(
          xintercept = mean(trees$CrownArea, na.rm = TRUE),
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        ggplot2::labs(
          title = "Crown Area Distribution",
          x = "Area (m²)",
          y = "Count"
        ) +
        ggplot2::theme_bw()

      out[[3]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = .data[[BLOCKNAME]], y = Height, fill = as.factor(.data[[BLOCKNAME]])) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        ggplot2::theme_bw() +
        ggplot2::guides(fill = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::scale_x_discrete(
          breaks = levels(as.factor(trees[[BLOCKNAME]]))[seq(1, length(unique(trees[[BLOCKNAME]])), 5)]
        ) +
        ggplot2::labs(
          title = "Height Distribution by Block",
          x = "Block",
          y = "Height (m)",
          fill = "Block"
        )

      code_var <- intersect(FAMILYCODENAMES, names(trees))[1]
      if (!is.na(code_var))
      {
        out[[4]] <- ggplot2::ggplot(trees) +
          ggplot2::aes(
            x = as.factor(.data[[code_var]]),
            y = Height,
            fill = as.factor(.data[[code_var]]),
            col  = as.factor(.data[[code_var]])
          ) +
          ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
          ggplot2::theme_bw() +
          ggplot2::guides(fill = "none", col = "none") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
          ggplot2::labs(
            title = paste("Height Distribution by", code_var),
            x     = code_var,
            y     = "Height (m)",
            fill  = code_var
          )
      }

      code_var <- intersect(CLONECODENAMES, names(trees))[1]
      if (!is.na(code_var))
      {
        out[[4]] <- ggplot2::ggplot(trees) +
          ggplot2::aes(
            x = as.factor(.data[[code_var]]),
            y = Height,
            fill = as.factor(.data[[code_var]]),
            col  = as.factor(.data[[code_var]])
          ) +
          ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
          ggplot2::theme_bw() +
          ggplot2::guides(fill = "none", col = "none") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
          ggplot2::labs(
            title = paste("Height Distribution by", code_var),
            x     = code_var,
            y     = "Height (m)",
            fill  = code_var
          )
      }

      out
    },

    read_cloud = function(fraction = 1)
    {
      if (is.null(self$flas))
        stop("No file point cloud file registered. Select a file first")

      assert_file_exists(self$flas)

      if (fraction < 0) stop("Fraction cannot be negative")
      filter = ""
      if (fraction  < 1)
        filter = paste("-keep_random_fraction", fraction)

      self$las = lidR::readLAS(self$flas, select = "xyzc", filter = filter)

      if (sf::st_crs(self$las) == sf::NA_crs_)
        self$set_crs(self$crs, nowrite = TRUE)

      self$params$keepRandomFraction = fraction
    },

    classify_ground = function(rigidness = 2, cloth_resolution = 0.5)
    {
      assert_point_cloud_loaded(self$las)

      if (!any(self$las$Classification == 2L))
      {
        self$las = lidR::classify_ground(self$las, lidR::csf(class_threshold = 0.05, rigidness = rigidness, cloth_resolution = cloth_resolution), last_returns = FALSE)
      }

      self$params$rigidness = rigidness
      self$params$cloth_resolution = cloth_resolution

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
        self$fdtm = paste0(self$wd, "/output/dtm.tif")
        terra::writeRaster(dtm, self$fdtm, overwrite = TRUE)
        self$dtm = terra::rast(self$fdtm)
      }

      self$write_config()
    },

    compute_chm = function(res = 0.1)
    {
      if (is.null(self$dtm))
        stop("DTM is missing. Cannot compute the CHM")

      assert_point_cloud_loaded(self$las)

      chm = lidR::rasterize_canopy(self$las, res)

      dtm = self$dtm
      dtm <- terra::resample(dtm, chm, method = "bilinear")

      chm[is.na(chm)] = dtm[is.na(chm)]
      chm = chm - dtm
      chm = lidR::pitfill_stonge2008(chm)

      # Save on disk
      if (!is.null(self$wd))
      {
        self$fchm = paste0(self$wd, "/output/chm.tif")
        terra::writeRaster(chm, self$fchm, overwrite = TRUE)
        self$chm = terra::rast(self$fchm)
      }

      self$params$resCHM = res

      self$write_config()
    },

    joint_database = function(x)
    {
      if (!is.null(x) & !is.null(self$fdatabase))
      {
        db = self$database

        if (!BLOCKNAME  %in% names(db)) stop(paste0("Column ", BLOCKNAME, " is missing in the Excel file"))
        if (!TPOSNAME   %in% names(db)) stop(paste0("Column ", TPOSNAME, " is missing in the Excel file"))

        tmp = merge(x, db, by = c(BLOCKNAME, TPOSNAME), all.x = TRUE)

        # Reorder: first all original `trees` columns, then the new ones from `db`
        tree_cols <- names(x)
        db_cols   <- setdiff(names(tmp), tree_cols)
        tmp <- tmp[c(db_cols, tree_cols)]
        tmp = sf::st_as_sf(tmp)

        return(tmp)
      }
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
        self$fschm = paste0(self$wd, "/output/schm.tif")
        terra::writeRaster(schm, self$fschm, overwrite = TRUE)
        self$schm = terra::rast(self$fschm)
      }

      self$params$smoothCHM = smooth
      self$params$smoothPasses = passes
      self$write_config()
    },

    clip = function(buffer = 5)
    {
      if (!is.null(self$boundaries))
      {
        bound = self$boundaries
        bound = terra::vect(self$boundaries)
        bound = terra::buffer(bound, buffer)

        if (!is.null(self$dtm))
        {
          self$dtm  = terra::crop(self$dtm, bound)
          self$dtm = terra::mask(self$dtm, bound)
        }

        if (!is.null(self$chm))
        {
          self$chm  = terra::crop(self$chm, bound)
          self$chm = terra::mask(self$chm, bound)
        }

        if (!is.null(self$schm))
        {
          self$schm  = terra::crop(self$schm, bound)
          self$schm = terra::mask(self$schm, bound)
        }

        if (!is.null(self$las))
        {
          cat("Clipping point cloud")
          self$las  = lidR::clip_roi(self$las, sf::st_as_sf(bound))
        }
      }
    },

    is_adjusted = function()
    {
      return(!is.null(self$layout$tree_layout_adjusted))
    },

    show_all = function(...)
    {
      self$leaflet(...)
    },

    leaflet = function(proxy = NULL, mapId = "", edit = NULL,
                        dtm = TRUE, chm = TRUE, schm = TRUE, bound = TRUE,
                        bbox = TRUE, trees = TRUE, crowns = TRUE, layout = TRUE)
    {
      use_proxy <- !is.null(proxy)

      if (!use_proxy) {
        map <- make_base_map("")
      } else {
        map <- leaflet::leafletProxy(mapId, session = proxy)
      }

      overlayGroups <- character()

      if (dtm) {
        res <- add_dtm_layer(map, self$dtm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (schm) {
        res <- add_schm_layer(map, self$schm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (chm) {
        res <- add_chm_layer(map, self$chm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (bbox) {
        res <- add_bbox_layer(map, self$bbox, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (bound) {
        res <- add_boundaries_layer(map, self$boundaries, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (layout) {
        res <- add_block_layout_layer(map, self$layout, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)

        res <- add_warnings_layer(map, self$layout_warnings, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)

        res <- add_tree_layout_layer(map, self$layout, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (crowns) {
        res <- add_crowns_layer(map, self$crowns, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (trees) {
        res <- add_trees_layer(map, self$trees, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      # Add draw toolbar only for initial render
      if (!is.null(edit) && !use_proxy) {
        map <- map |> leaflet.extras::addDrawToolbar(
          targetGroup = edit,
          polylineOptions = FALSE, polygonOptions = FALSE,
          circleOptions = FALSE, rectangleOptions = FALSE,
          markerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        )
      }

      # Layers control vs. default view: only on initial render (avoid duplicate controls on proxy updates)
      if (!use_proxy) {
        if (length(overlayGroups) > 0) {
          map <- map |> leaflet::addLayersControl(
            overlayGroups = overlayGroups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
        } else {
          map <- map |> leaflet::setView(lng = 174.8, lat = -41.0, zoom = 3)
        }
      }

      if (!use_proxy) {
        map = center_on_object(map, self$bbox)
      }

      map
    },

    show_trees = function(...)
    {
      self$leaflet(dtm = FALSE, chm = TRUE, schm = TRUE, bound = TRUE,
                   bbox = FALSE, trees = TRUE, crowns = TRUE, layout = FALSE, ...)
    },

    show_chm = function(...)
    {
      self$leaflet(dtm = TRUE, chm = TRUE, schm = TRUE, bound = TRUE,
                   bbox = FALSE, trees = FALSE, crowns = FALSE, layout = FALSE, ...)
    },

    show_layout = function(...)
    {
      self$leaflet(dtm = FALSE, chm = TRUE, schm = TRUE, bound = TRUE,
                   bbox = FALSE, trees = FALSE, crowns = FALSE, layout = TRUE, ...)
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

        ndisplay = 100000

        # Ground points
        gnd = lidR::filter_ground(las)
        n = lidR::npoints(gnd)
        if (n > ndisplay)
        {
          i = sample(1:n, ndisplay)
          i = sort(i)
          gnd = gnd[i]
        }

        # Non Ground points
        ngnd = lidR::filter_poi(las, Classification != lidR::LASGROUND)
        n = lidR::npoints(ngnd)
        if (n > ndisplay)
        {
          i = sample(1:n, ndisplay)
          i = sort(i)
          ngnd = ngnd[i]
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

      self$wd <- dirname(file)
      self$fconfig <- file
      self$write_config()
      if (!dir.exists(paste0(self$wd, "/output")))
        dir.create(paste0(self$wd, "/output"))
    },

    read_config = function(file)
    {
      cat("Read config file:", file, "\n")

      assert_file_ext(file, "rpbc")

      self$wd = dirname(file)
      self$fconfig = file

      config = jsonlite::read_json(file)

      if (is.null(config$format)) stop("Invalid RPBC file format")
      if (config$format$signature != "RPBC") stop("Invalid RPBC file signature")
      if (config$format$version != "1.0") stop("Invalid RPBC file version")

      if (!is.null(config$crs))
        self$set_crs(config$crs$wkt, nowrite = TRUE)

      # First we read the point cloud
      if (!is.null(config$point_cloud))
      {
        self$set_cloud(config$point_cloud$file, nowrite = TRUE)
        self$params$keepRandomFraction = config$point_cloud$fraction
        self$params$rigidness = config$point_cloud$rigidness
        self$params$cloth_resolution = config$point_cloud$cloth_resolution
      }

      # Second we read the database
      if (!is.null(config$database))
      {
        self$fdatabase = config$database$file
        self$set_database(self$fdatabase, nowrite = TRUE)
      }

      # Maybe we have a boundary file. But maybe the boundary come from Excel file
      # Thus is has already been built by set_database
      if (!is.null(config$boundaries) & is.null(self$boundaries))
      {
        self$set_boundaries(config$boundaries$file, nowrite = TRUE)
      }

      if (!is.null(config$dtm))
      {
        self$set_dtm(config$dtm$file, nowrite = TRUE)
      }

      if (!is.null(config$chm))
      {
        self$set_chm(config$chm$file, nowrite = TRUE)
        self$params$resCHM = config$chm$res
      }

      if (!is.null(config$schm))
      {
        self$set_schm(config$schm$file, nowrite = TRUE)
        self$params$smoothCHM = config$schm$smoothCHM
        self$params$smoothPasses = config$schm$smoothPasses
      }

      # Now more complex, set up the layout
      if (!is.null(config$layout))
      {
        if (config$layout$file != config$database$file)
          self$set_layout(config$layout$file, nowrite = TRUE)

        # from_geodatabase is false. In means we read from Excel database. Otherwise
        # the layout comes from third party source. It has been loaded as is.
        if (!self$layout$from_geodatabase)
        {
          # We build the layout
          if (!is.null(config$layout$block_size))
          {
            self$set_layout_parameter(
              block_size = unlist(config$layout$block_size),
              num_trees = config$layout$num_trees,
              start = config$layout$start,
              orientation = config$layout$orientation,
              nowrite = TRUE)
          }

          if (!is.null(config$layout$matrix))
          {
            M = unlist(config$layout$matrix)
            M = matrix(M, ncol = 3, byrow = TRUE)
            self$layout$set_matrix(M)
          }
        }
      }

      if (!is.null(config$measurements))
      {
        if (!is.null(config$measurements$file))
        {
          self$set_measurements(config$measurements$file, nowrite = TRUE)
        }
        self$params$crownsHmin = config$measurements$crownsHmin
        self$params$treesHmin = config$measurements$treesHmin
      }

      if (!is.null(config$debug))
      {
        self$fdebug = config$debug$file
        self$layout_warnings = list(
          warn = tryCatch({sf::st_read(self$fdebug, layer = "warnings", quiet = TRUE)}, error = function(e) NULL),
          move = sf::st_read(self$fdebug, layer = "moves", quiet = TRUE)
        )
      }

      if (!is.null(self$crs))
        self$set_crs(self$crs)
    },

    write_config = function()
    {
      cat("Write config file\n")

      if (is.null(self$fconfig))
        stop("Impossible to save the project. No project file associated.")

      config = list()

      # Insert the RPBC file format
      config$format = list(
        signature = "RPBC",
        version = "1.0")

      # For point cloud we store
      # - the file
      # - ground classification CSF parameters
      # - CHM resolution
      # - the random fraction to read
      config$point_cloud = list(
        file = self$flas,
        fraction = self$params$keepRandomFraction,
        rigidness = self$params$rigidness,
        cloth_resolution = self$params$cloth_resolution
      )

      config$boundaries = list(
        file = self$fboundaries
      )

      config$dtm = list(
        file = self$fdtm
      )

      config$chm = list(
        file = self$fchm,
        res = self$params$resCHM
      )

      config$schm = list(
        file = self$fschm,
        smoothCHM = self$params$smoothCHM,
        smoothPasses = self$params$smoothPasses
      )

      # The layout is the theoretic position of the trees. It can be either an Excel file with block
      # layout from which we build the trees positions on-the-fly using block size, tree count and so on.
      # It can also be a geospatial file with the tree positions
      M_list = NULL
      if (!is.null(self$layout))
        M_list <- split(self$layout$M, row(self$layout$M))

      config$layout = list(
        file = self$flayout,
        block_size = self$layout$block_size,
        num_trees = self$layout$num_trees,
        start = self$layout$start,
        orientation = self$layout$orientation,
        matrix = M_list,
        origin = self$layout$origin
      )

      # Measurements file is the final file with exact tree positions and measurement for each tree
      # It is a geopackage with two layers for the trees (POINTS) and the crowns (POLYGON)
      config$measurements = list(
        file = self$fmeasurements,
        treesHmin = self$params$treesHmin,
        crownsHmin = self$params$crownsHmin,
        crownsWatershed = self$params$crownsWatershed
      )

      # This is the excel database
      config$database = list(
        file = self$fdatabase
      )

      config$debug = list(
        file = self$fdebug
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

      jsonlite::write_json(config, self$fconfig, pretty = TRUE, auto_unbox = TRUE, digits = 8)
    },

    state = function()
    {
      list(
        CHM = !is.null(self$chm),
        sCHM = !is.null(self$schm),
        Boudaries = !is.null(self$boundaries),
        Layout = !is.null(self$layout),
        Trees = !is.null(self$trees),
        Crowns = !is.null(self$crowns)
      )
    }
  )
)

validate_coordinates <- function(long, lat, east, north)
{
  # Geographic coordinates have precedence
  if (!is.null(long) || !is.null(lat)) {
    if (!is.null(long) && !is.null(lat)) {
      return("lonlat")
    } else {
      return("invalid")
    }
  }

  if (!is.null(east) || !is.null(north)) {
    if (!is.null(east) && !is.null(north)) {
      return("projected")
    } else {
      return("invalid")
    }
  }
  return("invalid")
}
