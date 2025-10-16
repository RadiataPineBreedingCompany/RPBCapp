#' @include Layout.R
#' @importFrom R6 R6Class
PlantationModel <- R6Class("PlantationModel",
public = list(
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
  crs = sf::NA_crs_,      # sf::crs
  params = list(),

  set_crs = function(x)
  {
    if (is.null(x)) return()
    if (is.na(x)) return()

    self$crs = sf::st_crs(x)

    if (!is.null(self$boundaries)) self$boundaries = sf::st_transform(self$boundaries, self$crs)
    if (!is.null(self$layout))self$layout$set_crs(self$crs)
    if (!is.null(self$chm)) terra::crs(self$chm) = self$crs$wkt
    if (!is.null(self$schm)) terra::crs(self$schm) = self$crs$wkt
    if (!is.null(self$dtm)) terra::crs(self$dtm) = self$crs$wkt
    if (!is.null(self$las)) sf::st_crs(self$las) = self$crs
    if (!is.null(self$bbox)) sf::st_crs(self$bbox) = self$crs
    if (!is.null(self$trees)) sf::st_crs(self$trees) = self$crs
    if (!is.null(self$crowns)) sf::st_crs(self$crowns) = self$crs
  },

  set_cloud = function(file)
  {
    cat("Set cloud:", file, "\n")

    self$las = NULL
    gc()

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
      self$set_crs(crs)
    else
      self$set_crs(self$crs)
  },

  # Read the boundaries from file
  set_boundaries = function(file)
  {
    cat("Set Boundaries:", file, "\n")

    # If the input file is an excel file, try to read the boundaries from
    # one of the sheet
    if (tools::file_ext(file) %in% c("xls", "xlsx"))
    {
      # Find a sheet with a valid sheet name
      sheet_name <- xls_find_sheet(file, BOUNDARYSHEETNAMES, mustWork = FALSE)

      # The sheet is missing. We do not have boundaries
      if (is.null(sheet_name))
        return(NULL)

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

    self$boundaries = boundaries
    self$clip()
  },

  # Read the DTM from file
  set_dtm = function(file)
  {
    cat("Set DTM:", file, "\n")

    self$dtm = terra::rast(file)
    self$clip()

    if (is.null(self$crs) || self$crs == sf::NA_crs_ )
      self$set_crs(sf::st_crs(self$chm))
  },

  has_dtm = function()
  {
    return(!is.null(self$dtm))
  },

  # Read the CHM from file
  set_chm = function(file)
  {
    cat("Set CHM:", file, "\n")

    self$chm = terra::rast(file)
    self$clip()

    if (is.null(self$crs) || self$crs == sf::NA_crs_ )
      self$set_crs(sf::st_crs(self$chm))
  },

  # Read the smooth CHM from file
  set_schm = function(file)
  {
    cat("Set sCHM:", file, "\n")

    self$schm = terra::rast(file)
    self$clip()

    if (is.null(self$crs) || self$crs == sf::NA_crs_ )
      self$set_crs(sf::st_crs(self$chm))
  },

  # Read an Excel database
  set_database = function(file)
  {
    cat("Set database:", file, "\n")

    # Load the tree data base. A table with all the trees. It MUST exist
    sheet_name <- xls_find_sheet(file, TREESHEETNAMES, mustWork = TRUE)
    database = readxl::read_excel(file, sheet = sheet_name)

    # Fix the standard
    names(database)[names(database) == "Pset(Block)"] <- BLOCKNAME

    # Assert standard validity
    df_find_column(database, BLOCKNAME, mustWork = TRUE)
    df_find_column(database, TPOSNAME,  mustWork = TRUE)

    self$database = database
  },

  set_layout = function(file)
  {
    cat("Set layout", file, "\n")

    self$layout = RPBCLayout$new()
    self$layout$read_layout(file)

    if (!self$has_crs())
      self$set_crs(sf::st_crs(self$layout$tree_layout_oriented))
  },

  set_layout_parameter = function(block_size = 18.6, num_trees = 6, start = "bl", orientation = "v")
  {
    cat("Set layout parameter\n")

    is_geodatabase = self$layout$from_geodatabase

    required(self$layout, "No 'layout' object yet. Read and Excel file first")
    stopifnot(!anyNA(block_size), !is.na(num_trees), !is.na(start[1]), !is.na(orientation))

    if (!is_geodatabase)
    {
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
    }

    # Combine the layout with the database that contains among other
    # the Clone code and Family code of each tree.
    self$layout$tree_layout_raw = self$joint_database(self$layout$tree_layout_raw)

    # Maybe the tree layout contains a column Long Lat because the database has a long lat.
    # In this case this means that some trees have a position recorded.
    tlr = self$layout$tree_layout_raw

    if (!is_geodatabase)
    {
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
    }

    # invalidate previous results
    self$crowns = NULL
    self$trees = NULL
    self$layout$tree_layout_adjusted = NULL
  },

  set_measurements = function(file)
  {
    self$trees = sf::st_read(file, layer = "trees", quiet = TRUE)
    self$crowns = sf::st_read(file, layer = "crowns", quiet = TRUE)
    self$layout$tree_layout_adjusted = self$trees
  },

  read_cloud = function(file, fraction = 1)
  {
    if (fraction < 0) stop("Fraction cannot be negative")
    filter = ""
    if (fraction  < 1)
      filter = paste("-keep_random_fraction", fraction)

    self$las = lidR::readLAS(file, select = "xyzc", filter = filter)

    if (sf::st_crs(self$las) == sf::NA_crs_)
      self$set_crs(self$crs)

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
  },

  compute_terrain = function(res = 0.1)
  {
    assert_point_cloud_loaded(self$las)

    gnd = lidR::filter_ground(self$las)
    if (lidR::npoints(gnd) == 0)
      stop("No ground points. Process aborted")

    gnd = lidR::decimate_points(gnd, lidR::lowest(0.1))
    self$dtm = lidR::rasterize_terrain(gnd, res, lidR::tin())
  },

  compute_chm = function(res = 0.1)
  {
    required(self$dtm, "DTM is missing. Cannot compute the CHM")

    assert_point_cloud_loaded(self$las)

    chm = lidR::rasterize_canopy(self$las, res)

    dtm = self$dtm
    dtm <- terra::resample(dtm, chm, method = "bilinear")

    chm[is.na(chm)] = dtm[is.na(chm)]
    chm = chm - dtm

    self$chm = lidR::pitfill_stonge2008(chm)
    self$params$resCHM = res
  },

  joint_database = function(x)
  {
    required(x, "Internal error in joint_database(). 'x' is missing. Please report at info@r-lidar.com")
    required(self$database, "Internal error in joint_database(). 'database' is missing. Please report at info@r-lidar.com")

    db = self$database

    if (!BLOCKNAME  %in% names(db)) stop(paste0("Column ", BLOCKNAME, " is missing in the database"))
    if (!TPOSNAME   %in% names(db)) stop(paste0("Column ", TPOSNAME, " is missing in the database"))

    tmp = merge(x, db, by = c(BLOCKNAME, TPOSNAME), all.x = TRUE)

    # Reorder: first all original `trees` columns, then the new ones from `db`
    tree_cols <- names(x)
    db_cols <- setdiff(names(tmp), tree_cols)
    tmp <- tmp[c(db_cols, tree_cols)]
    tmp <- sf::st_as_sf(tmp)

    tmp = tmp[order(tmp[[BLOCKNAME]], tmp[[TPOSNAME]]), ]

    return(tmp)
  },

  smooth_chm = function(smooth = 2, passes = 2)
  {
    required(self$chm, "No CHM yet. Impossible to smooth the CHM")

    schm = self$chm
    w = as.integer(smooth/terra::res(schm)[1])
    if (w %% 2 == 0)  w <- w + 1
    if (w <= 1) stop("The 'smooth' parameter is too small compared to the resolution of the CHM")

    for (i in 1:passes)
      schm = terra::focal(schm, w, "mean", na.rm = TRUE)

    names(schm) = names(self$chm)

    self$schm = schm
    self$params$smoothCHM = smooth
    self$params$smoothPasses = passes
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
        cat("Clipping point cloud\n")
        self$las  = lidR::clip_roi(self$las, sf::st_as_sf(bound))
      }
    }
  },

  is_adjusted = function()
  {
    return(!is.null(self$layout$tree_layout_adjusted))
  },

  reset = function()
  {
    self$las = NULL             # LAS
    self$bbox = NULL            # sf POLYGON
    self$boundaries = NULL      # sf POLYGON
    self$dtm = NULL             # SpatRaster
    self$chm = NULL             # SpatRaster
    self$schm = NULL            # SpatRaster
    self$layout = NULL          # R6 RPBClayout
    self$layout_warnings = NULL # list with 2 sf POLYGON
    self$trees = NULL           # sf POINT
    self$crowns = NULL          # sf POLYGON
    self$database = NULL        # dataframe
    self$crs = NULL             # sf::crs
    self$params = list()
  },

  has_crs = function()
  {
    if (is.null(self$crs)) return(FALSE)
    if (is.na(self$crs))  return(FALSE)
    return(TRUE)
  },
  has_cloud = function() { return((is.null(self$las))) },
  has_boundaries = function() { return(!is.null(self$boundaries)) },
  has_chm = function() { return(!is.null(self$chm)) },
  has_schm = function() { return(!is.null(self$schm)) },
  has_database = function() { return(!is.null(self$database)) },
  has_layout = function() { return(!is.null(self$layout)) },
  has_trees = function() { return(!is.null(self$trees)) },
  has_crowns = function() { return(!is.null(self$crowns)) }
))

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

