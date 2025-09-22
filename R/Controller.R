#' @export
#' @include Model.R
#' @include View.R
PlantationController <- R6::R6Class("PlantationController",
public = list(
  model = NULL,

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

  initialize = function(model)
  {
    self$model = model
    fconfig = tempfile(fileext = ".rpbc")
    self$create_config(fconfig)
  },

  set_crs = function(crs)
  {
    if (!is.na(crs))
      self$model$set_crs(crs)
  },

  get_crs = function() { self$model$crs },

  set_cloud = function(file)
  {
    assert_file_exists(file)
    self$model$set_cloud(file)
    self$flas <- file
  },

  set_boundaries = function(file)
  {
    assert_file_exists(file)
    self$model$set_boundaries(file)
    if (!is.null(self$model$boundaries))
      self$fboundaries = file
  },

  set_dtm = function(file)
  {
    assert_file_exists(file)
    self$model$set_dtm(file)
    self$fdtm <- file
  },

  set_chm = function(file)
  {
    assert_file_exists(file)
    self$model$set_chm(file)
    self$fchm <- file
  },

  set_schm = function(file)
  {
    assert_file_exists(file)
    self$model$set_schm(file)
    self$fschm <- file
  },

  set_database = function(file)
  {
    assert_file_exists(file)

    self$model$set_database(file)
    self$fdatabase <- file

    self$set_layout(file)
    sheet_name <- xls_find_sheet(file, BOUNDARYSHEETNAMES, mustWork = FALSE)
    if (!is.null(sheet_name))
    {
      self$set_boundaries(file)
    }
  },

  set_measurements = function(file)
  {
    assert_file_exists(file)
    self$model$set_measurements(file)
    self$fmeasurements = file
  },

  set_layout_parameter = function(block_size, num_trees, start, orientation)
  {
    if (is.null(self$model$crs)) stop("Cannot create a layout in a project without CRS")
    if (is.na(self$model$crs)) stop("Cannot create a layout in a project without CRS")
    required(self$model$layout, "No 'layout' object yet. Read and Excel file first")
    stopifnot(!anyNA(block_size), !is.na(num_trees), !is.na(start[1]), !is.na(orientation))
    stopifnot(is.numeric(block_size), is.numeric(num_trees), is.character(start[1]), is.character(orientation))

    self$model$set_layout_parameter(block_size, num_trees, start, orientation)
    self$model$layout$move()
  },

  set_layout = function(file)
  {
    assert_file_exists(file)
    self$model$set_layout(file)
    self$model$set_layout_parameter(
      self$model$layout$block_size,
      self$model$layout$num_trees,
      self$model$layout$start,
      self$model$layout$orientation)

    if (!is.null(self$has_layout()))
      self$flayout <- file
  },

  set_origin = function(x, y)
  {
    required(self$model$layout, "Cannot assign an origin without a layout")
    self$model$layout$set_origin(x, y)
  },

  has_layout = function() { return(!is.null(self$model$layout)) },
  has_tree_map = function() { return(!is.null(self$model$layout$tree_layout_raw)) },

  create_config = function(file)
  {
    assert_file_ext(file, "rpbc")

    self$wd <- dirname(file)
    self$fconfig <- file
    self$write_config()

    if (!dir.exists(paste0(self$wd, "/output")))
      dir.create(paste0(self$wd, "/output"))
  },

  process_pointcloud = function(read_fraction = 1, rigidness = 2, cloth_resolution = 0.5, smoothCHM = 2, smoothPasses = 2, res = 0.1, progress = NULL)
  {
    required(self$wd, "Unitialized project without working directory")
    required(self$flas, "No file point cloud file registered. Select a file first")

    prog <- make_progress(progress, 9)
    on.exit(prog$finalize(), add = TRUE)

    prog$tick(1, detail = "Reading points cloud...")
    self$model$read_cloud(self$flas, read_fraction)

    prog$tick(2,  detail = "Clipping point cloud...")
    self$model$clip()

    prog$tick(3,  detail = "Classifying ground points...")
    self$model$classify_ground(rigidness = rigidness, cloth_resolution = cloth_resolution)

    prog$tick(4,  detail = "Computing DTM...")
    self$model$compute_terrain()

    prog$tick(5,  detail = "Computing CHM...")
    self$model$compute_chm(res)

    prog$tick(6,  detail = "Computing smooth CHM...")
    self$model$smooth_chm(smoothCHM, smoothPasses)

    prog$tick(7,  detail = "Clipping...")
    self$model$clip()

    prog$tick(8,  detail = "Saving")

    self$save_dtm()
    self$save_chm()
    self$save_schm()
    self$write_config()

    prog$tick(9,  detail = "Done")
  },

  smooth_chm = function(smooth = 2, passes = 2)
  {
    if (is.null(self$model$chm))
      stop("No CHM yet. Impossible to smooth the CHM")

    stopifnot(is.numeric(smooth), is.numeric(passes))
    stopifnot(length(smooth) == 1, length(passes) == 1)
    stopifnot(smooth > 0, passes > 0)

    self$model$smooth_chm(smooth, passes)

    self$save_schm()
  },

  align_layout = function(progress = NULL)
  {
    # Assertions
    required(self$model$layout, "Missing: layout")
    required(self$model$layout$M, "Missing: affine matrix")
    required(self$model$layout$block_layout_raw, "Missing: block layout")
    required(self$model$layout$tree_layout_raw, "Missing: tree layout")
    required(self$model$schm, "Missing: smooth CHM")
    required(self$model$layout$spacing, "Missing: tree layouts spacing")
    required(self$model$boundaries, "Missing: plantation boundaries")

    # Input data
    layout = self$model$layout$tree_layout_raw
    chm = self$model$schm
    ws = self$model$layout$spacing*0.75
    boundaries = self$model$boundaries
    pivot = as.numeric(self$model$layout$M[1:2,3])

    # Alignment
    M = layout_alignment_lm(layout, chm, pivot, ws, boundaries, progress)
    self$model$layout$set_matrix(M)

    # Reset measurement
    self$model$trees  <- NULL
    self$model$crowns <- NULL
    self$model$layout_warnings <- NULL

    self$save_plan()
    self$write_config()
  },

  optim_layout = function(progress = NULL)
  {
    # Assertions
    required(self$model$layout, "Missing: layout")
    required(self$model$layout$block_layout_oriented, "Missing: block layout")
    required(self$model$layout$tree_layout_oriented, "Missing: tree layout")
    required(self$model$schm, "Missing: smooth CHM")
    required(self$model$layout$spacing, "Missing: tree layouts spacing")
    required(self$model$boundaries, "Missing: plantation boundaries")

    # Input data
    layout = self$model$layout$tree_layout_oriented
    blocks = self$model$layout$block_layout_oriented
    boundaries = self$model$boundaries
    chm = self$model$schm
    ws = self$model$layout$spacing

    # Alignment
    new_layout = layout_optimize_by_block(layout, blocks, chm, ws, boundaries, progress = progress)

    self$model$layout$tree_layout_oriented = new_layout
    self$model$layout_warnings = validate_tree(new_layout, layout, spacing)

    self$save_debug()
    self$save_plan()
    self$write_config()
  },

  adjust_layout = function(hmin = 2, progress = NULL)
  {
    # Assertions
    required(self$model$chm, "No CHM available")
    required(self$model$schm, "No smoothed CHM available")
    required(self$model$layout$tree_layout_oriented, "No tree layout available")
    required(self$model$layout$spacing, "No spacing available in tree layout")
    if (is.null(hmin)) stop("No 'hmin' argument")
    if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0) stop("'hmin' must be a single positive numeric value")

    # Input data
    chm  = self$model$chm
    echm = self$model$schm
    plan = self$model$layout$tree_layout_oriented
    spacing = self$model$layout$spacing
    echm[is.na(echm)] = 0

    # Alignment
    new_layout = relocate_trees(chm, echm, plan, spacing, hmin, progress)

    self$model$layout_warnings = validate_tree(new_layout, plan, spacing)
    self$model$layout$tree_layout_adjusted = new_layout

    self$model$params$treesHmin = hmin

    self$save_debug()
    self$save_plan()
    self$write_config()
  },

  measure_trees = function(hmin = 2, watershed = FALSE, progress = NULL)
  {
    # Assertions
    required(self$model$chm, "No CHM available")
    required(self$model$schm, "No smoothed CHM available")
    required(self$model$layout$tree_layout_adjusted, "No tree layout available")
    required(self$model$layout$spacing, "No spacing available in tree layout")
    required(hmin, "No 'hmin' argument")
    if (!is.numeric(hmin) || length(hmin) != 1 || hmin <= 0) stop("'hmin' must be a single positive numeric value")

    # Input data
    chm = self$model$chm
    echm = self$model$schm
    echm[is.na(echm)] = 0
    trees = self$model$layout$tree_layout_adjusted
    spacing = self$model$layout$spacing

    # Measurement
    ans <- measure_trees(trees, chm, echm, spacing, hmin, use_dalponte = !watershed, progress = progress)

    self$model$trees  = self$model$joint_database(ans$trees)
    self$model$crowns = self$model$joint_database(ans$crowns)

    self$model$params$crownsHmin = hmin
    self$model$params$crownsWatershed = watershed

    self$save_measurements()
    self$write_config()
  },

  export_trees = function(progress = NULL)
  {
    required(self$model$crowns, "Crowns not available")

    pol = self$model$crowns
    pol = sf::st_geometry(pol)
    typ = sf::st_geometry_type(pol) == "POLYGON"
    pol = pol[typ,]

    n = length(pol)

    prog <- make_progress(progress, n)
    on.exit(prog$finalize(), add = TRUE)

    prog$tick(1*n/3, "Reading point cloud")

    if (is.null(self$model$las))
      self$model$read_cloud(self$flas, self$model$params$keepRandomFraction)

    las = self$model$las

    prog$tick(2*n/3, "Extracting trees")

    ids = lidR:::point_in_polygons(las, pol, by_poly = TRUE)

    dir = paste0(self$wd, "/output/trees")
    if (!dir.exists(dir))
      dir.create(dir)

    files = list.files(dir, recursive = TRUE, full.names = T)
    file.remove(files)

    for (i in seq_along(ids))
    {
      prog$tick(2*n/3+i, "Writing on disk trees")
      id = ids[[i]]
      tree = las[id]
      file = paste0(dir, "/tree_", i, ".las")
      lidR::writeLAS(tree, file)
    }
  },

  save_debug = function()
  {
    if (is.null(self$fdebug))
      self$fdebug = paste0(self$wd,  "/output/debug.gpkg")

    if (file.exists(self$fdebug))
      file.remove(self$fdebug)

    sf::st_write(self$model$layout_warnings$move, dsn = self$fdebug, layer = "moves", quiet = TRUE, append = FALSE)
    if (!is.null(self$model$layout_warnings$warn))
      sf::st_write(self$model$layout_warnings$warn, dsn = self$fdebug, layer = "warnings", quiet = TRUE, append = FALSE)
  },

  save_chm = function()
  {
    self$fchm = paste0(self$wd, "/output/chm.tif")
    terra::writeRaster(self$model$chm, self$fchm, overwrite = TRUE)
    self$model$chm = terra::rast(self$fchm)
  },

  save_schm = function(schm)
  {
    self$fschm = paste0(self$wd, "/output/schm.tif")
    terra::writeRaster(self$model$schm, self$fschm, overwrite = TRUE)
    self$model$schm = terra::rast(self$fschm)
  },

  save_dtm = function()
  {
    self$fdtm = paste0(self$wd, "/output/dtm.tif")
    terra::writeRaster(self$model$dtm, self$fdtm, overwrite = TRUE)
    self$model$dtm = terra::rast(self$fdtm)
  },

  save_plan = function()
  {
    if (is.null(self$fplan))
      self$fplan = paste0(self$wd,  "/output/plan_layout.gpkg")

    if (file.exists(self$fplan))
      file.remove(self$fplan)

    if (!is.null(self$model$layout$tree_layout_adjusted))
      sf::st_write(self$model$layout$tree_layout_adjusted, dsn = self$fplan, layer = "trees", quiet = TRUE, append = FALSE)
    else
      sf::st_write(self$model$layout$tree_layout_oriented, dsn = self$fplan, layer = "trees", quiet = TRUE, append = FALSE)

    sf::st_write(self$model$layout$block_layout_oriented, dsn = self$fplan, layer = "block", quiet = TRUE, append = FALSE)
  },

  save_measurements = function()
  {
    if (is.null(self$fmeasurements))
      self$fmeasurements = paste0(self$wd,  "/output/tree_measurements.gpkg")

    if (file.exists(self$fmeasurements))
      file.remove(self$fmeasurements)

    sf::st_write(self$model$trees, dsn = self$fmeasurements, layer = "trees", quiet = TRUE, append = FALSE)
    sf::st_write(self$model$crowns, dsn = self$fmeasurements, layer = "crowns", quiet = TRUE, append = FALSE)
  },

  save = function()
  {
    self$write_config()
  },

  read_config = function(file)
  {
    assert_file_ext(file, "rpbc")

    cat("Read config file:", file, "\n")

    params = list()

    self$wd = dirname(file)
    self$fconfig = file

    config = jsonlite::read_json(file)

    if (is.null(config$format)) stop("Invalid RPBC file format")
    if (config$format$signature != "RPBC") stop("Invalid RPBC file signature")
    if (config$format$version != "1.0") stop("Invalid RPBC file version")

    if (!is.null(config$crs))
      self$model$set_crs(config$crs$wkt)

    # First we read the point cloud
    if (!is.null(config$point_cloud))
    {
      self$set_cloud(config$point_cloud$file)
      params$keepRandomFraction = config$point_cloud$fraction
      params$rigidness = config$point_cloud$rigidness
      params$cloth_resolution = config$point_cloud$cloth_resolution
    }

    # Second we read the database
    if (!is.null(config$database))
    {
      self$fdatabase = config$database$file
      self$set_database(self$fdatabase)
    }

    # Maybe we have a boundary file. But maybe the boundary come from Excel file
    # Thus is has already been built by set_database
    if (!is.null(config$boundaries) & is.null(self$boundaries))
    {
      self$set_boundaries(config$boundaries$file)
    }

    if (!is.null(config$dtm))
    {
      self$set_dtm(config$dtm$file)
    }

    if (!is.null(config$chm))
    {
      self$set_chm(config$chm$file)
      params$resCHM = config$chm$res
    }

    if (!is.null(config$schm))
    {
      self$set_schm(config$schm$file)
      params$smoothCHM = config$schm$smoothCHM
      params$smoothPasses = config$schm$smoothPasses
    }

    # Now more complex, set up the layout
    if (!is.null(config$layout))
    {
      if (config$layout$file != config$database$file)
        self$set_layout(config$layout$file)

      # from_geodatabase is false. In means we read from Excel database. Otherwise
      # the layout comes from third party source. It has been loaded as is.
      if (!self$model$layout$from_geodatabase)
      {
        # We build the layout
        if (!is.null(config$layout$block_size))
        {
          self$set_layout_parameter(
            block_size = unlist(config$layout$block_size),
            num_trees = config$layout$num_trees,
            start = config$layout$start,
            orientation = config$layout$orientation)
        }

        if (!is.null(config$layout$matrix))
        {
          M = unlist(config$layout$matrix)
          M = matrix(M, ncol = 3, byrow = TRUE)
          self$model$layout$set_matrix(M)
        }
      }
    }

    if (!is.null(config$measurements))
    {
      if (!is.null(config$measurements$file))
      {
        self$set_measurements(config$measurements$file)
      }
      params$crownsHmin = config$measurements$crownsHmin
      params$treesHmin = config$measurements$treesHmin
    }

    if (!is.null(config$debug))
    {
      self$fdebug = config$debug$file
      self$model$layout_warnings = list(
        warn = tryCatch({sf::st_read(self$fdebug, layer = "warnings", quiet = TRUE)}, error = function(e) NULL),
        move = sf::st_read(self$fdebug, layer = "moves", quiet = TRUE)
      )
    }

    self$model$params = params
  },

  get_file_table = function() {
    safe_val <- function(x) if (is.null(x)) NA else x

    # Mapping extensions → object type
    type_map <- list(
      "las"  = "LAS",
      "laz"  = "LAS",
      "tif"  = "Raster",
      "tiff" = "Raster",
      "gpkg" = "Geopackage",
      "shp"  = "Shapefile",
      "xlsx" = "Excel file",
      "xls"  = "Excel file",
      "csv"  = "CSV",
      "rpbc" = "JSON",
      "json" = "JSON",    # could also be sf/json depending on use
      "txt"  = "Table"
    )

    # Mapping self$ slot names → human-readable labels
    label_map <- list(
      fconfig      = "Configuration file",
      flas         = "Point cloud",
      fboundaries  = "Boundaries",
      fdtm         = "DTM",
      fchm         = "CHM",
      fschm        = "Smooth CHM",
      fdatabase    = "RPBC database",
      flayout      = "Layout",
      fdebug       = "Layout warning",
      fmeasurements= "Measurements"
    )

    # Collect all non-null file slots
    files <- lapply(names(label_map), function(slot) {
      path <- safe_val(self[[slot]])
      if (is.na(path)) return(NULL)

      ext <- tolower(tools::file_ext(path))
      obj_type <- type_map[[ext]] %||% "Unknown"

      list(
        Object = label_map[[slot]],
        Type   = obj_type,
        Path   = path
      )
    })

    # Bind into a data.frame
    do.call(rbind, lapply(files, as.data.frame, stringsAsFactors = FALSE))
  },

  write_config = function()
  {
    cat("Write config file\n")

    if (is.null(self$fconfig))
      stop("Impossible to save the project. No project file associated.")

    params = self$model$params

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
      fraction = params$keepRandomFraction,
      rigidness = params$rigidness,
      cloth_resolution = params$cloth_resolution
    )

    config$boundaries = list(
      file = self$fboundaries
    )

    config$dtm = list(
      file = self$fdtm
    )

    config$chm = list(
      file = self$fchm,
      res = params$resCHM
    )

    config$schm = list(
      file = self$fschm,
      smoothCHM = params$smoothCHM,
      smoothPasses = params$smoothPasses
    )

    # The layout is the theoretic position of the trees. It can be either an Excel file with block
    # layout from which we build the trees positions on-the-fly using block size, tree count and so on.
    # It can also be a geospatial file with the tree positions
    M_list = NULL
    if (!is.null(self$model$layout))
      M_list <- split(self$model$layout$M, row(self$model$layout$M))

    config$layout = list(
      file = self$flayout,
      block_size = self$model$layout$block_size,
      num_trees = self$model$layout$num_trees,
      start = self$model$layout$start,
      orientation = self$model$layout$orientation,
      matrix = M_list,
      origin = self$model$layout$origin
    )

    # Measurements file is the final file with exact tree positions and measurement for each tree
    # It is a geopackage with two layers for the trees (POINTS) and the crowns (POLYGON)
    config$measurements = list(
      file = self$fmeasurements,
      treesHmin = params$treesHmin,
      crownsHmin = params$crownsHmin,
      crownsWatershed = params$crownsWatershed
    )

    # This is the excel database
    config$database = list(
      file = self$fdatabase
    )

    config$debug = list(
      file = self$fdebug
    )

    config$crs = list(
      wkt = self$model$crs$wkt,
      epsg = self$model$crs$epsg
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

  reset = function()
  {
    # Files
    self$wd = NULL
    self$fconfig = NULL
    self$flayout = NULL
    self$flas = NULL
    self$fdatabase = NULL
    self$fboundaries = NULL
    self$fdebug = NULL
    self$fplan = NULL
    self$fdtm = NULL
    self$fchm = NULL
    self$fschm = NULL
    self$ftrees = NULL
    self$fcrowns = NULL
    self$fmeasurements = NULL
    self$model$reset()
    gc()
  }

))
