#' @export
RPBCLayout <- R6::R6Class("Plantation",
  public = list(
    block_layout_table = NULL,

    block_layout_raw = NULL,
    block_layout_oriented = NULL,

    tree_layout_raw = NULL,
    tree_layout_oriented = NULL,
    tree_layout_adjusted = NULL,

    crs = NULL,

    M = diag(3),
    orientation = 'v',
    start = "bl",
    block_size = 18.6,
    num_trees = c(6, 6),
    spacing = 3.1,

    from_geodatabase = FALSE,

    initialize = function()
    {
    },

    # The layout is the theoretical pattern of trees. It can be computed from the Excel database
    # or can be read from a shape file. If read from the Excel database what we read is actually
    # the block layout. From the block layout we can compute the tree position with build_layout().
    # However we already have the tree position if read from a geospatial database.
    read_layout = function(file)
    {
      cat("Read layout:", file, "\n")

      ext = tools::file_ext(file)

      if (ext %in% c("xls", "xlsx"))
      {
        sheet_name = xls_find_sheet(file, BLOCKSHEETNAMES, mustWork = TRUE)
        block_layout_table = readxl::read_excel(file, sheet = sheet_name)

        # Backward compatibility
        name = df_find_column(block_layout_table, c("BlockID", BLOCKNAME))
        if (is.null(name))
        {
          msg = paste0("Reading ", basename(file), ", sheet ", sheet_name, ". Expected a column named 'BlockID' or 'Pset' but found none")
          stop(msg)
        }
        names(block_layout_table)[names(block_layout_table) == "BlockID"] == BLOCKNAME

        expected_columns = c("BlockRow", "BlockCol")
        if (!all(expected_columns %in% names(block_layout_table)))
        {
          msg = paste0("Reading ", basename(file), ", sheet ", sheet_name, ". Expected column names 'BlockRow', 'BlockCol' but found '", paste0(u, collapse = "' '"), "'.")
          stop(msg)
        }

        self$block_layout_table = block_layout_table
      }
      else if (ext %in% c("shp", "gpkg"))
      {
        blocks = NULL
        if (ext == "shp")
        {
          layout = sf::st_read(file, quiet = TRUE)
        }

        if (ext == "gpkg")
        {
          layer = sf::st_layers(file)
          names = layer$name

          if (!"trees" %in% names)
            stop("When loading a plantation layout from an external GeoPackage it must have a layer named 'trees'. Use the file 'plan_layout.gpkg' produced by the application.")

          if (!"trees" %in% names)
            stop("When loading a plantation layout from an external GeoPackage it must have a layer named 'block'. Use the file 'plan_layout.gpkg' produced by the application.")

          layout = sf::st_read(file, layer = "trees", quiet = TRUE)
          blocks = sf::st_read(file, layer = "block", quiet = TRUE)
        }
        assert_sf_point(layout)

        if (any(!c(TPOSNAME, BLOCKNAME) %in% names(layout)))
          stop(paste0("The tree layout must have attributes named '", TPOSNAME, "' '", BLOCKNAME, "'"))

        if (!is.null(blocks))
        {
          blockname = df_find_column(blocks, c("BlockID", BLOCKNAME), mustWork = FALSE)
          if (is.null(blockname))
            stop(paste0("The block layout must have an attribute named '", BLOCKNAME, "' or 'BlockID'"))
        }

        # Estimate spacing
        d = sf::st_coordinates(layout)
        d = RANN::nn2(d, k = 4)
        d = d$nn.dists[,-1]
        self$spacing = stats::median(d)

        self$tree_layout_oriented = layout[, c(TPOSNAME, BLOCKNAME)]
        self$tree_layout_raw = self$tree_layout_oriented

        if (!is.null(blocks))
        {
          self$block_layout_raw = blocks[, c(blockname)]
          self$block_layout_oriented = self$block_layout_raw
        }

        self$from_geodatabase = TRUE
      }
      else
      {
        stop(paste0("Format not supported: ", ext))
      }
    },

    build_layout = function(block_size, num_trees, start, orientation)
    {
      if (self$from_geodatabase) return()

      if (length(num_trees) == 1) num_trees = c(num_trees, num_trees)
      if (length(num_trees) > 2)  stop("Internal error: invalid input for 'num_trees'")

      cat("Build layout:", block_size, num_trees, start, orientation, "\n")

      if (is.null(self$block_layout_table))
        stop("No block layout table. Load an Excel file first")

      self$start = start
      self$orientation = orientation
      self$block_size = block_size
      self$num_trees = num_trees
      self$spacing = self$block_size/self$num_trees
      self$block_layout_raw = generate_blocks(self$block_layout_table, block_size)
      self$tree_layout_raw = generate_trees(self$block_layout_raw, block_size, num_trees, start = start, orientation = orientation)
      self$block_layout_oriented = self$block_layout_raw
      self$tree_layout_oriented = self$tree_layout_raw

      self$move()
    },

    set_crs = function(crs)
    {
      if (!is.null(crs))
      {
        if (!is.null(self$block_layout_raw)) sf::st_crs(self$block_layout_raw) = crs
        if (!is.null(self$tree_layout_raw)) sf::st_crs(self$tree_layout_raw) = crs
        if (!is.null(self$block_layout_oriented)) sf::st_crs(self$block_layout_oriented) = crs
        if (!is.null(self$tree_layout_oriented)) sf::st_crs(self$tree_layout_oriented) = crs
      }
    },

    set_origin = function(x,y)
    {
      cat("set origin", x, y, "\n")
      M = self$M
      M[1,3] = x
      M[2,3] = y
      self$set_matrix(M)
    },

    #set_angle = function(angle)
    #{
    #  self$angle = angle
    #  self$move()
    #},

    set_matrix = function(M)
    {
      self$M = M
      self$move()

    },

    move = function()
    {
      crs = sf::st_crs(self$block_layout_raw)

      self$block_layout_oriented <- st_affine(self$block_layout_raw, self$M)
      self$tree_layout_oriented  <- st_affine(self$tree_layout_raw, self$M)
      self$tree_layout_adjusted  <- NULL
      self$set_crs(crs)
    }
  )
)

rotation <- function(a)
{
  r <- a * pi / 180 # degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}


st_affine <- function(x, M)
{
  stopifnot(ncol(M) == 3, nrow(M) == 3)
  stopifnot(inherits(x, "sf") || inherits(x, "sfc"))

  geom <- sf::st_geometry(x)

  geom_new <- lapply(geom, function(g)
  {
    type <- class(g)[2]
    coords <- sf::st_coordinates(g)
    P <- cbind(coords[,1:2, drop = FALSE], 1)
    P_new <- P %*% t(M)
    coords[,1:2] <- P_new[,1:2]

    # Rebuild geometry per type
    if (type == "POINT") {
      sf::st_point(coords[1,1:2])
    } else if (type == "MULTIPOINT") {
      sf::st_multipoint(coords[,1:2])
    } else if (type == "LINESTRING") {
      sf::st_linestring(coords[,1:2])
    } else if (type == "POLYGON") {
      sf::st_polygon(list(coords[,1:2]))
    } else {
      stop(paste("Unsupported geometry type:", type))
    }
  })

  geom_new <- sf::st_sfc(geom_new, crs = sf::st_crs(x))

  if (inherits(x, "sf")) {
    sf::st_set_geometry(x, geom_new)
  } else {
    geom_new
  }
}



rotate_sf <- function(x, theta, center = c(0,0))
{
  rot <- rotation(theta)
  geom <- sf::st_geometry(x)

  # Shift to origin, rotate, shift back
  rotated <- (geom - center) * rot + center

  sf::st_set_geometry(x, rotated)
}

#' @export
generate_blocks <- function(block_layout, block_size)
{
  block_size_x = block_size[1]
  block_size_y = block_size[1]
  if (length(block_size) > 1)
    block_size_y = block_size[2]

  # Add extra buffer block
  #nrows = max(block_layout$BlockRow)
  #ncols = max(block_layout$BlockCol)
  #all_blocks = expand.grid(BlockID = -1, BlockRow = 0:(nrows+1), BlockCol = 0:(ncols+1))
  #all_blocks = rbind(block_layout, all_blocks)
  #all_blocks = data.table::as.data.table(all_blocks)
  #all_blocks = all_blocks[!duplicated(all_blocks, by = c("BlockRow", "BlockCol"))]
  all_blocks = block_layout

  # create list of polygons
  polys <- lapply(seq_len(nrow(all_blocks)), function(i) {
    row <- all_blocks[i, ]
    coords <- rbind(
      c((row$BlockCol - 1) * block_size_x, (row$BlockRow - 1) * block_size_y), # bottom-left
      c((row$BlockCol)     * block_size_x, (row$BlockRow - 1) * block_size_y), # bottom-right
      c((row$BlockCol)     * block_size_x, (row$BlockRow)     * block_size_y), # top-right
      c((row$BlockCol - 1) * block_size_x, (row$BlockRow)     * block_size_y), # top-left
      c((row$BlockCol - 1) * block_size_x, (row$BlockRow - 1) * block_size_y)  # close polygon
    )
    sf::st_polygon(list(coords))
  })

  # bind to sf
  all_blocks$geometry <- sf::st_sfc(polys)
  all_blocks = sf::st_as_sf(all_blocks)
  all_blocks
}


#' @export
generate_trees <- function(block_layout, block_size, num_trees, start, orientation)
{
  block_size_x = block_size[1]
  block_size_y = block_size[1]
  if (length(block_size) > 1)
    block_size_y = block_size[2]

  blockID <- df_find_column(block_layout, c("BlockID", BLOCKNAME))

  sf::st_agr(block_layout) = "constant"
  block_centers <- sf::st_centroid(block_layout)
  block_centers <- sf::st_coordinates(block_centers)
  block_centers <- cbind(block_centers, block_layout[[blockID]])

  tree_layout <- lapply(1:nrow(block_centers), function(i)
  {
    block <- generate_snake_coords(
      num_trees[1],
      num_trees[2],
      block_centers[i,1],
      block_centers[i,2],
      block_size_x,
      block_size_y,
      start = start,
      orientation = orientation
    )
    block <- sf::st_as_sf(block, coords = c("x", "y"))
    names(block)[1] <- TPOSNAME
    block[[BLOCKNAME]] <- block_centers[i,3]
    block
  })

  tree_layout <- do.call(rbind, tree_layout)
  return(tree_layout)
}

