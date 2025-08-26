#' @export
RPBCLayout <- R6::R6Class("Plantation",
  public = list(
    block_layout_table = NULL,
    block_layout_raw = NULL,
    tree_layout_raw = NULL,
    block_layout = NULL,
    tree_layout = NULL,
    tree_layout_adjusted = NULL,
    crs = NULL,
    origin = c(0,0),
    angle = 0,
    orientation = 'v',
    start = "bl",
    block_size = NA,
    num_trees = NA,
    spacing = NA,

    initialize = function()
    {
    },

    read_layout = function(file, sheet_name = "Linear Layout Map")
    {
      sheets <- readxl::excel_sheets(file)
      if (!sheet_name %in% sheets)
        stop(paste("Sheet", sheet_name, "does not exist in the Excel file"))

      self$block_layout_table = readxl::read_excel(file, sheet = sheet_name)
    },

    build_layout = function(block_size, num_trees, start, orientation)
    {
      if (is.null(self$block_layout_table))
        stop("No block layout table. Load an Excel file first")

      self$start = start
      self$orientation = orientation
      self$block_size = block_size
      self$num_trees = num_trees
      self$spacing = self$block_size/self$num_trees
      self$block_layout_raw = generate_blocks(self$block_layout_table, block_size)
      self$tree_layout_raw = generate_trees(self$block_layout_raw , block_size, num_trees, start = start, orientation = orientation)
      self$block_layout = self$block_layout_raw
      self$tree_layout = self$tree_layout_raw
      self$tree_layout_adjusted = self$tree_layout
    },

    set_crs = function(crs)
    {
      if (!is.null(crs))
      {
        sf::st_crs(self$block_layout_raw) = crs
        sf::st_crs(self$tree_layout_raw) = crs
        sf::st_crs(self$block_layout) = crs
        sf::st_crs(self$tree_layout) = crs
        sf::st_crs(self$tree_layout_adjusted) = crs
      }
    },

    set_origin = function(x,y)
    {
      self$origin = c(x,y)
      self$move()
    },

    set_angle = function(angle)
    {
      self$angle = angle
      self$move()
    },

    move = function()
    {
      crs = sf::st_crs(self$block_layout_raw)

      angle = self$angle
      translate = self$origin
      offset = self$spacing/2
      translate = translate - c(offset, offset)

      self$block_layout <- rotate_sf(self$block_layout_raw, angle, c(offset, offset))
      self$tree_layout <- rotate_sf(self$tree_layout_raw, angle, c(offset, offset))

      shift = sf::st_sfc(sf::st_point(translate))
      self$block_layout = sf::st_set_geometry(self$block_layout, sf::st_geometry(self$block_layout) + shift)
      self$tree_layout = sf::st_set_geometry(self$tree_layout,  sf::st_geometry(self$tree_layout) + shift)
      self$tree_layout_adjusted = self$tree_layout

      self$set_crs(crs)
      #plot(self$tree_layout_raw$geometry, col = "red", axes = T)
      #plot(self$tree_layout$geometry, col = "blue", add = T)
    },

    plot = function()
    {
      plot(sf::st_geometry(self$block_layout), axes = TRUE, main = "Block and tree pattern")

      blk = split(self$tree_layout, self$tree_layout$Block)
      lines_list <- lapply(blk, function(block) {
        sf::st_cast(sf::st_combine(block), "LINESTRING")
      })
      lines = do.call(c, lines_list)
      plot(lines, add = T, col = "gray")

      plot(self$tree_layout, add = T, pch = 19, cex = 0.25)
    }
  )
)


rotation <- function(a)
{
  r <- a * pi / 180 # degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

#' @export
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
  # create list of polygons
  polys <- lapply(seq_len(nrow(block_layout)), function(i) {
    row <- block_layout[i, ]
    coords <- rbind(
      c((row$BlockCol - 1) * block_size, (row$BlockRow - 1) * block_size), # bottom-left
      c((row$BlockCol)     * block_size, (row$BlockRow - 1) * block_size), # bottom-right
      c((row$BlockCol)     * block_size, (row$BlockRow)     * block_size), # top-right
      c((row$BlockCol - 1) * block_size, (row$BlockRow)     * block_size), # top-left
      c((row$BlockCol - 1) * block_size, (row$BlockRow - 1) * block_size)  # close polygon
    )
    sf::st_polygon(list(coords))
  })

  # bind to sf
  block_layout$geometry <- sf::st_sfc(polys)
  sf::st_as_sf(block_layout)
}

#' @export
generate_trees = function(block_layout, block_size, num_trees, start, orientation)
{
  block_centers = sf::st_centroid(block_layout)
  block_centers = sf::st_coordinates(block_centers)
  tree_layout = lapply(1:nrow(block_centers), function(i)
  {
    block = generate_snake_coords(num_trees, block_centers[i,1], block_centers[i,2], block_size/num_trees, start = start, orientation = orientation)
    block = sf::st_as_sf(block, coords = c("x", "y"))
    block$Block = i
    block
  })
  tree_layout = do.call(rbind, tree_layout)
  tree_layout
}
