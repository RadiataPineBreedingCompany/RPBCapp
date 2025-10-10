snake_pattern <- function(nx, ny, orientation = c("h", "v"), start = c("tl", "tr", "bl", "br"))
{
  orientation <- match.arg(orientation)
  start <- match.arg(start)

  rc_list <- vector("list", nx * ny)
  k <- 1
  if (orientation == "h") {
    for (r in seq_len(ny)) {
      cols <- if (r %% 2 == 1) seq_len(nx) else rev(seq_len(nx))
      for (c in cols) {
        rc_list[[k]] <- c(r, c); k <- k + 1
      }
    }
  } else { # vertical snake
    for (c in seq_len(nx)) {
      rows <- if (c %% 2 == 1) seq_len(ny) else rev(seq_len(ny))
      for (r in rows) {
        rc_list[[k]] <- c(r, c); k <- k + 1
      }
    }
  }

  rc <- do.call(rbind, rc_list)
  rr <- rc[, 1]; cc <- rc[, 2]

  # Apply starting corner transform
  if (start == "tr") {
    cc <- nx - cc + 1
  } else if (start == "bl") {
    rr <- ny - rr + 1
  } else if (start == "br") {
    rr <- ny - rr + 1
    cc <- nx - cc + 1
  }

  # Convert (row, col) to row-major index (expand.grid(x, y))
  idx <- (rr - 1) * nx + cc
  as.integer(idx)
}


#' Generate coordinates in a snake pattern on a square grid
#'
#' Creates the coordinates of \code{n} x \code{n} points arranged in a square grid
#' centered at a specified location, spaced evenly, and ordered in a "snake" pattern
#' (back-and-forth either horizontally or vertically) starting from any of the four corners.
#'
#' @param nx Integer. Number of points along the X axis (columns).
#' @param ny Integer. Number of points along the Y axis (rows).
#' @param cx Numeric. X-coordinate of the center of the square.
#' @param cy Numeric. Y-coordinate of the center of the square.
#' @param spacing Numeric. Distance between adjacent points.
#' @param orientation Character. Either \code{"horizontal"} (snake pattern moves along rows)
#'   or \code{"vertical"} (snake pattern moves along columns).
#' @param start Character. Starting corner of the snake. One of:
#'   \code{"tl"} = top-left, \code{"tr"} = top-right,
#'   \code{"bl"} = bottom-left, \code{"br"} = bottom-right.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{x}{X-coordinates of points in snake order.}
#'   \item{y}{Y-coordinates of points in snake order.}
#'   \item{id}{Sequence number of each point in the snake pattern.}
#' }
#'
#' @examples
#' # 6×6, 1 m spacing, center at (100, 200), horizontal snake from bottom-left
#' coords <- generate_snake_coords(n = 6, cx = 100, cy = 200, spacing = 1, orientation = "h", start = "bl")
#' plot(coords$x, coords$y, asp = 1, type = "b")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#'
#' # 6×6 grid, 0.5 m spacing, centered at (100, 200), horizontal snake from bottom-right
#' coords <- generate_snake_coords(6, cx = 100, cy = 200, spacing = 0.5, orientation = "h", start = "br")
#' plot(coords$x, coords$y, asp = 1, type = "b", main = "Snake Pattern")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#'
#' # 6×6 grid, 1 m spacing, centered at (100, 200), vertical snake from top-left
#' coords <- generate_snake_coords(6, cx = 100, cy = 200, spacing = 1, orientation = "v", start = "tl")
#' plot(coords$x, coords$y, asp = 1, type = "b", main = "Snake Pattern")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#'
#' # 8×8 grid, 1 m spacing, centered at (0, 0), vertical snake from top-right
#' coords <- generate_snake_coords(8, cx = 0, cy = 0, spacing = 1, orientation = "v", start = "tr")
#' plot(coords$x, coords$y, asp = 1, type = "b", main = "Snake Pattern")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#'
#' # 8×8 grid, 1 m spacing, centered at (0, 0), horizontal snake from top-right
#' coords <- generate_snake_coords(8, cx = 0, cy = 0, spacing = 1, orientation = "h", start = "tr")
#' plot(coords$x, coords$y, asp = 1, type = "b", main = "Snake Pattern")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#'
#' coords <- generate_snake_coords(4, cx = 10, cy = 10, spacing = 2, orientation = "h", start = "tl")
#' plot(coords$x, coords$y, asp = 1, type = "b", main = "Snake Pattern")
#' text(coords$x, coords$y, labels = seq_len(nrow(coords)), pos = 3, cex = 0.8, col = "blue")
#' @seealso \code{\link{snake_pattern}} for generating the ordering indices only.
#' @export
generate_snake_coords <- function(nx, ny = nx, cx, cy, spacing_x, spacing_y, orientation = c("h", "v"), start = c("tl", "tr", "bl", "br"))
{
  orientation <- match.arg(orientation)
  start <- match.arg(start)
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  if (nx < 1 || ny < 1)
    stop("Both nx and ny must be >= 1")

  # Offsets: centered around (0,0)
  offsets_x <- seq(-(nx - 1)/2, (nx - 1)/2, length.out = nx) * (spacing_x / nx)
  offsets_y <- seq(-(ny - 1)/2, (ny - 1)/2, length.out = ny) * (spacing_y / ny)

  # Build row-major grid (top to bottom)
  grid <- expand.grid(x = offsets_x, y = rev(offsets_y))

  # Compute snake ordering
  order <- snake_pattern(nx, ny, orientation, start)

  # Apply ordering and shift by center
  coords <- grid[order, , drop = FALSE]
  coords$x <- coords$x + cx
  coords$y <- coords$y + cy
  coords$id <- seq_len(nrow(coords))

  coords
}

