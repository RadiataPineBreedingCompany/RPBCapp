snake_pattern <- function(n, orientation = c("h", "v"), start = c("tl", "tr", "bl", "br"))
{
  orientation <- match.arg(orientation)
  start <- match.arg(start)

  # build sequence of (row, col) pairs for top-left start
  rc_list <- vector("list", n * n)
  k <- 1
  if (orientation == "h") {
    for (r in seq_len(n)) {
      cols <- if (r %% 2 == 1) seq_len(n) else rev(seq_len(n))
      for (c in cols) {
        rc_list[[k]] <- c(r, c); k <- k + 1
      }
    }
  } else { # vertical
    for (c in seq_len(n)) {
      rows <- if (c %% 2 == 1) seq_len(n) else rev(seq_len(n))
      for (r in rows) {
        rc_list[[k]] <- c(r, c); k <- k + 1
      }
    }
  }

  rc <- do.call(rbind, rc_list)
  rr <- rc[, 1]; cc <- rc[, 2]

  # apply corner transform
  if (start == "tr") {
    cc <- n - cc + 1
  } else if (start == "bl") {
    rr <- n - rr + 1
  } else if (start == "br") {
    rr <- n - rr + 1
    cc <- n - cc + 1
  }

  # convert (row, col) to row-major index for expand.grid(x, y)
  idx <- (rr - 1) * n + cc
  as.integer(idx)
}

#' Generate coordinates in a snake pattern on a square grid
#'
#' Creates the coordinates of \code{n} x \code{n} points arranged in a square grid
#' centered at a specified location, spaced evenly, and ordered in a "snake" pattern
#' (back-and-forth either horizontally or vertically) starting from any of the four corners.
#'
#' @param n Integer. Number of points along one side of the square grid.
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
generate_snake_coords <- function(n, cx, cy, spacing,  orientation = c("h", "v"), start = c("tl", "tr", "bl", "br"))
{
  orientation <- match.arg(orientation)
  start <- match.arg(start)

  order <- snake_pattern(n, orientation, start)

  offsets <- seq(-(n-1)/2, (n-1)/2) * spacing

  # IMPORTANT: use row-major ordering (top row first, left->right)
  grid <- expand.grid(x = offsets, y = rev(offsets))

  Prow <- rep(n:1, each = n)
  Pcol <- rep(letters[1:n], n)

  coords <- grid[order, , drop = FALSE]
  coords$x <- coords$x + cx
  coords$y <- coords$y + cy
  coords$Tpos <- seq_len(nrow(coords))

  coords$Prow <- Prow[order]
  coords$Pcol <- Pcol[order]
  coords
}

