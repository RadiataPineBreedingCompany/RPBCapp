#' Calculate crown attributes for a set of tree points
#'
#' This function calculates various crown attributes for a set of 3D tree points including:
#' crown height (Zmax, Zq999, Zq99, Z_mean), crown size (n_points, vol_convex, vol_concave),
#' and crown complexity (CV_Z, CRR).
#'
#' @param X Numeric vector, x-coordinates of tree points
#' @param Y Numeric vector, y-coordinates of tree points
#' @param Z Numeric vector, z-coordinates of tree points
#'
#' @return A data frame with columns for each crown attribute.
#
hull_metrics <- function(X, Y, Z, alpha = 1)
{
  if (length(X) < 10)
  {
    return(list(
      vol_concave = NA_real_,
      vol_convex = NA_real_,
      CRR = NA_real_
    ))
  }

  if (length(X) < 10)
  {
    return(list(
      vol_concave = NA_real_,
      vol_convex = NA_real_,
      CRR = NA_real_
    ))
  }

  # Build matrix and center XY
  a3d <- cbind(
    X - mean(X),
    Y - mean(Y),
    Z
  )

  alpha  <- c(Inf, alpha)
  ashape <- alphashape3d::ashape3d(x = a3d, alpha = alpha, pert = TRUE)

  v1 = volume_ashape3d_fast(ashape, indexAlpha = 1)
  v2 = volume_ashape3d_fast(ashape, indexAlpha = 2)

  list(
    vol_concave = v1,
    vol_convex  = v2,
    CRR         = (mean(Z) - min(Z)) / (max(Z) - min(Z))
  )
}

volume_ashape3d_fast <- function(as3d, indexAlpha = 1)
{
  tetra <- as3d$tetra
  x     <- as3d$x

  # Extract all 4 vertices for every tetrahedron at once (each is n x 3)
  x1 <- x[tetra[, 1], ]
  x2 <- x[tetra[, 2], ]
  x3 <- x[tetra[, 3], ]
  x4 <- x[tetra[, 4], ]

  # Vectorized scalar triple product  |( x4-x1 ) · ( (x2-x1) × (x3-x1) )| / 6
  d41 <- x4 - x1
  d21 <- x2 - x1
  d31 <- x3 - x1

  # Cross product (x2-x1) × (x3-x1), row-wise
  cross_x <- d21[, 2] * d31[, 3] - d21[, 3] * d31[, 2]
  cross_y <- d21[, 3] * d31[, 1] - d21[, 1] * d31[, 3]
  cross_z <- d21[, 1] * d31[, 2] - d21[, 2] * d31[, 1]

  # Dot product with (x4-x1), then volume of each tetrahedron
  vol <- abs(d41[, 1] * cross_x + d41[, 2] * cross_y + d41[, 3] * cross_z) / 6

  # Sum only tetrahedra that belong to the alpha shape for each requested alpha
  vapply(indexAlpha, function(iAlpha) {
    sum(vol[tetra[, 5 + iAlpha] == 1])
  }, numeric(1))
}
