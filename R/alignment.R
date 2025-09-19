#' @export
layout_alignment_lm = function(layout, chm, pivot, ws, boundaries, progress = NULL)
{
  prog <- make_progress(progress, 4)
  on.exit(prog$finalize(), add = TRUE)

  prog$tick(1, "Automatic tree location")

  debug = FALSE

  ttps = lidR::locate_trees(chm, lidR::lmf(min(ws)))
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': automatic detection of trees failed and found zero trees. Please report at info@r-lidar.com")


  u = sf::st_contains(boundaries, ttps)
  ttps = ttps[u[[1]],]
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': trees are not withing the boundaries. Please report at info@r-lidar.com")

  layout = remove_virtual_trees(layout)
  pivot2 = find_tree_zero(layout)

  vref = sf::st_coordinates(ttps)[,1:2]
  umov = sf::st_coordinates(layout)[,1:2]

  vref = sweep(vref, 2, pivot)    # shift to pivot
  umov = sweep(umov, 2, pivot2)   # shift to pivot2

  if (debug)
  {
    plot(vref, col = "blue", asp = 1, main = "vref (blue) vs umov (red)")
    points(umov, col = "red")
  }

  prog$tick(2, "Rotation optimization")

  # ---- optimize rotation ----
  rms_distance_nn <- function(angle, A, B)
  {
    theta = angle*pi/180
    R <- matrix(c(cos(theta), -sin(theta),
                  sin(theta),  cos(theta)), 2, 2)
    A_rot <- t(R %*% t(A))
    nn <- RANN::nn2(data = B, query = A_rot, k = 1)
    rms = sqrt(mean(nn$nn.dists^2))

    if (debug)
    {
      plot(B, asp = 1, main = paste("angle =", round(angle,1)))
      points(A, col = "red")
      points(A_rot, col = "blue")
      plot(boundaries$geometry, add = TRUE)
      cat("angle =", round(angle,1), " rms =", round(rms,2), "\n")
    }

    return(rms)
  }

  res <- optimize(f = rms_distance_nn, interval = c(-180, 180), A = umov, B = vref)
  angle = res$minimum
  cat("angle =", round(angle,1), " rms =", round(res$objective,2), "\n")

  if (debug)
  {
    plot(vref, col = "blue", asp = 1)
    p  = rotate_sf(sf::st_as_sf(sf::st_sfc(sf::st_point(pivot2))), angle)
    points(rotate_sf(layout, angle)$geometry - p$x , col = "red")
  }

  prog$tick(3, "Translation optimization")

  # ---- optimize translation ----
  loss <- function(params, A, B, angle)
  {
    angle_rad = angle * pi / 180
    R <- matrix(c(cos(angle_rad), -sin(angle_rad),
                  sin(angle_rad),  cos(angle_rad)), 2, 2)
    rotated <- t(R %*% t(A))
    transformed <- rotated + matrix(params, nrow(A), 2, byrow = TRUE)

    if (debug) {
      plot(B, asp = 1, main = paste("tx =", params[1], "ty =", params[2]))
      points(rotated, col = "blue")
      points(transformed, col = "red")
    }

    nn <- RANN::nn2(data = B, query = transformed, k = 1)
    sqrt(mean(nn$nn.dists^2))
  }

  fit <- nlm(loss, p = c(0,0), A = umov, B = vref, angle = angle)

  cat("rms =", round(fit$minimum,2), " move = (", paste(round(fit$estimate,2), collapse = ", "), ")\n")


  # ---- build homogeneous transformation ----
  θ <- -angle * pi/180
  R <- matrix(c(cos(θ), -sin(θ), 0,
                sin(θ),  cos(θ), 0,
                0,       0,      1), 3, 3, byrow = TRUE)

  T1 <- matrix(c(1, 0, -pivot2[1],
                 0, 1, -pivot2[2],
                 0, 0,  1), 3, 3, byrow = TRUE)

  T2 <- matrix(c(1, 0, pivot[1] + fit$estimate[1],
                 0, 1, pivot[2] + fit$estimate[2],
                 0, 0, 1), 3, 3, byrow = TRUE)

  M <- T2 %*% R %*% T1

  prog$tick(4, "Completed")

  cat("Final 3x3 transform:\n")
  print(M)

  return(M)
}


#' @export
layout_alignment_svd <- function(local_points, global_points)
{
  if (!is.matrix(local_points) || !is.matrix(global_points))
    stop("Input points must be matrices. Please report to info@r-lidar.com")

  if (nrow(local_points) < 2)
    stop("At least 2 points must be moved to realign the tree layout.")

  if (ncol(local_points) != 2)
    stop("The matrices must have 2 columns. Please report to info@r-lidar.com")

  if (ncol(local_points) != ncol(global_points))
    stop("Different number of columns in local and global coordinates. Please report to info@r-lidar.com")

  if (nrow(local_points) != nrow(global_points))
    stop("Different number of rows in local and global coordinates. Please report to info@r-lidar.com")

  local_points = local_points[,1:2]
  global_points = global_points[,1:2]

  # Compute centroids
  centroid_local <- colMeans(local_points)
  centroid_global <- colMeans(global_points)

  # Center the points around their centroids
  centered_local <- sweep(local_points, 2, centroid_local, "-")
  centered_global <- sweep(global_points, 2, centroid_global, "-")

  # Compute the cross-covariance matrix
  H <- t(centered_local) %*% centered_global

  # Perform Singular Value Decomposition (SVD)
  svd_result <- svd(H)
  U <- svd_result$u
  V <- svd_result$v

  # Compute the rotation matrix
  R <- V %*% t(U)

  # Ensure proper rotation (handle reflection case)
  if (det(R) < 0) {
    V[, 2] <- -V[, 2]
    R <- V %*% t(U)
  }

  # Compute the translation vector
  t <- centroid_global - R %*% centroid_local

  # Combine into a transformation matrix
  transformation_matrix <- matrix(0, 3, 3)
  transformation_matrix[1:2, 1:2] <- R
  transformation_matrix[1:2, 3] <- t
  transformation_matrix[3, 3] <- 1

  return(transformation_matrix)
}

