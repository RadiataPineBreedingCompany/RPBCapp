#' @export
layout_alignment_lm = function(layout, chm, pivot, ws, boundaries)
{
  debug = FALSE
  # layout = self$layout$tree_layout_raw
  # chm = self$schm
  # pivot = self$layout$origin
  # ws = self$layout$spacing*0.75
  # boundaries = self$boundaries

  ttps = lidR::locate_trees(chm, lidR::lmf(min(ws)))

  if (nrow(ttps) == 0)
    stop("Internal error: automatic detection of trees failed and found zero trees. Please report at info@r-lidar.com")

  layout = remove_virtual_trees(layout)
  u = sf::st_contains(boundaries, ttps)
  ttps = ttps[u[[1]],]

  pivot2 = sf::st_coordinates(layout$geometry[1])

  if (debug)
  {
    terra::plot(chm)
    plot(boundaries$geometry, add = T)
    plot(ttps$geometry, add = T, cex = 0.5)
  }

  vref = sf::st_coordinates(ttps)
  vref = vref[,1:2]
  umov = sf::st_coordinates(layout)

  vref = sweep(vref, 2, pivot)
  umov = sweep(umov, 2, pivot2)

  if (debug)
  {
    plot(vref, col = "blue", asp = 1)
    points(umov, col = "red")
  }

  rms_distance_nn <- function(angle, A, B)
  {
    theta = angle*pi/180
    # Rotation matrix
    R <- matrix(c(cos(theta), -sin(theta),
                  sin(theta),  cos(theta)), ncol = 2, byrow = TRUE)

    # Rotate A
    A_rot <- t(R %*% t(A))

    # 1-NN distances from rotated A to B
    nn <- RANN::nn2(data = B, query = A_rot, k = 1)
    d = as.numeric(nn$nn.dists)
    rms = sqrt(mean(d^2))  # RMS

    if (debug)
    {
      plot(B, asp = 1)
      points(A, asp = 1, col = "red")
      points(A_rot, asp = 1, col = "blue")
      plot(boundaries$geometry, add = T)
      cat('rms =', round(rms,2), "\n")
    }

    return(rms)
  }

  res <- optimize(f = rms_distance_nn, interval = c(-180, 180), A = umov, B = vref)
  cat('angle =', round(res$minimum,1), 'rms =', round(res$objective,2), "\n")

  angle = res$minimum

  if (debug)
  {
    plot(vref, col = "blue", asp = 1)
    p  = rotate_sf(sf::st_as_sf(sf::st_sfc(sf::st_point(pivot2))), -angle)
    points(rotate_sf(layout, -angle)$geometry-p$x , col = "red")
  }

  loss <- function(params, A, B, angle)
  {
    angle_rad = angle * pi / 180
    tx <- params[1]
    ty <- params[2]

    # rotation
    R <- matrix(c(cos(angle_rad), -sin(angle_rad),
                  sin(angle_rad),  cos(angle_rad)), 2, 2)

    # apply rotation then translation
    rotated <- t(R %*% t(A))
    transformed <- rotated + matrix(c(tx, ty), nrow(A), 2, byrow = TRUE)

    if (debug)
    {
      plot(B, asp = 1, main = paste(params))
      points(rotated, col = "blue")
      points(transformed, col ="red")
    }

    nn <- RANN::nn2(data = B, query = transformed, k = 1)
    rms = sqrt(mean(nn$nn.dists^2))  # RMS
    rms
  }


  # Adjust in translation
  init = c(0,0)
  fit <- nlm(loss, p = init, A = umov, B = vref, angle = -angle)

  cat("rms = ", round(fit$minimum,2), ' move = (', paste(round(fit$estimate,2),collapse = ", "), ")\n", sep = "")

  res = c(angle, fit$estimate)
  return(res)
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

