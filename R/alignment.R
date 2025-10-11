layout_alignment_lm = function(layout, chm, pivot, ws, boundaries, progress = NULL)
{
  prog <- make_progress(progress, 4)
  on.exit(prog$finalize(), add = TRUE)

  prog$tick(1, "Automatic tree location")

  debug = FALSE

  layout = remove_cut_trees(layout)

  ttps = lidR::locate_trees(chm, lidR::lmf(min(ws)))
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': automatic detection of trees failed and found zero trees. Please report at info@r-lidar.com")


  u = sf::st_contains(boundaries, ttps)
  ttps = ttps[u[[1]],]
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': trees are not withing the boundaries. Please report at info@r-lidar.com")

  pivot2 = find_tree_zero(layout)

  vref = sf::st_coordinates(ttps)[,1:2]
  umov = sf::st_coordinates(layout)[,1:2]

  vref = sweep(vref, 2, pivot)    # shift to pivot
  umov = sweep(umov, 2, pivot2)   # shift to pivot2

  if (debug)
  {
    plot(vref, col = "blue", asp = 1, main = "vref (blue) vs umov (red)")
    graphics::points(umov, col = "red")
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
      graphics::points(A, col = "red")
      graphics::points(A_rot, col = "blue")
      plot(boundaries$geometry, add = TRUE)
      cat("angle =", round(angle,1), " rms =", round(rms,2), "\n")
    }

    return(rms)
  }

  res <- stats::optimize(f = rms_distance_nn, interval = c(-180, 180), A = umov, B = vref)
  angle = res$minimum
  cat("angle =", round(angle,1), " rms =", round(res$objective,2), "\n")

  if (debug)
  {
    plot(vref, col = "blue", asp = 1)
    p  = rotate_sf(sf::st_as_sf(sf::st_sfc(sf::st_point(pivot2))), angle)
    graphics::points(rotate_sf(layout, angle)$geometry - p$x , col = "red")
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
      graphics::points(rotated, col = "blue")
      graphics::points(transformed, col = "red")
    }

    nn <- RANN::nn2(data = B, query = transformed, k = 1)
    sqrt(mean(nn$nn.dists^2))
  }

  fit <- stats::nlm(loss, p = c(0,0), A = umov, B = vref, angle = angle)

  cat("rms =", round(fit$minimum,2), " move = (", paste(round(fit$estimate,2), collapse = ", "), ")\n")


  # ---- build homogeneous transformation ----
  theta <- -angle * pi/180
  R <- matrix(c(cos(theta), -sin(theta), 0,
                sin(theta),  cos(theta), 0,
                0,           0,          1), 3, 3, byrow = TRUE)

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

layout_optimize_by_block = function(layout, blocks, chm, ws, boundaries, progress = NULL)
{
  ws = min(ws)

  prog <- make_progress(progress, 4)
  on.exit(prog$finalize(), add = TRUE)
  prog$tick(1, "Automatic tree location")

  ttps = lidR::locate_trees(chm, lidR::lmf(ws))
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': automatic detection of trees failed and found zero trees. Please report at info@r-lidar.com")


  u = sf::st_contains(boundaries, ttps)
  ttps = ttps[u[[1]],]
  if (nrow(ttps) == 0)
    stop("Internal error in 'layout_alignment_lm': trees are not withing the boundaries. Please report at info@r-lidar.com")

  rmse_transform <- function(params, A, B)
  {
    #theta <- params[1]
    #t <- params[2:3]
    theta = 0
    t = params[1:2]

    # Build 3x3 homogeneous transformation
    M <- matrix(c(
      cos(theta), -sin(theta), t[1],
      sin(theta),  cos(theta), t[2],
      0,           0,          1
    ), nrow = 3, byrow = TRUE)


    # Convert A to homogeneous coordinates
    A_h <- cbind(A, 1)
    A_trans <- t(M %*% t(A_h))
    A_trans <- A_trans[,1:2]  # back to 2D

    # Nearest neighbors in B
    nn <- RANN::nn2(data = B, query = A_trans, k = 1)
    d = sort(nn$nn.dists)
    rms = sqrt(mean(d^2))
  }

  prog$tick(2, "Optim by block")

  Mats = lapply(1:length(blocks$geometry), function(i)
  {
    block = blocks[i,]
    sf::st_agr(block) = "constant"
    sf::st_agr(ttps) = "constant"
    sf::st_agr(layout) = "constant"
    block_ttop = sf::st_intersection(block, ttps)
    block_layout = sf::st_intersection(block, layout)

    B = sf::st_coordinates(block_ttop)[,1:2]
    A = sf::st_coordinates(block_layout)[,1:2]

    # Example: optimize
    res <- stats::optim(
      c(0, 0),
      rmse_transform,
      A = A,
      B = B,
      method = "L-BFGS-B",
      lower = c(-ws/3, -ws/3),
      upper = c(ws/3, ws/3))

    theta_opt <- 0# res$par[1]
    t_opt <- res$par[1:2]

    # Final 3x3 transformation matrix
    T_opt <- matrix(c(
      cos(theta_opt), -sin(theta_opt), t_opt[1],
      sin(theta_opt),  cos(theta_opt), t_opt[2],
      0, 0, 1
    ), nrow = 3, byrow = TRUE)

    if (FALSE)
    {
      A_h = cbind(A, 1)
      A_trans = A_h %*% t(T_opt)
      A_trans = A_trans[,1:2]
      plot(block$geometry, axe = TRUE, main = i)
      graphics::points(B, pch = 19)
      graphics::points(A, col = "red", pch = 19)
      graphics::points(A_trans, col = "blue", pch = 19)
    }

    T_opt
  })

  prog$tick(3, "Update")

  split_layout = split(layout, layout$Pset)

  for (i in 1:length(split_layout))
  {
    M = Mats[[i]]
    xy = sf::st_coordinates(split_layout[[i]])
    xy = cbind(xy, 1)
    xy = xy %*% t(M)
    xy = xy[,1:2]
    geom = sf::st_sfc(lapply(seq_len(nrow(xy)), function(j) {
      sf::st_point(xy[j, ])
    }))

    split_layout[[i]] = sf::st_set_geometry(split_layout[[i]], geom)
  }

  new_layout = do.call(rbind, split_layout)
  sf::st_crs(new_layout) = sf::st_crs(layout)

  prog$tick(4, "Done")

  new_layout
}



