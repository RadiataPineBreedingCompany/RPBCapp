#' @export
layout_alignment_angle = function(layout, chm, pivot, ws, boundaries)
{
  #' layout = self$layout$tree_layout_oriented
  #' chm = self$schm
  #' pivot = self$layout$origin
  #' ws = self$layout$spacing*0.75
  #' boundaries = self$boundaries

  ttps = lidR::locate_trees(chm, lidR::lmf(ws))
  layout = remove_virtual_trees(layout)
  u = sf::st_contains(boundaries, ttps)
  ttps = ttps[u[[1]],]

  #terra::plot(self$schm)
  #plot(ttps$geometry, add = T, cex = 0.5)

  vref = sf::st_coordinates(ttps)
  vref = vref[,1:2]
  umov = sf::st_coordinates(layout)

  #plot(vref, col = "blue")
  #points(umov, col = "red")

  rms_distance_nn <- function(angle, A, B, p, debug = FALSE)
  {
    theta = angle*pi/180
    # Rotation matrix
    R <- matrix(c(cos(theta), -sin(theta),
                  sin(theta),  cos(theta)), ncol = 2, byrow = TRUE)

    # Center both sets at pivot
    A0 <- sweep(A, 2, p)
    B0 <- sweep(B, 2, p)

    # Rotate A
    A_rot <- t(R %*% t(A0))

    # 1-NN distances from rotated A to B
    nn <- RANN::nn2(data = B0, query = A_rot, k = 1)
    rms = sqrt(mean(nn$nn.dists^2))  # RMS

    if (debug)
    {
      plot(A_rot, asp = 1, col = "red")
      points(B0)
    }
    return(rms)
  }

  res <- optimize(f = rms_distance_nn, interval = c(-180, 180), A = umov, B = vref, p = pivot)
  cat('rms = ', res$objective, "\n")

  angle = res$minimum

  loss <- function(params, A, B, pivot, angle)
  {
    angle_rad = angle * pi / 180
    tx <- params[1]
    ty <- params[2]

    # Center both sets at pivot
    A0 <- sweep(A, 2, pivot)
    B0 <- sweep(B, 2, pivot)

    # rotation
    R <- matrix(c(cos(angle_rad), -sin(angle_rad),
                  sin(angle_rad),  cos(angle_rad)), 2, 2)

    # apply rotation then translation
    rotated <- t(R %*% t(A0))
    transformed <- rotated + matrix(c(tx, ty), nrow(A), 2, byrow = TRUE)

    if (TRUE)
    {
      plot(B0, asp = 1, main = paste(params))
      points(rotated, col = "blue")
      points(transformed, col ="red")
    }

    nn <- RANN::nn2(data = B0, query = transformed, k = 1)
    rms = sqrt(mean(nn$nn.dists^2))  # RMS
    rms
  }

  # Adjust in translation
  init = c(0,0)
  fit <- nlm(loss, p = init, A = umov, B = vref, pivot = pivot, angle = -angle)

  cat("rms =", fit$minimum, "\n")

  res = c(angle, fit$estimate)
  return(res)
}
