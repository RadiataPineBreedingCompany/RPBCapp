#' @export
layout_alignment_angle = function(layout, chm, pivot, ws)
{
  ttps = lidR::locate_trees(chm, lidR::lmf(ws))

  #terra::plot(self$schm)
  #plot(ttps, add = T, cex = 0.25)

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
  return(res$minimum)
}
