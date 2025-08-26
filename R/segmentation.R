extract_with_buffer = function(raster, points, buffer)
{
  names(raster) = "Z"
  discs = sf::st_as_sf(sf::st_buffer(points, buffer))
  ans = terra::extract(raster, discs, xy = TRUE)
  ans = data.table::as.data.table(ans)
  ans
}

#' @export
relocate_trees = function(chm, echm, plan, spacing, hmin, progress = NULL)
{
  resolution = terra::res(echm)[1]
  max_adjustment = spacing/2
  search_radius = resolution*sqrt(2)*1.1    # to move upward in the echm gradient
  iterations = as.integer(max_adjustment/resolution+1)  # number of movements

  prog <- make_progress(progress, iterations)
  on.exit(prog$finalize(), add = TRUE)

  # -----------------------------------------
  # Plan adjustemnt
  # ----------------------------------------

  # Move the points of the plan by moving upward to reach apices
  tmp = sf::st_geometry(plan)

  for (i in 1:iterations)
  {
    prog$tick(i)

    ans = extract_with_buffer(echm, tmp, search_radius)
    j = ans[, .I[which.max(Z)], by = ID]$V1
    tmp = ans[j]

    tmp = sf::st_as_sf(tmp, coords = c("x", "y"))
    sf::st_crs(tmp) = sf::st_crs(chm)
    names(tmp)[2] = "H"
  }

  distance = as.numeric(sf::st_distance(tmp, plan, by_element = T))

  # -------------------------------------------
  # Height extraction
  # -------------------------------------------

  neighborhood <- extract_with_buffer(chm, tmp, search_radius)
  local_max_values = aggregate(neighborhood$Z, by = list(neighborhood$ID), max)$x

  tmp$Height = round(local_max_values, 2)

  # -------------------------------------------
  # Verify if each local tree is a local maximum
  # otherwise reset its position
  # -------------------------------------------

  point_values <- terra::extract(echm, tmp)[,2]
  neighborhood <- extract_with_buffer(echm, tmp, search_radius)
  local_max_values = aggregate(neighborhood$Z, by = list(neighborhood$ID), max)$x
  h = tmp$Height
  h[is.na(h)] = -1
  is_local_max <- point_values == local_max_values & distance < max_adjustment & h > hmin

  # Reset geometry of non local max
  sf::st_geometry(tmp)[!is_local_max] = sf::st_geometry(plan)[!is_local_max]

  # Visualize
  if (FALSE)
  {
    col <- c("red", "blue")
    leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, options = leaflet::providerTileOptions(minZoom = 8, maxZoom = 22)) |>
      leaflet::addTiles() |>
      leafem::addGeotiff(terra::sources(echm), colorOptions = leafem::colorOptions(palette = lidR::height.colors(20)), na.color = "transparent") |>
      leaflet::addCircleMarkers(data = sf::st_transform(tmp,4326), radius = 2, color = col[is_local_max + 1], fillOpacity = 0.9, stroke = FALSE) |>
      leaflet::addLegend(position = "topright", colors = col, labels = c("not local max", "local max"), title = "CHM et plan corrigé")
  }

  # For non local max, estimate the local min value. Maybe the point is in or close to a gap
  neighborhood <- extract_with_buffer(chm, plan[!is_local_max,], search_radius)
  local_min_values = aggregate(neighborhood$Z, by = list(neighborhood$ID), min)$x
  tmp$Height[!is_local_max] = round(local_min_values, 2)

  trees = plan
  sf::st_geometry(trees) <- sf::st_geometry(tmp)
  trees$Height = tmp$Height
  trees$TreeFound = tmp$Height >= hmin
  trees$ApexFound = is_local_max
  trees$TreeFound[trees$ApexFound == FALSE & is.na(trees$TreeFound)] = TRUE
  sf::st_geometry(trees) = sf::st_geometry(tmp)

  trees$ID = NULL
  trees$H = NULL
  trees$TREEID = 1:nrow(trees)

  #trees = cbind(mesurage[!cut,], trees)
  trees = sf::st_as_sf(trees)

  return(trees)
}

#' @export
validate_tree = function(trees, plan, spacing, hmin)
{
  # Is there some duplicated trees?
  validation = data.table::as.data.table(sf::st_coordinates(trees))
  validation$ApexFound = trees$ApexFound
  validation$TREEID = trees$TREEID
  validation = validation[ApexFound == TRUE, list(.N, TREEID), by = list(X,Y)]
  err = validation$N > 1
  if (any(err))
  {
    i = validation[err]
    err = trees[trees$TREEID %in% i$TREEID,]
    plot(sf::st_geometry(err), col = "red", cex = 2, add = T)
    #st_write(err, ftreeserr, delete_dsn = TRUE)
    stop(paste0(nrow(err), " invalid tree detected. Some trees are duplicated. This should never happen. Please report to info@r-lidar.com"))
  }

  # Too close trees = warning
  th = spacing*0.5
  dist_matrix = sf::st_distance(trees)
  class(dist_matrix) = "matrix"
  diag(dist_matrix) <- Inf
  too_close <- which(dist_matrix < th, arr.ind = TRUE)
  if (nrow(too_close > 0))
  {
    too_close = trees[too_close[,1], ]
    too_close = sf::st_buffer(too_close, th)
    #plot(sf::st_geometry(too_close), col = NA, border = "yellow", cex = 2, add = T)
    #st_write(too_close, ftooclosedebug, delete_dsn = TRUE)
  }


  # No apex but relatively high height. We are expecting those tree to be on the ground
  b = trees$ApexFound == FALSE & trees$Height > hmin
  too_high = trees[b,]
  if (nrow(too_high) > 0)
  {
    too_high = trees[too_high[,1], ]
    too_high = sf::st_buffer(too_high, spacing/2)
    #plot(sf::st_geometry(too_high), col = NA, border = "green", cex = 2, add = T)
    #st_write(too_high, ftoohighdebug, delete_dsn = TRUE)
  }

  # Vector of movements
  coords_tmp <- sf::st_coordinates(trees)
  coords_plan <- sf::st_coordinates(plan)
  debug_lines <- sf::st_sfc(lapply(seq_len(nrow(coords_tmp)), function(i) { sf::st_linestring(rbind(coords_tmp[i, ], coords_plan[i, ])) }))
  debug_lines <- sf::st_sf(geometry = debug_lines)
  suppressWarnings(sf::st_crs(debug_lines) <- sf::st_crs(trees))

  reason = c(rep("Too close", nrow(too_close)), rep("Too high", nrow(too_high)))
  tree_warning = c(sf::st_geometry(too_close), sf::st_geometry(too_high))
  tree_warning = sf::st_as_sf(tree_warning)
  tree_warning$reason = reason

  return(list(move = debug_lines, warn = tree_warning))
  #plot(debug_lines, add = T, col = "white")
  #st_write(debug_lines, flinedebug, delete_dsn = TRUE)
}

#' @export
measure_trees = function(trees, chm, echm, spacing, hmin, use_dalponte = TRUE, progress = NULL)
{
  prog <- make_progress(progress, 3)
  on.exit(prog$finalize(), add = TRUE)

  names(chm) = "Z"
  names(echm) = "Z"

  col = lidR::pastel.colors(nrow(trees))

  prog$tick(1, "Individual tree segmentation")

  # Merge the extra trees required to buffer the segmentation
  #extra = sf::st_as_sf(extra)
  #extra$TREEID = -(1:nrow(extra))
  pos = c(sf::st_geometry(trees))#, sf::st_geometry(extra))
  id = c(trees$TREEID)#, extra$TREEID)
  seeds = sf::st_as_sf(data.frame(TREEID = id, geometry = pos))

  if (use_dalponte)
  {
    segment = lidR::dalponte2016(echm*1L, seeds, th_cr = 0, max_cr = spacing/2/terra::res(echm)[1]*1.25, th_tree = hmin, ID = "TREEID")
    rcrowns = segment()
    pcrown = sf::st_as_sf(terra::as.polygons(rcrowns))
    names(pcrown)[1] = "TREEID"
  }

  if (!use_dalponte)
  {
    pcrown <- ForestTools::mcws(treetops = seeds, CHM = echm, minHeight = hmin, IDfield = "TREEID", format = "polygons")
  }

  prog$tick(2, "Individual tree measurement")

  pcrown =  sf::st_simplify(pcrown, dTolerance = 0.1)

  # Remove the extra trees
  pcrown = pcrown[pcrown$TREEID > 0,]

  pcrown$ApexFound = trees$ApexFound
  #pcrown = cbind(mesurage[!cut,], pcrown)
  pcrown = sf::st_as_sf(pcrown)


  # Extract pixel values for each crown to estimate the height
  val = terra::extract(chm, pcrown)
  height = aggregate(val$Z, by = list(val$ID), max, na.rm = TRUE)$x
  height[is.infinite(height)] = NA

  area = round(as.numeric(sf::st_area(pcrown)), 2)
  area[area == 0] = NA

  pcrown$CrownArea = area
  pcrown$Height = round(height,2)
  trees$CrownArea = area
  trees$Height = round(height,2)

  trees$TREEID = NULL
  pcrown$TREEID = NULL
  pcrown$Block = trees$Block
  pcrown$Tpos = trees$Tpos
  pcrown$Prow = trees$Prow
  pcrown$Pcol = trees$Pcol

  # Remove polygon for which with have no apex
  sf::st_geometry(pcrown)[pcrown$ApexFound == FALSE] <- sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(pcrown))

  #trees$TreeFound = NULL
  #trees$ApexFound = NULL
  pcrown$ApexFound = NULL

  order <- c( "Block", "Tpos", "Prow", "Pcol", "ApexFound", "TreeFound", "Height", "CrownArea", "geometry")
  trees = trees[, order]
  order <- c( "Block", "Tpos", "Prow", "Pcol", "Height", "CrownArea", "geometry")
  pcrown = pcrown[, order]

  prog$tick(3, "Done")

  return(list(trees = trees, crowns = pcrown))
}

