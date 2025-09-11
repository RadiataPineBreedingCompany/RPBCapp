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
    f = tempfile(fileext = ".tif")
    u = terra::writeRaster(echm, f, overwrite = T)
    col <- c("red", "blue")
    leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, options = leaflet::providerTileOptions(minZoom = 8, maxZoom = 22)) |>
      leaflet::addTiles() |>
      leafem::addGeotiff(terra::sources(u), colorOptions = leafem::colorOptions(palette = lidR::height.colors(20)), na.color = "transparent") |>
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

  #trees = cbind(mesurage[!cut,], trees)
  trees = sf::st_as_sf(trees)

  return(trees)
}

#' @export
validate_tree = function(trees, plan, spacing, hmin)
{
  trees = remove_virtual_trees(trees)
  plan = remove_virtual_trees(plan)

  trees$TREEID = plan$TREEID = 1:nrow(plan)
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
  idx <- which(dist_matrix < th, arr.ind = TRUE)
  too_close = NULL
  if (nrow(idx) > 0)
  {
    too_close = trees[idx[,1], ]
    too_close = sf::st_buffer(too_close, th)
    too_close$reason = "Too close"
  }


  # No apex but relatively high height. We are expecting those tree to be on the ground
  #b = trees$TreeFound == TRUE & !trees$ApexFound
  #too_high = NULL
  #if (sum(b) > 0)
  #{
  #  too_high = trees[b, ]
  #  too_high = sf::st_buffer(too_high, spacing/2)
  #  too_high$reason = "Too high"
  #}

  # Vector of movements
  coords_tmp <- sf::st_coordinates(trees)
  coords_plan <- sf::st_coordinates(plan)
  debug_lines <- sf::st_sfc(lapply(seq_len(nrow(coords_tmp)), function(i) { sf::st_linestring(rbind(coords_tmp[i, ], coords_plan[i, ])) }))
  debug_lines <- sf::st_sf(geometry = debug_lines)
  suppressWarnings(sf::st_crs(debug_lines) <- sf::st_crs(trees))


  tree_warning = too_close

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
  chm = terra::toMemory(chm)
  echm = terra::toMemory(echm)

  col = lidR::pastel.colors(nrow(trees))

  prog$tick(1, "Individual tree segmentation")

  pos = sf::st_geometry(trees)

  nofound = trees$ApexFound == FALSE
  virtual_trees = trees[[BLOCKNAME]] < 0

  # Handle extra trees required to buffer the segmentation
  id = 1:nrow(trees)
  seeds = sf::st_as_sf(data.frame(TREEID = id, geometry = pos))

  if (use_dalponte)
  {
    segment = lidR::dalponte2016(echm, seeds, th_cr = 0, max_cr = spacing/2/terra::res(echm)[1]*1.25, th_tree = hmin, ID = "TREEID")
    rcrowns = segment()
    pcrown = sf::st_as_sf(terra::as.polygons(rcrowns))
    names(pcrown)[1] = "TREEID"
  }

  if (!use_dalponte)
  {
    pcrown <- ForestTools::mcws(treetops = seeds, CHM = echm, minHeight = hmin, IDfield = "TREEID", format = "polygons")
  }


  prog$tick(2, "Individual tree measurement")

  pcrown$ApexFound = trees$ApexFound
  #pcrown = cbind(mesurage[!cut,], pcrown)
  pcrown = sf::st_as_sf(pcrown)

  # Extract pixel values for each crown to estimate the height
  val = terra::extract(chm, pcrown)
  height = aggregate(val$Z, by = list(val$ID), max, na.rm = TRUE)$x
  height[is.infinite(height)] = NA
  height[nofound] = NA

  area = round(as.numeric(sf::st_area(pcrown)), 2)
  area[area == 0] = NA
  area[nofound] = NA

  pcrown =  sf::st_simplify(pcrown, dTolerance = 0.1)

  pcrown$CrownArea = area
  pcrown$Height = round(height,2)
  trees$CrownArea = area
  trees$Height = round(height,2)

  trees$TREEID = NULL
  pcrown$TREEID = NULL
  pcrown[[BLOCKNAME]] = trees[[BLOCKNAME]]
  pcrown[[TPOSNAME]] = trees[[TPOSNAME]]
  pcrown[[ROWNAME]] = trees[[ROWNAME]]
  pcrown[[COLNAME]] = trees[[COLNAME]]

  # Remove polygon for which with have no apex
  sf::st_geometry(pcrown)[pcrown$ApexFound == FALSE] <- sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(pcrown))

  #trees$TreeFound = NULL
  #trees$ApexFound = NULL
  pcrown$ApexFound = NULL

  order <- c(LAYOUTNAMES, "ApexFound", "TreeFound", "Height", "CrownArea", attr(trees, "sf_column"))
  trees = trees[, order]
  order <- c(LAYOUTNAMES, "Height", "CrownArea", attr(pcrown, "sf_column"))
  pcrown = pcrown[, order]

  pcrown = pcrown[virtual_trees == FALSE,]
  trees = trees[virtual_trees == FALSE,]

  prog$tick(3, "Done")

  return(list(trees = trees, crowns = pcrown))
}

