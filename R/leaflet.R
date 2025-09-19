# ---- base map ----
#' @export
make_base_map <- function(groups = c()) {
  leaflet::leaflet() |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      options = leaflet::providerTileOptions(minZoom = 2, maxZoom = 22)
    ) |>
    leaflet::setView(
      lng = 174.8,
      lat = -41.0,
      zoom = 3) |>
    leaflet::addLayersControl(
      overlayGroups = groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet.extras::addResetMapButton()
}

# ---- raster layers ----
#' @export
add_dtm_layer <- function(map, dtm, proxy = FALSE) {
  if (is.null(dtm)) return(list(map = map, groups = NULL))

  if (anyNA(terra::minmax(dtm)))
  {
    warning("Cannot determine the range value of the DTM. DTM is corrupted")
    return(list(map = map, groups = NULL))
  }

  dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
  dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
  dtm_hillshade <- terra::stretch(dtm_hillshade, minq = 0.02, maxq = 0.98)
  dtm_val <- terra::values(dtm_hillshade)
  dtm_pal <- leaflet::colorNumeric(gray.colors(10, 0, 1), dtm_val, na.color = "transparent")

  if (proxy) map <- map |> leaflet::clearGroup("DTM")
  map <- map |> leaflet::addRasterImage(dtm_hillshade, colors = dtm_pal, group = "DTM", maxBytes = 2000000)
  list(map = map, groups = "DTM")
}

#' @export
add_schm_layer <- function(map, schm, proxy = FALSE) {
  if (is.null(schm)) return(list(map = map, groups = NULL))

  if (anyNA(terra::minmax(schm)))
  {
    warning("Cannot determine the range value of the sCHM. sCHM is corrupted")
    return(list(map = map, groups = NULL))
  }

  if (terra::inMemory(schm)) {
    o <- tempfile(fileext = ".tif")
    terra::writeRaster(schm, o, overwrite = TRUE)
    schm <- terra::rast(o)
  }
  file <- terra::sources(schm)

  if (proxy) map <- map |> leaflet::clearGroup("sCHM")
  map <- map |>
    leafem::addGeotiff(
      file,
      colorOptions = leafem::colorOptions(palette = lidR::height.colors(20), na.color = "transparent"),
      resolution = 128,
      layerId = 1,
      group = "sCHM",
      autozoom = FALSE
    )
  list(map = map, groups = "sCHM")
}

#' @export
add_chm_layer <- function(map, chm, proxy = FALSE) {
  if (is.null(chm)) return(list(map = map, groups = NULL))

  if (anyNA(terra::minmax(chm)))
  {
    warning("Cannot determine the range value of the CHM. CHM is corrupted")
    return(list(map = map, groups = NULL))
  }

  if (terra::inMemory(chm)) {
    o <- tempfile(fileext = ".tif")
    terra::writeRaster(chm, o, overwrite = TRUE)
    chm <- terra::rast(o)
  }
  file <- terra::sources(chm)

  if (proxy) map <- map |> leaflet::clearGroup("CHM")
  map <- map |>
    leafem::addGeotiff(
      file,
      colorOptions = leafem::colorOptions(palette = lidR::height.colors(20), na.color = "transparent"),
      resolution = 128,
      layerId = 0,
      group = "CHM",
      autozoom = FALSE
    )
  list(map = map, groups = "CHM")
}

# ---- vector layers ----
#' @export
add_bbox_layer <- function(map, bbox, proxy = FALSE) {
  if (is.null(bbox)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(bbox, 4326)
  if (proxy) map <- map |> leaflet::clearGroup("Bounding box")
  map <- map |> leaflet::addPolygons(data = data, group = "Bounding box", color = "red", fill = FALSE)
  list(map = map, groups = "Bounding box")
}

#' @export
add_boundaries_layer <- function(map, boundaries, proxy = FALSE) {
  if (is.null(boundaries)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(boundaries, 4326)
  if (proxy) map <- map |> leaflet::clearGroup("Boundaries")
  map <- map |> leaflet::addPolygons(data = data, group = "Boundaries", color = "green", fill = FALSE)
  list(map = map, groups = "Boundaries")
}

#' @export
add_block_layout_layer <- function(map, layout, proxy = FALSE) {
  if (is.null(layout) || is.null(layout$block_layout_oriented)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(layout$block_layout_oriented, 4326)
  data <- data[data$BlockID > 0, ]
  if (nrow(data) == 0) return(list(map = map, groups = NULL))
  if (proxy) map <- map |> leaflet::clearGroup("Block layout")
  map <- map |> leaflet::addPolygons(data = data, group = "Block layout", color = "black", fill = FALSE, weight = 3)
  list(map = map, groups = "Block layout")
}

#' @export
add_warnings_layer <- function(map, layout_warnings, proxy = FALSE) {
  if (is.null(layout_warnings)) return(list(map = map, groups = NULL))
  added <- character()

  # Move lines
  if (!is.null(layout_warnings$move)) {
    data_move <- sf::st_transform(layout_warnings$move, 4326)
    if (proxy) map <- map |> leaflet::clearGroup("Move")
    map <- map |> leaflet::addPolylines(data = data_move, group = "Move", color = "white", weight = 2, opacity = 0.9)
    added <- c(added, "Move")
  }

  # Warning polygons (with legend on initial render)
  if (!is.null(layout_warnings$warn)) {
    data_warn <- sf::st_transform(layout_warnings$warn, 4326)
    reason_vals <- data_warn$reason
    pal <- leaflet::colorFactor(palette = c("yellow", "green"), domain = reason_vals)

    if (proxy) map <- map |> leaflet::clearGroup("Warnings")
    map <- map |> leaflet::addPolygons(data = data_warn, opacity = 0.9, group = "Warnings",
                                       color = ~pal(reason), fill = FALSE, weight = 3)
    if (!proxy) map <- map |> leaflet::addLegend(position = "bottomleft", pal = pal, values = reason_vals, title = "Warnings")
    added <- c(added, "Warnings")
  }

  if (length(added) == 0) list(map = map, groups = NULL) else list(map = map, groups = added)
}

#' @export
add_crowns_layer <- function(map, crowns, proxy = FALSE) {
  if (is.null(crowns)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(crowns, 4326)
  data <- data[!sf::st_is_empty(data), ]
  if (nrow(data) == 0) return(list(map = map, groups = NULL))
  if (proxy) map <- map |> leaflet::clearGroup("Crowns")
  map <- map |> leaflet::addPolygons(data = data, color = "gray", fill = FALSE, opacity = 0.5, weight = 1, group = "Crowns")
  list(map = map, groups = "Crowns")
}

#' @export
add_tree_layout_layer <- function(map, layout, proxy = FALSE) {
  if (is.null(layout)) return(list(map = map, groups = NULL))

  if (!is.null(layout$tree_layout_adjusted)) {
    data <- sf::st_transform(layout$tree_layout_adjusted, 4326)
  } else {
    data <- sf::st_transform(layout$tree_layout_oriented, 4326)
  }

  data <- remove_virtual_trees(data)
  if (nrow(data) == 0) return(list(map = map, groups = NULL))

  if ("ApexFound" %in% names(data)) {
    col <- ifelse(data$ApexFound, "green", "red")
    col <- ifelse(data$TreeFound & !data$ApexFound, "orange", col)
    if (!proxy) {
      # only add legend on initial render
      map <- map |> leaflet::addLegend(
        position = "bottomright",
        colors = c("green", "orange", "red"),
        labels = c("Tree apex found", "Tree apex not found", "No tree"),
        title = "Tree status"
      )
    }
  } else {
    col <- "purple"
  }

  if (proxy) map <- map |> leaflet::clearGroup("Tree layout")
  map <- map |> leaflet::addCircleMarkers(data = data, color = col, group = "Tree layout", radius = 1, layerId = 1:nrow(data))
  list(map = map, groups = "Tree layout")
}

#' @export
add_trees_layer <- function(map, trees, proxy = FALSE) {
  if (is.null(trees)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(trees, 4326)
  data <- remove_virtual_trees(data)
  if (nrow(data) == 0) return(list(map = map, groups = NULL))

  pal <- leaflet::colorNumeric(palette = viridis::viridis(8), domain = data$Height)
  if (proxy) map <- map |> leaflet::clearGroup("Trees")

  map <- map |> leaflet::addCircleMarkers(
    data = data, group = "Trees", radius = 4,
    color = ~pal(Height), fillOpacity = 0.9, stroke = FALSE,
    popup = leafpop::popupTable(data, feature.id = FALSE, row.numbers = FALSE)
  )

  if (!proxy) {
    map <- map |> leaflet::addLegend(pal = pal, values = data$Height, title = "Tree Height (m)", opacity = 1)
  }

  list(map = map, groups = "Trees")
}

#' @export
center_on_object <- function(map, sfobj)
{
  if (is.null(sfobj)) return(map)

  # transform to WGS84
  sfobj <- sf::st_transform(sfobj, 4326)
  bb <- sf::st_bbox(sfobj)

  # convert to unnamed numeric vector
  map |> leaflet::fitBounds(
    lng1 = unname(bb["xmin"]),
    lat1 = unname(bb["ymin"]),
    lng2 = unname(bb["xmax"]),
    lat2 = unname(bb["ymax"])
  )
}

