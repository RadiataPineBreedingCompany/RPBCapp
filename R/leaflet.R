# ---- base map ----

#' @export
make_base_map <- function(groups = c())
{
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
.add_raster_layer <- function(map, rast, group, palette, preprocess = NULL, proxy = FALSE)
{
  if (is.null(rast))
    return(list(map = map, groups = NULL))

  # check range validity
  if (anyNA(terra::minmax(rast)))
  {
    warning(sprintf("Cannot determine the range value of the %s. %s is corrupted", group, group))
    return(list(map = map, groups = NULL))
  }

  # optional preprocessing (e.g., hillshade)
  if (!is.null(preprocess))
  {
    rast <- preprocess(rast)
  }

  # ensure raster is on disk
  if (terra::inMemory(rast))
  {
    o <- tempfile(fileext = ".tif")
    terra::writeRaster(rast, o, overwrite = TRUE)
    rast <- terra::rast(o)
  }

  file <- terra::sources(rast)

  # clear previous group (for proxy updates)
  if (proxy) map <- leaflet::clearGroup(map, group)

  # add to map
  map <- leafem::addGeotiff(
    map,
    file,
    colorOptions = leafem::colorOptions(
      palette = palette,
      na.color = "transparent"
    ),
    resolution = 128,
    group = group,
    autozoom = FALSE
  )

  list(map = map, groups = group)
}

# ---- specific layers ----

#' @export
add_dtm_layer <- function(map, dtm, proxy = FALSE) {
  hillshade_fn <- function(dtm) {
    dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
    hs <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
    terra::stretch(hs, minq = 0.02, maxq = 0.98)
  }

  .add_raster_layer(
    map = map,
    rast = dtm,
    group = "DTM",
    palette = grDevices::gray.colors(50, 0, 1),
    preprocess = hillshade_fn,
    proxy = proxy
  )
}

#' @export
add_schm_layer <- function(map, schm, proxy = FALSE) {
  .add_raster_layer(
    map = map,
    rast = schm,
    group = "sCHM",
    palette = lidR::height.colors(20),
    proxy = proxy
  )
}

#' @export
add_chm_layer <- function(map, chm, proxy = FALSE) {
  .add_raster_layer(
    map = map,
    rast = chm,
    group = "CHM",
    palette = lidR::height.colors(20),
    proxy = proxy
  )
}

# ---- vector layers ----
#' @export
add_bbox_layer <- function(map, bbox, proxy = FALSE)
{
  if (is.null(bbox)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(bbox, 4326)
  if (proxy) map <- map |> leaflet::clearGroup("Bounding box")
  map <- map |> leaflet::addPolygons(data = data, group = "Bounding box", color = "red", fill = FALSE)
  list(map = map, groups = "Bounding box")
}

#' @export
add_boundaries_layer <- function(map, boundaries, proxy = FALSE)
{
  if (is.null(boundaries)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(boundaries, 4326)
  if (proxy) map <- map |> leaflet::clearGroup("Boundaries")
  map <- map |> leaflet::addPolygons(data = data, group = "Boundaries", color = "green", fill = FALSE)
  list(map = map, groups = "Boundaries")
}

#' @export
add_block_layout_layer <- function(map, layout, proxy = FALSE)
{
  if (is.null(layout) || is.null(layout$block_layout_oriented)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(layout$block_layout_oriented, 4326)
  data <- data[data$BlockID > 0, ]
  if (nrow(data) == 0) return(list(map = map, groups = NULL))
  if (proxy) map <- map |> leaflet::clearGroup("Block layout")
  map <- map |> leaflet::addPolygons(data = data, group = "Block layout", color = "black", fill = FALSE, weight = 3)
  list(map = map, groups = "Block layout")
}

#' @export
add_warnings_layer <- function(map, layout_warnings, proxy = FALSE)
{
  if (is.null(layout_warnings)) return(list(map = map, groups = NULL))
  added <- character()

  if (proxy)
  {
    map <- map |> leaflet::clearGroup("Move")
    map <- map |> leaflet::clearGroup("Warnings")
    map <- map |> leaflet::removeControl("warnings_legend")
  }

  # Move lines
  if (!is.null(layout_warnings$move))
  {
    data_move <- sf::st_transform(layout_warnings$move, 4326)

    map <- map |> leaflet::addPolylines(
      data = data_move,
      group = "Move",
      color = "white",
      weight = 2,
      opacity = 0.9)
    added <- c(added, "Move")
  }

  # Warning polygons (with legend on initial render)
  if (!is.null(layout_warnings$warn))
  {
    data_warn <- sf::st_transform(layout_warnings$warn, 4326)
    reason_vals <- data_warn$reason
    pal <- leaflet::colorFactor(palette = c("purple", "red"), domain = reason_vals)

    map <- map |> leaflet::addPolygons(
      data = data_warn,
      opacity = 0.9,
      group = "Warnings",
      color = ~pal(reason),
      fill = FALSE,
      weight = 3)

    added <- c(added, "Warnings")

    map <- map |> leaflet::addLegend(
      position = "bottomleft",
      pal = pal,
      values = reason_vals,
      title = "Warnings",
      layerId = "warnings_legend"
    )
  }

  if (length(added) == 0) list(map = map, groups = NULL) else list(map = map, groups = added)
}

#' @export
add_crowns_layer <- function(map, crowns, proxy = FALSE)
{
  if (is.null(crowns)) return(list(map = map, groups = NULL))
  data <- sf::st_transform(crowns, 4326)
  data <- data[!sf::st_is_empty(data), ]
  if (nrow(data) == 0) return(list(map = map, groups = NULL))
  if (proxy) map <- map |> leaflet::clearGroup("Crowns")
  map <- map |> leaflet::addPolygons(data = data, color = "gray", fill = FALSE, opacity = 0.5, weight = 1, group = "Crowns")
  list(map = map, groups = "Crowns")
}

#' @export
add_tree_layout_layer <- function(map, layout, proxy = FALSE)
{
  if (is.null(layout)) return(list(map = map, groups = NULL))

  if (!is.null(layout$tree_layout_adjusted)) {
    data <- sf::st_transform(layout$tree_layout_adjusted, 4326)
  } else if (!is.null(layout$tree_layout_oriented)) {
    data <- sf::st_transform(layout$tree_layout_oriented, 4326)
  } else {
    return(list(map = map, groups = NULL))
  }

  data <- remove_cut_trees(data)
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

# ==== Clear ====

#' @export
clear_group = function(map, group)
{
  map <- map |> leaflet::clearGroup(group)
}

