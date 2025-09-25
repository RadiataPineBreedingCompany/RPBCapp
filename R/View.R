#' @export
PlantationView <- R6::R6Class("PlantationView",
  public = list(
    model = NULL,

    initialize = function(model) {
      self$model <- model
    },

    summary = function()
    {
      if (!is.null(self$model$dtm)) cat("DTM loaded\n")
      if (!is.null(self$model$chm)) cat("CHM loaded\n")
      if (!is.null(self$model$chm)) cat("sCHM loaded\n")
      if (!is.null(self$model$las)) cat("Cloud loaded\n")
      if (!is.null(self$model$layout))
      {
        if (is.null(self$model$crs))
          crsname = NA_character_
        else
          crsname = self$model$crs$Name

        cat("Project CRS: ", crsname, "\n")
        cat("Block Layout:\n")
        cat(" Num. blocks:", nrow(self$model$layout$block_layout_table), "\n")

        cat("Tree Layout\n")
        if (is.null(self$model$layout$tree_layout_raw))
        {
          cat(" Not built yet\n")
        }
        else
        {
          cat("  Starting point:",   self$model$layout$start, "\n")
          cat("  Orientation   :",   self$model$layout$orientation, "\n")
          cat("  Num. trees    :",   nrow(remove_cut_trees(self$model$layout$tree_layout_raw)), "\n")
          cat("  Tree spacing  :", self$model$layout$spacing, "\n")
        }
      }
    },

    plot_chm = function(...) {
      if (is.null(self$model$chm)) stop("No CHM available")
      terra::plot(self$model$chm, main = "Canopy Height Model", ...)
    },

    plot_trees = function(...) {
      if (is.null(self$model$trees)) stop("No trees to plot")
      plot(sf::st_geometry(self$model$trees), ...)
    },

    plot_crowns = function(...) {
      if (is.null(self$model$trees)) stop("No crown to plot")
      plot(sf::st_geometry(self$model$crowns), ...)
    },

    plot_layout = function()
    {
      block_layout = self$model$layout$block_layout_oriented
      tree_layout =  self$model$layout$tree_layout_oriented

      cols = rep("black", nrow(block_layout))
      cols[block_layout$BlockID < 0] = "gray95"
      plot(sf::st_geometry(block_layout), axes = TRUE, main = "Block and tree pattern", border = cols)

      blk = split(tree_layout, tree_layout[[BLOCKNAME]])
      lines_list <- lapply(blk, function(block) {
        sf::st_cast(sf::st_combine(block), "LINESTRING")
      })
      lines = do.call(c, lines_list)
      plot(lines, add = T, col = "gray")

      cut = is_cut_tree(tree_layout)
      noncut_layout = remove_cut_trees(tree_layout)
      cut_trees = tree_layout[cut,]

      cols <- sf::sf.colors(nlevels(as.factor(noncut_layout$Tpos)))
      cols <- cols[as.factor(noncut_layout$Tpos)]
      cols[noncut_layout[[BLOCKNAME]] < 0] = "gray"

      plot(sf::st_geometry(noncut_layout), add = T, pch = 19, cex = 0.3, col = cols)
      plot(sf::st_geometry(cut_trees), add = T, pch = 4, cex = 0.5, col = "black")

      tree_zero = sf::st_geometry(noncut_layout)[1]
      plot(tree_zero, add = T, pch = 19, cex = 1, col = "red")

      graphics::legend("topright", "Tree zero (block 1, tree 1)", pch = 19, col = "red")
    },

    leaflet = function(proxy = NULL, mapId = "", edit = NULL,
                       dtm = TRUE, chm = TRUE, schm = TRUE, bound = TRUE,
                       bbox = TRUE, trees = TRUE, crowns = TRUE, layout = TRUE)
    {
      use_proxy <- !is.null(proxy)

      if (!use_proxy) {
        map <- make_base_map("")
      } else {
        map <- leaflet::leafletProxy(mapId, session = proxy)
      }

      overlayGroups <- character()

      if (dtm) {
        res <- add_dtm_layer(map, self$model$dtm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (schm) {
        res <- add_schm_layer(map, self$model$schm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (chm) {
        res <- add_chm_layer(map, self$model$chm, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (bbox) {
        res <- add_bbox_layer(map, self$model$bbox, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (bound) {
        res <- add_boundaries_layer(map, self$model$boundaries, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (layout) {
        res <- add_block_layout_layer(map, self$model$layout, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)

        res <- add_warnings_layer(map, self$model$layout_warnings, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)

        res <- add_tree_layout_layer(map, self$model$layout, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (crowns) {
        res <- add_crowns_layer(map, self$model$crowns, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      if (trees) {
        res <- add_trees_layer(map, self$model$trees, proxy = use_proxy)
        map <- res$map
        if (!is.null(res$groups)) overlayGroups <- c(overlayGroups, res$groups)
      }

      # Add draw toolbar only for initial render
      if (!is.null(edit) && !use_proxy) {
        map <- map |> leaflet.extras::addDrawToolbar(
          targetGroup = edit,
          polylineOptions = FALSE, polygonOptions = FALSE,
          circleOptions = FALSE, rectangleOptions = FALSE,
          markerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        )
      }

      # Layers control vs. default view: only on initial render (avoid duplicate controls on proxy updates)
      if (!use_proxy) {
        if (length(overlayGroups) > 0) {
          map <- map |> leaflet::addLayersControl(
            overlayGroups = overlayGroups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
        } else {
          map <- map |> leaflet::setView(lng = 174.8, lat = -41.0, zoom = 3)
        }
      }

      if (!use_proxy) {
        map = center_on_object(map, self$model$bbox)
      }

      map
    },

    compute_tree_stats = function()
    {
      trees <- self$model$trees
      if (is.null(trees))
      {
        return(list(
          found = list(n = NA, p = NA),
          missing = list(n = NA, p = NA),
          non_measured = list(n = NA, p = NA)
        ))
      }

      N <- nrow(trees)

      n_found <- sum(trees$ApexFound)
      p_found <- round(n_found / N * 100, 1)

      n_missing <- sum(trees$ApexFound | trees$TreeFound)
      p_missing <- round((N - n_missing) / N * 100, 1)

      n_nonmeasured <- sum(!trees$ApexFound & trees$TreeFound)
      p_nonmeasured <- round(n_nonmeasured / N * 100, 1)

      list(
        found = list(n = n_found, p = p_found),
        missing = list(n = N - n_missing, p = p_missing),
        non_measured = list(n = n_nonmeasured, p = p_nonmeasured)
      )
    },

    ggstats = function()
    {
      if (is.null(self$model$trees))
        return(list())

      void = ggplot2::ggplot() + ggplot2::theme_void()
      out = vector("list", 6)
      out[[1]] = void
      out[[2]] = void
      out[[3]] = void
      out[[4]] = void
      out[[5]] = void
      out[[6]] = void

      trees = self$model$trees

      out[[1]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = Height) +
        ggplot2::geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
        ggplot2::geom_vline(
          xintercept = mean(trees$Height, na.rm = TRUE),
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        ggplot2::labs(
          title = "Height Distribution",
          x = "Height",
          y = "Count"
        ) +
        ggplot2::theme_bw()


      out[[2]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = CrownArea) +
        ggplot2::geom_histogram(binwidth = 0.5, fill = "darkgoldenrod2", color = "black") +
        ggplot2::geom_vline(
          xintercept = mean(trees$CrownArea, na.rm = TRUE),
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        ggplot2::labs(
          title = "Crown Area Distribution",
          x = "Area (m²)",
          y = "Count"
        ) +
        ggplot2::theme_bw()

      out[[3]] = ggplot2::ggplot(trees) +
        ggplot2::aes(x = .data[[BLOCKNAME]], y = Height, fill = as.factor(.data[[BLOCKNAME]])) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        ggplot2::theme_bw() +
        ggplot2::guides(fill = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::scale_x_discrete(
          breaks = levels(as.factor(trees[[BLOCKNAME]]))[seq(1, length(unique(trees[[BLOCKNAME]])), 5)]
        ) +
        ggplot2::labs(
          title = "Height Distribution by Block",
          x = "Block",
          y = "Height (m)",
          fill = "Block"
        )

      code_var <- intersect(FAMILYCODENAMES, names(trees))[1]
      if (!is.na(code_var))
      {
        out[[4]] <- ggplot2::ggplot(trees) +
          ggplot2::aes(
            x = as.factor(.data[[code_var]]),
            y = Height,
            fill = as.factor(.data[[code_var]]),
            col  = as.factor(.data[[code_var]])
          ) +
          ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
          ggplot2::theme_bw() +
          ggplot2::guides(fill = "none", col = "none") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
          ggplot2::labs(
            title = paste("Height Distribution by", code_var),
            x     = code_var,
            y     = "Height (m)",
            fill  = code_var
          )
      }

      code_var <- intersect(CLONECODENAMES, names(trees))[1]
      if (!is.na(code_var))
      {
        out[[4]] <- ggplot2::ggplot(trees) +
          ggplot2::aes(
            x = as.factor(.data[[code_var]]),
            y = Height,
            fill = as.factor(.data[[code_var]]),
            col  = as.factor(.data[[code_var]])
          ) +
          ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
          ggplot2::theme_bw() +
          ggplot2::guides(fill = "none", col = "none") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
          ggplot2::labs(
            title = paste("Height Distribution by", code_var),
            x     = code_var,
            y     = "Height (m)",
            fill  = code_var
          )
      }

      out
    },

    rgl = function(budget = 2000000, useNULL = FALSE)
    {
      las = self$model$las
      boundaries = self$model$boundaries
      dtm = self$model$dtm
      chm = self$model$chm
      bbox = self$model$bbox

      rgl::open3d(useNULL = useNULL)
      rgl::bg3d("black")

      offset = c(0,0,0)
      if (!is.null(las))
      {
        xmin = las@header[["Min X"]]
        xmax = las@header[["Max X"]]
        ymin = las@header[["Min Y"]]
        ymax = las@header[["Max Y"]]
        zmin = las@header[["Min Z"]]
        zmax = las@header[["Max Z"]]
        offset = c((xmin+xmax)/2, (ymin+ymax)/2, zmin)
      } else if (!is.null(bbox)) {
        coords <- sf::st_coordinates(bbox)
        offset = c(mean(coords[,1]), mean(coords[,2]),mean(coords[,3]))
      } else if (!is.null(boundaries)) {
        coords <- sf::st_coordinates(boundaries)
        offset = c(mean(coords[,1]), mean(coords[,2]),mean(coords[,3]))
      } else if (!is.null(chm)) {
        e = terra::ext(chm)
        z = min(terra::values(chm), na.rm = T)
        offset = c(e[1], e[3], z)
      }

      if (!is.null(bbox))
      {
        coords <- sf::st_coordinates(bbox)
        z <- rep(0, nrow(coords))
        xyz <- cbind(coords[, 1:2], z)
        xyz[,1] = xyz[,1] - offset[1]
        xyz[,2] = xyz[,2] - offset[2]
        rgl::lines3d(xyz, col = "red", lwd = 2, tag = "bbox")
      }

      if (!is.null(dtm))
      {
        dtm = terra::aggregate(dtm)
        lidR::add_dtm3d(offset[1:2], dtm-offset[3], tag = "dtm")
      }

      if (!is.null(boundaries))
      {
        coords <- sf::st_coordinates(boundaries)
        z <- rep(0, nrow(coords))
        xyz <- cbind(coords[, 1:2], z)
        xyz[,1] = xyz[,1] - offset[1]
        xyz[,2] = xyz[,2] - offset[2]
        rgl::lines3d(xyz, col = "green", lwd = 2, tag = "boundaries")
      }

      if (!is.null(las))
      {
        ndisplay = budget/2

        # Ground points
        gnd = lidR::filter_ground(las)
        n = lidR::npoints(gnd)
        if (n > ndisplay)
        {
          i = sample(1:n, ndisplay)
          i = sort(i)
          gnd = gnd[i]
        }

        # Non Ground points
        ngnd = lidR::filter_poi(las, Classification != lidR::LASGROUND)
        n = lidR::npoints(ngnd)
        if (n > ndisplay)
        {
          i = sample(1:n, ndisplay)
          i = sort(i)
          ngnd = ngnd[i]
        }

        pal = lidR::height.colors(25)
        col = lidR:::set.colors(ngnd$Z, pal)
        rgl::points3d(gnd$X-offset[1], gnd$Y-offset[2], gnd$Z-offset[3], col = "blue", size = 2, tag = "ground")
        rgl::points3d(ngnd$X-offset[1], ngnd$Y-offset[2], ngnd$Z-offset[3], col = col, size = 2, tag = "vegetation")
      }
    },

    get_object_by_index = function(i)
    {
      switch(i,
             self$model$chm,
             self$model$schm,
             self$model$boundaries,
             self$model$layout$tree_layout_oriented,
             self$model$trees,
             self$model$crowns)
    },

    state = function(DT = FALSE)
    {
      out = list(
        CHM = !is.null(self$model$chm),
        sCHM = !is.null(self$model$schm),
        Boudaries = !is.null(self$model$boundaries),
        Layout = !is.null(self$model$layout),
        Trees = !is.null(self$model$trees),
        Crowns = !is.null(self$model$crowns)
      )

      if (isTRUE(DT))
      {
        df <- data.frame(
          Layer = names(out),
          Status = unlist(out),
          stringsAsFactors = FALSE
        )

        # Replace TRUE/FALSE with HTML icons
        df$Status <- ifelse(
          df$Status,
          "<i class='fa fa-check-circle' style='color: green;'></i>",
          "<i class='fa fa-times-circle' style='color: red;'></i>"
        )

        out = DT::datatable(
          df,
          selection = "single",
          escape = FALSE,   # allow HTML icons
          rownames = FALSE,
          options = list(dom = 't', pageLength = 100)
        )
      }

      return(out)
    }
))
