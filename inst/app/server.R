library(shiny)
library(RPBCapp)

cat("Loading Server...\n")

popup_error = function(msg)
{
  showModal(
    modalDialog(
      div(
        style = "display: flex; align-items: center;",
        span(icon("exclamation-triangle"), style = "color: #a94442; font-size: 24px; margin-right: 10px;"),
        span("Error", style = "color: #a94442; font-weight: bold; font-size: 20px;")
      ),
      tags$div(msg, style = "margin-top: 10px; color: #333;"),
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
      size = "m"
    )
  )
}

popup_warning = function(msg)
{
  showModal(
    modalDialog(
      div(
        style = "display: flex; align-items: center;",
        span(icon("exclamation-circle"), style = "color: #8a6d3b; font-size: 24px; margin-right: 10px;"),
        span("Warning", style = "color: #8a6d3b; font-weight: bold; font-size: 20px;")
      ),
      tags$div(msg, style = "margin-top: 10px; color: #333;"),
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
      size = "m"
    )
  )
}

safe_run <- function(expr, catch_warnings = FALSE)
{
  if (catch_warnings)
  {
    tryCatch(
      withCallingHandlers(
        expr,
        warning = function(w) {
          popup_warning(conditionMessage(w))
          invokeRestart("muffleWarning")  # continue after warning
        }
      ),
      error = function(e) {
        popup_error(conditionMessage(e))
        cat(paste0("Error in ", deparse(conditionCall(e)), ": ", conditionMessage(e)), "\n")
        NULL
      }
    )
  }
  else
  {
    tryCatch(
      expr,
      error = function(e) {
        popup_error(conditionMessage(e))
        cat(paste0("Error in ", deparse(conditionCall(e)), ": ", conditionMessage(e)), "\n")
        NULL
      }
    )
  }
}

show_notification = function(msg)
{
  cat(msg, "\n")
  showNotification(msg)
}

short_path <- function(paths, max_chars = 40)
{
  sapply(paths, function(p) {
    if (is.na(p)) return(NA)
    # Short path is ok as is
    if (nchar(p) <= max_chars) return(p)

    comps <- strsplit(p, .Platform$file.sep)[[1]]
    n <- length(comps)
    if (n <= 2) return(p)

    first <- paste(comps[1:(n-1)], collapse = .Platform$file.sep) # keep most of start
    last <- comps[n]  # filename
    # Truncate first if still too long
    total_len <- nchar(first) + nchar(last) + nchar("[...]") + 2  # 2 for separators
    if (total_len > max_chars) {
      # keep only first N chars from start
      keep <- max_chars - nchar(last) - nchar("[...]") - 2
      first <- substr(first, 1, keep)
    }

    paste0(first, .Platform$file.sep, "[...]", .Platform$file.sep, last)
  }, USE.NAMES = FALSE)
}

selectCRSModalDialog = function()
{
  modalDialog(
    title =  tagList(
      tags$span(
        icon("exclamation-triangle", lib = "font-awesome"),
        "No CRS recorded in the point cloud",
        style = "color: red;"
      )
    ),
    selectInput(
      "choose_crs",
      "Choose CRS:",
      choices = c(
        # --- New Zealand ---
        "NZGD2000 / New Zealand Transverse Mercator 2000 (EPSG:2193)" = "EPSG:2193",
        "NZGD2000 / Chatham Islands Transverse Mercator 2000 (EPSG:3793)" = "EPSG:3793",
        "NZGD49 / New Zealand Map Grid (NZMG) (EPSG:27200)" = "EPSG:27200",

        # --- Australia (GDA2020 MGA Zones 49–56S) ---
        "GDA2020 / MGA Zone 49 (EPSG:7859)" = "EPSG:7859",
        "GDA2020 / MGA Zone 50 (EPSG:7860)" = "EPSG:7860",
        "GDA2020 / MGA Zone 51 (EPSG:7861)" = "EPSG:7861",
        "GDA2020 / MGA Zone 52 (EPSG:7862)" = "EPSG:7862",
        "GDA2020 / MGA Zone 53 (EPSG:7863)" = "EPSG:7863",
        "GDA2020 / MGA Zone 54 (EPSG:7864)" = "EPSG:7864",
        "GDA2020 / MGA Zone 55 (EPSG:7865)" = "EPSG:7865",
        "GDA2020 / MGA Zone 56 (EPSG:7866)" = "EPSG:7866",

        # --- Australia (GDA94 MGA Zones 49–56S, still in use) ---
        "GDA94 / MGA Zone 49 (EPSG:28349)" = "EPSG:28349",
        "GDA94 / MGA Zone 50 (EPSG:28350)" = "EPSG:28350",
        "GDA94 / MGA Zone 51 (EPSG:28351)" = "EPSG:28351",
        "GDA94 / MGA Zone 52 (EPSG:28352)" = "EPSG:28352",
        "GDA94 / MGA Zone 53 (EPSG:28353)" = "EPSG:28353",
        "GDA94 / MGA Zone 54 (EPSG:28354)" = "EPSG:28354",
        "GDA94 / MGA Zone 55 (EPSG:28355)" = "EPSG:28355",
        "GDA94 / MGA Zone 56 (EPSG:28356)" = "EPSG:28356"
      ),
      width = "100%"
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_crs", "OK")
    )
  )
}

server <- function(input, output, session)
{
  plantation_model <- PlantationModel$new()
  plantation <- PlantationController$new(plantation_model)
  plantation_view <- PlantationView$new(plantation_model)

  update_bbox_in_maps   <- reactiveVal(0)
  update_bound_in_maps  <- reactiveVal(0)
  update_layout_in_maps <- reactiveVal(0)
  update_chm_in_maps    <- reactiveVal(0)
  update_schm_in_maps   <- reactiveVal(0)
  update_dtm_in_maps    <- reactiveVal(0)
  update_trees_in_maps  <- reactiveVal(0)
  update_debug_in_maps  <- reactiveVal(0)

  update_stats_ui       <- reactiveVal(0)

  update_layout_plot <- reactiveVal(0)
  update_file_table  <- reactiveVal(0)
  update_state_table <- reactiveVal(0)
  update_rgl_view    <- reactiveVal(0)
  saveProject        <- reactiveVal(0)

  output$estimatedDensityValue = renderText(NA)
  output$recommandedFractionValue = renderText(NA)

  volumes <- c(RPBC = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/", Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "loadLasFileButton", roots = volumes, filetypes = c('las', "laz"))
  shinyFiles::shinyFileChoose(input, "loadCHMFileButton", roots = volumes, filetypes = c('tif', 'tiff'))
  shinyFiles::shinyFileChoose(input, "loadConfigFileButton", roots = volumes, filetypes = c('rpbc'))
  shinyFiles::shinyFileChoose(input, "loadBoundaryFileButton", roots = volumes, filetypes = c('shp', "gpkg"))
  shinyFiles::shinyFileChoose(input, "loadTreeMapFileButton", roots = volumes, filetypes = c('shp', "gpkg"))
  shinyFiles::shinyFileChoose(input, "loadBlockPatternFileButton", roots = volumes, filetypes = c('xls', 'xlsx'))
  shinyFiles::shinyFileSave(input, "createProjectButton", roots = volumes)

  # ===== Init Map ====

  output$mapTreeLayout <- leaflet::renderLeaflet({
    make_base_map(c("CHM", "sCHM", "Boundaries", "Block layout", "Move", "Warnings", "Tree layout")) |>
      leaflet.extras::addDrawToolbar(
        targetGroup = "Tree layout",
        polylineOptions = FALSE, polygonOptions = FALSE,
        circleOptions = FALSE, rectangleOptions = FALSE,
        markerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions()
      )
  })
  output$mapPreview <- leaflet::renderLeaflet({
    make_base_map(c("DTM", "CHM", "sCHM", "Boundaries", "Block layout", "Tree layout", "Bounding box"))
  })
  output$mapCHM <- leaflet::renderLeaflet({
    make_base_map(c("DTM", "CHM", "sCHM", "Boundaries"))
  })
  output$mapTrees <- leaflet::renderLeaflet({
    make_base_map(c("CHM", "sCHM", "Boundaries", "Block layout", "Trees", "Crowns"))
  })

  outputOptions(output, "mapTreeLayout", suspendWhenHidden = FALSE)
  outputOptions(output, "mapPreview", suspendWhenHidden = FALSE)
  outputOptions(output, "mapCHM", suspendWhenHidden = FALSE)
  outputOptions(output, "mapTrees", suspendWhenHidden = FALSE)

  # ===== OnClick Create Project Button ====

  observeEvent(input$createProjectButton, {

    file <- shinyFiles::parseSavePath(volumes, input$createProjectButton)$datapath

    if (!is.null(file) && length(file) > 0)
    {
      show_notification("Creation of a project")

      safe_run({
        plantation$reset()
        plantation$create_config(file)
      })

      update_file_table(runif(1))
      update_state_table(runif(1))
      update_stats_ui(runif(1))

      update_bbox_in_maps(runif(1))
      update_bound_in_maps(runif(1))
      update_layout_in_maps(runif(1))
      update_chm_in_maps(runif(1))
      update_schm_in_maps(runif(1))
      update_dtm_in_maps(runif(1))
      update_trees_in_maps(runif(1))
      update_debug_in_maps(runif(1))
    }

  }, ignoreInit = TRUE)

  # ===== OnClick Load Config File Button ====

  observeEvent(input$loadConfigFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadConfigFileButton)$datapath

    if (is.null(file) || (length(file) == 0))
      return(NULL)

    show_notification("Loading project")

    safe_run({
      plantation$reset()
      plantation$read_config(file)
    }, catch_warnings = TRUE)

    cat("Update UI sliders\n")

    if (!is.null(plantation$layout))
    {
      x_size = plantation$layout$block_size[1]
      y_size = x_size
      if (length(plantation$layout$block_size) == 2)
        y_size = plantation$layout$block_size[2]

      updateNumericInput(session, "blockSizeInputX",  value = x_size)
      updateNumericInput(session, "blockSizeInputY",  value = y_size)
      updateNumericInput(session, "treeNumberInput", value = plantation$model$layout$num_trees)
      updateRadioButtons(session, "partternStartChoiceRadioButton", selected = plantation$model$layout$start)
      updateRadioButtons(session, "partternOrientationChoiceRadioButton", selected = plantation$model$layout$orientation)
    }

    updateSliderInput(session, "smoothCHM", value = plantation$model$params$smoothCHM)
    updateSliderInput(session, "smoothPasses", value = plantation$model$params$smoothPasses)
    updateSliderInput(session, "hminAdjustTreesSlider", value = plantation$model$params$treesHmin)
    updateSliderInput(session, "hminMeasureTreesSlider", value = plantation$model$params$crownsHmin)
    updateSliderInput(session, "keepRandomFraction", value = plantation$model$params$keepRandomFraction)
    updateSliderInput(session, "rigidness", value = plantation$model$params$rigidness)
    updateSliderInput(session, "cloth_resolution", value = plantation$model$params$cloth_resolution)
    updateSliderInput(session, "res", value = plantation$model$params$resCHM)

    cat("Estimate density\n")

    if (!is.null(plantation$flas))
    {
      header = lidR::readLASheader(plantation$flas)
      d = round(lidR::density(header),1)
      f = round((200/d)/0.05)*0.05
      if (f > 1) f = 1
      output$estimatedDensityValue = renderText(d)
      output$recommandedFractionValue = renderText(f)
      if (is.null(plantation$model$params$keepRandomFraction))
        plantation$model$params$keepRandomFraction = f
    }

    cat("Trigger UI update\n")

    update_file_table(runif(1))
    update_state_table(runif(1))
    update_stats_ui(runif(1))

    update_bbox_in_maps(runif(1))
    update_bound_in_maps(runif(1))
    update_layout_in_maps(runif(1))
    update_chm_in_maps(runif(1))
    update_schm_in_maps(runif(1))
    update_dtm_in_maps(runif(1))
    update_trees_in_maps(runif(1))
    update_debug_in_maps(runif(1))

    update_rgl_view(runif(1))

    cat("Project loaded\n")

  }, ignoreInit = TRUE)

  # ===== OnClick Load LAS File Button ====

  observeEvent(input$loadLasFileButton, {

    path <- shinyFiles::parseFilePaths(volumes, input$loadLasFileButton)$datapath

    if (is.null(path) || length(path) == 0) return(NULL)

    safe_run({
      plantation$set_cloud(path)

      # We set the point cloud.
      # This should have set the CRS except if the LAS file has no CRS
      if (is.na(plantation$get_crs()))
      {
        showModal(selectCRSModalDialog())
      }

      if (!is.null(plantation$flas))
      {
        header = lidR::readLASheader(plantation$flas)
        d = round(lidR::density(header),1)
        f = round((200/d)/0.05)*0.05
        if (f > 1) f = 1
        output$estimatedDensityValue = renderText(d)
        output$recommandedFractionValue = renderText(f)
        updateSliderInput(session, "keepRandomFraction", value = f)
      }

      plantation$save()

      update_bbox_in_maps(runif(1))
      update_state_table(runif(1))
      update_file_table(runif(1))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick Load Block Pattern File Button ====

  observeEvent(input$loadBlockPatternFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadBlockPatternFileButton)$datapath

    if (is.null(file) || length(file) == 0)
      return(NULL)

    show_notification("Loading Excel database")

    safe_run({
      plantation$set_database(file)
      plantation$set_layout_parameter(
        c(input$blockSizeInputX, input$blockSizeInputY),
        input$treeNumberInput,
        input$partternStartChoiceRadioButton,
        input$partternOrientationChoiceRadioButton)
      plantation$save()

      update_bound_in_maps(runif(1))
      update_layout_in_maps(runif(1))
      update_file_table(runif(1))
      update_state_table(runif(1))
      update_layout_plot(runif(1))
    }, catch_warnings = TRUE)
  }, ignoreInit = TRUE)

  # ===== OnClick Load Boundary File Button ====
  observeEvent(input$loadBoundaryFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadBoundaryFileButton)$datapath

    if (is.null(file) || length(file) == 0)
      return(NULL)

    show_notification("Loading boundaries polygon")

    safe_run({
      plantation$set_boundaries(file)
      plantation$save()
      update_bound_in_maps(runif(1))
      update_file_table(runif(1))
      update_state_table(runif(1))
    })
  })

  # ===== OnClick Load CHM File Button ====

  observeEvent(input$loadCHMFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadCHMFileButton)$datapath

    show_notification("Loading CHM")

    safe_run({
      plantation$set_chm(file)
      plantation$save()
      update_chm_in_maps(runif(1))
      update_file_table(runif(1))
      update_state_table(runif(1))
    })
  })

  # ===== OnClick Load Tree Map File Button ====

  observeEvent(input$loadTreeMapFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadTreeMapFileButton)$datapath

    show_notification("Loading tree plantation design")

    safe_run({
      plantation$set_layout(file)
      plantation$save()
      update_file_table(runif(1))
      update_state_table(runif(1))
      update_layout_in_maps(runif(1))
    })
  })

  # ===== OnClick smooth CHM Button ====

  observeEvent(input$smoothCHMButton, {

    safe_run({
      show_notification("Smoothing CHM")
      plantation$smooth_chm(input$smoothCHM, input$smoothPasses)
      plantation$save()
      update_schm_in_maps(runif(1))
      update_file_table(runif(1))
    })
  })



  # ===== OnClick Optim Block Button ====

  observeEvent(input$optimBlockButton, {

    show_notification("Optim layout by block")

    safe_run({
      withProgress(message = 'Tree detection', value = 0, {
        plantation$optim_layout(incProgress)
        plantation$save()
      })
      update_layout_in_maps(runif(1))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick Align Layout Button ====

  observeEvent(input$alignLayoutButton, {

    show_notification("Aligning tree layout")

    safe_run({
      withProgress(message = 'Tree detection', value = 0, {
        plantation$align_layout(incProgress)
        plantation$save()
      })
      update_layout_in_maps(runif(1))
      update_state_table(runif(1))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick Adjust Layout Button ====

  observeEvent(input$adjustLayoutButton, {

    hmin = isolate(input$hminAdjustTreesSlider)

    if (is.null(hmin)) {
      popup_error("Internal error: hmin = NULL")
      return(NULL)
    }

    safe_run({
      withProgress(message = 'Tree detection', value = 0, {
        plantation$adjust_layout(hmin, progress = incProgress)
        plantation$save()
      })
      update_layout_in_maps(runif(1))
      update_debug_in_maps(runif(1))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick Run Measurement Button ====

  observeEvent(input$runMeasurementButton, {

    hmin = isolate(input$hminMeasureTreesSlider)

    if (is.null(hmin)) {
      popup_error("Internal error: hmin = NULL")
      return(NULL)
    }

    show_notification("Measuring trees")

    safe_run({
      if (!plantation$model$is_adjusted())
        stop("Tree location not found yet. Please run tree localisation first (tab 4)")

      withProgress(message = 'Tree measurement', value = 0, {
        plantation$measure_trees(hmin, progress = incProgress)
        plantation$save()
      })

      update_file_table(runif(1))
      update_trees_in_maps(runif(1))
      update_state_table(runif(1))
      update_stats_ui(runif(1))
    })
  }, ignoreInit = TRUE)

  # ===== On Edited Feature ======

  observeEvent(input$mapTreeLayout_draw_edited_features, {

    show_notification("SVD alignment")

    edits <- input$mapTreeLayout_draw_edited_features

    if (is.null(edits))
      return(NULL)

    safe_run({
      geojson <- jsonlite::toJSON(edits, auto_unbox = TRUE, digits = 8)
      edited  <- geojsonsf::geojson_sf(geojson)
      edited  <- sf::st_transform(edited, plantation$crs)

      local <- RPBCapp:::remove_virtual_trees(plantation$layout$tree_layout_raw)
      local <- local[edited$layerId,]

      Mlocal  <- sf::st_coordinates(local)
      Mglobal <- sf::st_coordinates(edited)

      M <- layout_alignment_svd(Mlocal, Mglobal)
      plantation$layout$set_matrix(M)
      plantation$save()

      update_layout_in_maps(runif(1))
    })
  })

  # ===== OnClick Process PointCloud Button ====

  observeEvent(input$processPointCloudButton, {

    safe_run({
      withProgress(message = 'Processing', value = 0, {

        if (is.null(plantation))
          stop("Uninitialized plantation.")

        plantation$process_pointcloud(
          input$keepRandomFraction,
          input$rigidness,
          cloth_resolution = input$cloth_resolution,
          smoothCHM = input$smoothCHM,
          smoothPasses = input$smoothPasses,
          progress = incProgress)
        plantation$save()

        update_dtm_in_maps(runif(1))
        update_chm_in_maps(runif(1))
        update_schm_in_maps(runif(1))
        update_rgl_view(runif(1))
      })
    })

    update_file_table(runif(1))
  })

  # ===== OnChange Parttern Choice Radio Button ====

  output$selectedPatternImage <- renderPlot({

    coords = generate_snake_coords(
      input$treeNumberInput, 0, 0, 1, 1,
      input$partternOrientationChoiceRadioButton,
      input$partternStartChoiceRadioButton)

    oldpar <- par(mar = c(2, 2, 1, 1))
    plot(coords$x, coords$y,
         asp = 1,
         type = "b",
         xaxt = "n",           # no x axis
         yaxt = "n",           # no y axis
         xlab = "",            # no x label
         ylab = "",            # no y label
         bty = "n"             # no box around plot
    )

    text(coords$x+0.01, coords$y+0.01, labels = seq_len(nrow(coords)), cex = 0.6, col = "blue")
    par(oldpar)  # reset margins to previous values
  })

  # ===== OnChange any tree pattern inputs ====

  observeEvent(
    list(input$blockSizeInputX,
         input$blockSizeInputY,
         input$treeNumberInput,
         input$partternStartChoiceRadioButton,
         input$partternOrientationChoiceRadioButton),
    {

      if (!plantation$has_layout()) return(NULL)

      plantation$set_layout_parameter(
        c(input$blockSizeInputX,input$blockSizeInputY),
        input$treeNumberInput,
        input$partternStartChoiceRadioButton,
        input$partternOrientationChoiceRadioButton)

      plantation$save()

      update_layout_plot(runif(1))
      update_layout_in_maps(runif(1))
    },
    ignoreInit = TRUE
  )

  # ===== OnClick CRS Modal Windows Ok ====

  observeEvent(input$confirm_crs, {
    req(input$choose_crs)
    plantation$set_crs(sf::st_crs(input$choose_crs))
    plantation$save()
    removeModal()
    update_preview_map(runif(1))
  })

  # ===== OnEvent save ====

  observe({
    val <- saveProject()
    if (val == 0) return(NULL)
    safe({ plantation$save() })
  })


  # ==== OnEvent Reload ====

  observeEvent(input$reload, {
    show_notification("Refreshing app")

    update_file_table(runif(1))
    update_state_table(runif(1))

    update_dtm_in_maps(runif(1))
    update_chm_in_maps(runif(1))
    update_schm_in_maps(runif(1))
    update_layout_in_maps(runif(1))
    update_trees_in_maps(runif(1))

    update_rgl_view(runif(1))

    update_stats_ui(runif(1))
    update_ggplots(runif(1))
  })


  # ===== OnClick tree Zero =====

  output$clickTreeZeroInfo <- renderUI({
    cat("Tree zero clicked\n")

    click <- input$mapTreeLayout_click
    if (is.null(click)) return("Click on the map")

    p <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    p <- sf::st_transform(p, plantation$get_crs())
    coords <- as.numeric(sf::st_coordinates(p))

    if (!plantation$has_layout())
    {
      popup_error("Clicked but no block layout was loaded yet")
      return(NULL)
    }

    safe_run({
      plantation$set_origin(coords[1], coords[2])
    })

    HTML(sprintf("X: %.2f<br/>Y: %.2f", coords[1], coords[2]))
  })

  # ===== update maps ====

  observeEvent(update_bbox_in_maps(), {
    # Update mapPreview with bbox layer
    proxy <- leaflet::leafletProxy("mapPreview")
    add_bbox_layer(proxy, plantation$model$bbox, proxy = TRUE)
    center_on_object(proxy, plantation$model$bbox)

    # Just center the other maps (no new layer)
    for (map_id in c("mapCHM", "mapTrees", "mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      center_on_object(proxy, plantation$model$bbox)
    }
  })

  observeEvent(update_bound_in_maps(), {
    for (map_id in c("mapPreview", "mapCHM", "mapTrees", "mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_boundaries_layer(proxy, plantation$model$boundaries, proxy = TRUE)
    }
  })

  observeEvent(update_layout_in_maps(), {
    for (map_id in c("mapPreview", "mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_block_layout_layer(proxy, plantation$model$layout, proxy = TRUE)
      add_tree_layout_layer(proxy, plantation$model$layout, proxy = TRUE)
    }
  })

  observeEvent(update_chm_in_maps(), {
    for (map_id in c("mapPreview", "mapCHM", "mapTrees", "mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_chm_layer(proxy, plantation$model$chm, proxy = TRUE)
    }
  })

  observeEvent(update_schm_in_maps(), {
    for (map_id in c("mapPreview", "mapCHM", "mapTrees", "mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_schm_layer(proxy, plantation$model$schm, proxy = TRUE)
    }
  })

  observeEvent(update_schm_in_maps(), {
    for (map_id in c("mapPreview", "mapCHM")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_dtm_layer(proxy, plantation$model$dtm, proxy = TRUE)
    }
  })

  observeEvent(update_trees_in_maps(), {
    for (map_id in c("mapTrees")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_trees_layer(proxy, plantation$model$trees, proxy = TRUE)
      add_crowns_layer(proxy, plantation$model$crowns, proxy = TRUE)
    }
  })

  observeEvent(update_debug_in_maps(), {
    for (map_id in c("mapTreeLayout")) {
      proxy <- leaflet::leafletProxy(map_id)
      add_warnings_layer(proxy, plantation$model$layout_warnings, proxy = TRUE)
      add_crowns_layer(proxy, plantation$model$crowns, proxy = TRUE)
    }
  })

  # ===== update table ====

  output$fileTable <- DT::renderDT({
    update_file_table()
    files_df = plantation$get_file_table()
    files_df$Path = short_path(files_df$Path, 50)
    files_df = na.omit(files_df)
    DT::datatable(
      files_df,
      selection = "single",
      escape = FALSE,   # allow HTML icons
      rownames = FALSE,
      options = list(dom = 't', pageLength = 100)
    )
  })

  # Modal to show full path when row clicked
  observeEvent(input$fileTable_rows_selected, {
    selected_row <- input$fileTable_rows_selected
    if (length(selected_row) == 0) return()

    files_df <- plantation$get_file_table()
    full_path <- files_df$Path[selected_row]

    cat("Showing:", full_path, "\n")

    showModal(modalDialog(
      title = "Full path",
      div(
        style = "width: 90vw; max-height: 200px; overflow: auto;",
        p(full_path)
      ),
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      fade = FALSE
    ))
  })

  output$stateTable <- DT::renderDT({
    update_state_table()
    plantation_view$state(DT = TRUE)
  })

  observeEvent(input$stateTable_rows_selected, {

    selected_row <- input$stateTable_rows_selected
    if (length(selected_row) == 0) return(NULL)

    out = plantation_view$get_object_by_index(selected_row)

    # For SpatRaster or sf objects
    if (inherits(out, "SpatRaster") | inherits(out, "sfc") | inherits(out, "sf"))
    {
      showModal(modalDialog(
        title = paste(class(out)[1], "object"),
        verbatimTextOutput("modalText"),
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        fade = FALSE
      ))

      output$modalText <- renderPrint({ print(out) })
    }
    else if (is.data.frame(out))
    {
      showModal(modalDialog(
        title = paste("Data Frame"),
        div(
          style = "width: 90vw; height: 80vh; overflow: auto;",  # <-- bigger modal
          DT::DTOutput("modalsfTable")
        ),
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        fade = FALSE
      ))

      output$modalsfTable <- DT::renderDT({
        out
      }, options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollY = "70vh",  # use viewport height to make it responsive
        autoWidth = TRUE
      ))
    }
    else
    {
      showModal(modalDialog(
        title = "Unsupported type",
        paste("Cannot display object of class:", class(out)),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  # ==== update plot ====

  output$plantationLayoutImage <- renderPlot({
    if (!plantation$has_tree_map()) {
      validate(FALSE, "No block layout file selected yet")
    }
    update_layout_plot()
    plantation_view$plot_layout()
  })

  output$rglplot3d <- rgl::renderRglwidget({
    update_rgl_view()
    show_notification("Rendering 3D scene")
    plantation_view$rgl(TRUE)
    u = rgl::rglwidget()
    u |> rgl::toggleWidget(tags = "bbox") |>
      rgl::toggleWidget(tags = "boundaries") |>
      rgl::toggleWidget(tags = "ground") |>
      rgl::toggleWidget(tags = "vegetation")
    u
  })


  # ==== update stats ui ====

  observe({
    update_stats_ui()

    stats <- plantation_view$compute_tree_stats()

    output$vb_found <- shiny::renderUI({
      value_box(
        title = "Trees Found",
        value = paste0(stats$found$n, " (", stats$found$p, "%)"),
        theme_color = "success"
      )
    })

    output$vb_missing <- shiny::renderUI({
      value_box(
        title = "Missing Trees",
        value = paste0(stats$missing$n, " (", stats$missing$p, "%)"),
        theme_color = "primary"
      )
    })

    output$vb_non_measured <- shiny::renderUI({
      value_box(
        title = "Non-measured Trees",
        value = paste0(stats$non_measured$n, " (", stats$non_measured$p, "%)"),
        theme_color = "danger"
      )
    })

    gglist = plantation_view$ggstats()

    for (i in 1:6)
    {
      local({
        j <- i
        output[[paste0("ggplot", j)]] <- shiny::renderPlot({
          if (j <= length(gglist)) {
            gglist[[j]]
          } else {
            # return blank if not enough plots
            ggplot2::ggplot() + ggplot2::theme_void()
          }
        })
      })
    }
  })
}
