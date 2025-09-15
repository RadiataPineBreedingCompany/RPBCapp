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

short_path <- function(paths, max_chars = 40) {
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
  plantation = Plantation$new()

  update_layout_map  <- reactiveVal(0)
  update_tree_map    <- reactiveVal(0)
  update_preview_map <- reactiveVal(0)
  update_chm_map     <- reactiveVal(0)
  update_layout_plot <- reactiveVal(0)
  update_file_table  <- reactiveVal(0)
  update_state_table <- reactiveVal(0)
  update_stats_ui    <- reactiveVal(0)
  update_rgl_view    <- reactiveVal(0)
  update_ggplots     <- reactiveVal(0)
  saveProject        <- reactiveVal(0)

  output$estimatedDensityValue = renderText(NA)
  output$recommandedFractionValue = renderText(NA)

  volumes <- c(RPBC = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/",
               Home = fs::path_home(),
               shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "loadLasFileButton", roots = volumes, filetypes = c('las', "laz"))
  shinyFiles::shinyFileChoose(input, "loadCHMFileButton", roots = volumes, filetypes = c('tif', 'tiff'))
  shinyFiles::shinyFileChoose(input, "loadConfigFileButton", roots = volumes, filetypes = c('rpbc'))
  shinyFiles::shinyFileChoose(input, "loadBoundaryFileButton", roots = volumes, filetypes = c('shp', "gpkg"))
  shinyFiles::shinyFileChoose(input, "loadTreeMapFileButton", roots = volumes, filetypes = c('shp', "gpkg"))
  shinyFiles::shinyFileChoose(input, "loadBlockPatternFileButton", roots = volumes, filetypes = c('xls', 'xlsx'))
  shinyFiles::shinyFileSave(input, "createProjectButton", roots = volumes)

  # ===== Init Map ====
  output$mapTreeLayout <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 174.8, lat = -41.0, zoom = 5)
  })

  output$mapPreview <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 174.8, lat = -41.0, zoom = 5)
  })

  # ===== OnClick createProjectButton ====
  observeEvent(input$createProjectButton, {

    file <- shinyFiles::parseSavePath(volumes, input$createProjectButton)$datapath
    print(file)

    if (!is.null(file) && length(file) > 0)
    {
      showNotification("Creation of a project")

      tryCatch({
        plantation$create_config(file)
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
      update_file_table(runif(1))
      update_state_table(runif(1))
    }

  }, ignoreInit = TRUE)

  # ===== OnClick loadConfigFileButton ====
  observeEvent(input$loadConfigFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadConfigFileButton)$datapath

    if (!is.null(file) && length(file) > 0)
    {
      showNotification("Loading project")

      tryCatch({
        plantation$read_config(file)

        if (!is.null(plantation$layout))
        {
          updateNumericInput(session, "blockSizeInput",  value = plantation$layout$block_size)
          updateNumericInput(session, "treeNumberInput", value = plantation$layout$num_trees)
          updateRadioButtons(session, "partternStartChoiceRadioButton", selected = plantation$layout$start)
          updateRadioButtons(session, "partternOrientationChoiceRadioButton", selected = plantation$layout$orientation)
        }

        updateSliderInput(session, "smoothCHM", value = plantation$params$smoothCHM)
        updateSliderInput(session, "smoothPasses", value = plantation$params$smoothPasses)
        updateSliderInput(session, "hminAdjustTreesSlider", value = plantation$params$treesHmin)
        updateSliderInput(session, "hminMeasureTreesSlider", value = plantation$params$crownsHmin)
        updateSliderInput(session, "keepRandomFraction", value = plantation$params$keepRandomFraction)
        updateSliderInput(session, "rigidness", value = plantation$params$rigidness)
        updateSliderInput(session, "cloth_resolution", value = plantation$params$cloth_resolution)
        updateSliderInput(session, "res", value = plantation$params$resCHM)

        if (!is.null(plantation$flas))
        {
          header = lidR::readLASheader(plantation$flas)
          d = round(lidR::density(header),1)
          f = round((200/d)/0.05)*0.05
          if (f > 1) f = 1
          output$estimatedDensityValue = renderText(d)
          output$recommandedFractionValue = renderText(f)
        }

        update_file_table(runif(1))
        update_state_table(runif(1))
        update_layout_map(runif(1))
        update_tree_map(runif(1))
        update_chm_map(runif(1))
        update_rgl_view(runif(1))
        update_preview_map(runif(1))
        update_stats_ui(runif(1))
        update_ggplots(runif(1))
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
    }
  }, ignoreInit = TRUE)

  # ===== OnClick loadLasFileButton ====
  observeEvent(input$loadLasFileButton, {
    path <- shinyFiles::parseFilePaths(volumes, input$loadLasFileButton)$datapath
    if (!is.null(path) && length(path) > 0)
    {
      tryCatch({
        plantation$set_cloud(path)

        # We set the point cloud. This should have set the CRS except if the LAS file has no CRS
        if (plantation$crs == sf::NA_crs_ | is.null(plantation$crs))
            showModal(selectCRSModalDialog())

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

        update_preview_map(runif(1))
        update_file_table(runif(1))
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
    }
  }, ignoreInit = TRUE)

  # ===== OnClick loadBlockPatternFileButton ====
  observeEvent(input$loadBlockPatternFileButton, {
    file <- shinyFiles::parseFilePaths(volumes, input$loadBlockPatternFileButton)$datapath

    if (!is.null(file) && length(file) > 0)
    {
      showNotification("Loading Excel database")

      tryCatch({
        plantation$set_database(file)
        plantation$set_layout_parameter(
          input$blockSizeInput,
          input$treeNumberInput,
          input$partternStartChoiceRadioButton,
          input$partternOrientationChoiceRadioButton)
        update_file_table(runif(1))
        update_layout_plot(runif(1))
        update_preview_map(runif(1))
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
    }
  }, ignoreInit = TRUE)

  # ===== OnClick loadBoundaryFileButton ====
  observeEvent(input$loadBoundaryFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadBoundaryFileButton)$datapath

    if (!is.null(file) && length(file) > 0)
    {
      showNotification("Loading boundaries polygon")

      tryCatch({

        plantation$set_boundaries(file)
        update_preview_map(runif(1))
        update_file_table(runif(1))
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
    }
  })

  # ===== OnClick loadCHMFileButton ====
  observeEvent(input$loadCHMFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadCHMFileButton)$datapath

    tryCatch({
      showNotification("Loading CHM")
      plantation$set_chm(file)
      update_preview_map(runif(1))
      update_chm_map(runif(1))
      update_file_table(runif(1))
      update_state_table(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  })

  # ===== OnClick loadTreeMapFileButton ====
  observeEvent(input$loadTreeMapFileButton, {

    file <- shinyFiles::parseFilePaths(volumes, input$loadTreeMapFileButton)$datapath

    tryCatch({
      showNotification("Loading tree plantation design")
      plantation$set_layout(file)
      update_file_table(runif(1))
      update_state_table(runif(1))
      update_tree_map(runif(1))
      update_layout_map(runif(1))
      update_preview_map(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  })


  # ===== OnClick smoothCHMButton ====
  observeEvent(input$smoothCHMButton, {

    tryCatch({
      showNotification("Smoothing CHM")
      plantation$smooth_chm(input$smoothCHM, input$smoothPasses)
      update_chm_map(runif(1))
      update_file_table(runif(1))
      update_state_table(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  })

  # ===== OnClick alignLayoutButton ====
  observeEvent(input$alignLayoutButton, {

    showNotification("Aligning tree layout")

    tryCatch({
      if (is.null(plantation$layout$tree_layout_oriented))  stop("Missing: tree layout")
      if (is.null(plantation$schm)) stop("Missing: smooth CHM")
      if (is.null(plantation$layout$origin)) stop("Missing: tree layout's origin")
      if (is.null(plantation$layout$spacing)) stop("Missing: tree layout'$'s spacing")
      if (is.null(plantation$boundaries)) stop("Missing: plantation boundaries")

      res = layout_alignment_lm(
        plantation$layout$tree_layout_raw,
        plantation$schm,
        plantation$layout$origin,
        plantation$layout$spacing*0.75,
        plantation$boundaries)
      angle = res[1]
      tx = res[2]
      ty = res[3]
      origin = plantation$layout$origin
      origin[1] = origin[1] + tx
      origin[2] = origin[2] + ty
      plantation$layout$set_angle(-angle)
      plantation$layout$set_origin(origin[1], origin[2])
      update_layout_map(runif(1))
      update_state_table(runif(1))
      saveProject(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick adjustLayoutButton ====
  observeEvent(input$adjustLayoutButton, {
    print("Tree detection")

    hmin = isolate(input$hminAdjustTreesSlider)

    if (is.null(hmin))
    {
      popup_error("Internal error: hmin = NULL")
      return()
    }

    tryCatch({
      withProgress(message = 'Tree detection', value = 0, {
        plantation$adjust_layout(hmin, progress = incProgress)
      })
      update_layout_map(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick runMeasurementButton ====
  observeEvent(input$runMeasurementButton, {
    hmin = isolate(input$hminMeasureTreesSlider)

    tryCatch({

      if (!plantation$is_adjusted())
        stop("Tree location not found yet. Please run tree localisation first (tab 4)")

      withProgress(message = 'Tree measurement', value = 0, {
        plantation$measure_trees(hmin, progress = incProgress)
      })

      update_file_table(runif(1))
      update_tree_map(runif(1))
      update_state_table(runif(1))
      update_stats_ui(runif(1))
      update_ggplots(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  # observeEvent(input$map_draw_edited_features, {
  #   edits <- input$map_draw_edited_features
  #   if (!is.null(edits)) {
  #     edited <- sf::st_read(jsonlite::toJSON(edits), quiet = TRUE)
  #     self$boundaries <- edited  # update your object
  #   }
  # })

  # ===== OnClick processPointCloudButton ====
  observeEvent(input$processPointCloudButton, {
    tryCatch({
      withProgress(message = 'Processing', value = 0, {

        if (is.null(plantation)) {
          stop("Uninitialized plantation.")
        }

        plantation$process_pointcloud(
          input$keepRandomFraction,
          input$rigidness,
          cloth_resolution = input$cloth_resolution,
          smoothCHM = input$smoothCHM,
          smoothPasses = input$smoothPasses,
          progress = incProgress)

        update_preview_map(runif(1))
        update_rgl_view(runif(1))
      })
    },
    error = function(e) {
      popup_error(conditionMessage(e))
    })

    update_chm_map(runif(1))
    update_preview_map(runif(1))
    update_file_table(runif(1))
  })


  # ===== OnChange partternChoiceRadioButton ====
  output$selectedPatternImage <- renderPlot({
    coords = generate_snake_coords(
      input$treeNumberInput, 0, 0, 1,
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

    text(coords$x+0.1, coords$y+0.1, labels = seq_len(nrow(coords)), cex = 0.6, col = "blue")
    par(oldpar)  # reset margins to previous values
  })

  # ===== OnChange any tree pattern inputs ====
  observeEvent(
    list(input$blockSizeInput,
         input$treeNumberInput,
         input$partternStartChoiceRadioButton,
         input$partternOrientationChoiceRadioButton),
    {
      validate(need(!is.null(plantation$layout$tree_layout_raw), "No block layout file selected yet"))

      plantation$set_layout_parameter(
        input$blockSizeInput,
        input$treeNumberInput,
        input$partternStartChoiceRadioButton,
        input$partternOrientationChoiceRadioButton)

      update_layout_plot(runif(1))
    }
  )

  # ===== OnEvent save ====
  observe({
    val <- saveProject()
    if (val != 0)
    {
      tryCatch({
        plantation$write_config()
      },
      error = function(e) {
        popup_error(conditionMessage(e))
      })
    }
  })

  # ===== OnEvent ggplot ====
  observe({
    update_ggplots()

    print("Update ggplots")

    tryCatch({
      gglist = plantation$get_ggstats()

      for (i in 1:6)
      {
        local({
          j <- i
          output[[paste0("ggplot", j)]] <- renderPlot({
            if (j <= length(gglist)) {
              gglist[[j]]
            } else {
              # return blank if not enough plots
              ggplot2::ggplot() + ggplot2::theme_void()
            }
          })
        })
      }
    },
    error = function(e) {
      popup_error(conditionMessage(e))
    })
  })

  observeEvent(input$confirm_crs, {
    req(input$choose_crs)
    plantation$set_crs(sf::st_crs(input$choose_crs))
    removeModal()
    update_preview_map(runif(1))
  })


  # ===== OnClick tree Zero =====
  output$clickTreeZeroInfo <- renderUI({

    click <- input$mapTreeLayout_click
    if (is.null(click)) return("Click on the map")

    # Convert to NZTM coordinates
    p <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    p <- sf::st_transform(p, 2193)
    coords <- as.numeric(sf::st_coordinates(p))

    print(paste0("Click = c(", paste0(coords, collapse = ", "), ")"))

    # Get plantation if needed
    if (is.null(plantation$layout))
    {
      popup_error("Clicked but no block layout was loaded")
      return()
    }
    else
    {
      plantation$layout$set_origin(coords[1], coords[2])
    }

    # Create nice HTML output
    HTML(sprintf(
      "<b>Clicked Coordinates:</b><br/>
     Latitude: %.6f<br/>
     Longitude: %.6f<br/>
     NZTM X: %.2f<br/>
     NZTM Y: %.2f",
      click$lat, click$lng, coords[1], coords[2]
    ))
  })

  # ===== update maps ====
  output$mapTreeLayout <- leaflet::renderLeaflet({
    showNotification("Updating layout map")
    update_layout_map()
    plantation$show_layout()
  })

  output$mapTrees <- leaflet::renderLeaflet({
    showNotification("Updating tree map")
    update_tree_map()
    plantation$show_trees()
  })

  output$mapCHM <- leaflet::renderLeaflet({
    showNotification("Updating CHM map")
    update_chm_map()
    plantation$show_chm()
  })


  output$mapPreview <- leaflet::renderLeaflet({
    showNotification("Updating preview map")
    update_preview_map()
    plantation$leaflet(trees = FALSE, schm = FALSE)
  })

  # ===== update table ====
  output$fileTable <- DT::renderDT({
    update_file_table()
    files_df = plantation$get_file_table()
    files_df$Path = short_path(files_df$Path, 50)
    na.omit(files_df)
  }, options = list(dom = 't', pageLength = 100))

  # Modal to show full path when row clicked
  observeEvent(input$fileTable_rows_selected,
  {
    selected_row <- input$fileTable_rows_selected
    if (length(selected_row) == 0) return()

    files_df <- plantation$get_file_table()
    full_path <- files_df$Path[selected_row]

    showModal(modalDialog(
      title = paste("Full path for row", selected_row),
      full_path,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  output$stateTable <- DT::renderDT({
    update_state_table()

    status_list <- plantation$state()

    df <- data.frame(
      Layer = names(status_list),
      Status = unlist(status_list),
      stringsAsFactors = FALSE
    )

    # Replace TRUE/FALSE with HTML icons
    df$Status <- ifelse(
      df$Status,
      "<i class='fa fa-check-circle' style='color: green;'></i>",
      "<i class='fa fa-times-circle' style='color: red;'></i>"
    )

    DT::datatable(
      df,
      selection = "single",
      escape = FALSE,   # allow HTML icons
      rownames = FALSE,
      options = list(dom = 't', pageLength = 100)
    )
  })

  observeEvent(input$stateTable_rows_selected,
  {
    selected_row <- input$stateTable_rows_selected
    if (length(selected_row) == 0) return()

    # Pick the object based on row index
    out <- switch(selected_row,
                  plantation$chm,
                  plantation$schm,
                  plantation$layout$tree_layout_oriented,
                  plantation$trees,
                  plantation$crowns
    )

    # Show modal depending on type
    if (inherits(out, "SpatRaster"))
    {
      showModal(modalDialog(
        title = paste("Raster for row", selected_row),
        verbatimTextOutput("modalRasterText"),
        easyClose = TRUE,
        footer = NULL
      ))

      output$modalRasterText <- renderPrint({print(out)})

    }
    else if (inherits(out, "sf") || is.data.frame(out))
    {
      showModal(modalDialog(
        title = paste("Table for row", selected_row),
        div(
          style = "overflow-y: auto; max-height: 1000px; max-width: 1000px",
          DT::renderDT(out, options = list(pageLength = 10, scrollX = TRUE, scrollY = "500px"))
        ),
        easyClose = TRUE,
        footer = NULL,
        size = "l"  # large modal
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
    update_layout_plot()
    validate(need(!is.null(plantation$layout$tree_layout_raw), "No block layout file selected yet"))
    print(plantation$layout)
    plantation$layout$plot()
  })

  output$rglplot3d <- rgl::renderRglwidget({
    update_rgl_view()
    showNotification("Rendering 3D scene")
    plantation$plot(TRUE)
    u = rgl::rglwidget()
    u |> rgl::toggleWidget(tags = "bbox") |>
      rgl::toggleWidget(tags = "boundaries") |>
      rgl::toggleWidget(tags = "ground") |>
      rgl::toggleWidget(tags = "vegetation")
    u
  })

  # === update ui ====
  output$vb_found <- renderUI({
    update_stats_ui()

    # Measured
    n = NA
    p = NA
    if (!is.null(plantation$trees))
    {
      N = nrow(plantation$trees)
      n = sum(plantation$trees$ApexFound)
      p = round(n/N*100,1)
    }

    value_box(
      title = "Trees Found",
      value = paste0(n, " (", p, "%)"),
      theme_color = "success"
    )
  })

  output$vb_missing <- renderUI({
    update_stats_ui()

    N = NA
    n = NA
    p = NA
    if (!is.null(plantation$trees))
    {
      N = nrow(plantation$trees)
      n = sum(plantation$trees$ApexFound | plantation$trees$TreeFound)
      p = round((N-n)/N*100,1)
    }
    value_box(
      title = "Missing Trees",
      value = paste0((N-n), " (", p, "%)"),
      theme_color = "primary"
    )
  })

  output$vb_non_measured <- renderUI({
    update_stats_ui()

    n = NA
    p = NA
    if (!is.null(plantation$trees))
    {
      N = nrow(plantation$trees)
      n = sum(!plantation$trees$ApexFound & plantation$trees$TreeFound)
      p = round(n/N*100,1)
    }

    value_box(
      title = "Non-measured Trees",
      value = paste0(n, " (", p, "%)"),
      theme_color = "danger"
    )
  })
}
