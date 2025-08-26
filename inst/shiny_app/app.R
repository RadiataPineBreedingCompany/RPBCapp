library(shiny)
library(bslib)
library(RPBCapp)

tooltiped <- function(text, tooltip_text)
{
  tags$p(
    text, " ",
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      tooltip_text
    )
  )
}

popup_error = function(msg)
{
  showModal(
    modalDialog(
      div(
        style = "display: flex; align-items: center;",
        span(icon("exclamation-triangle"), style = "color: #a94442; font-size: 24px; margin-right: 10px;"),
        span("Error", style = "color: #a94442; font-weight: bold; font-size: 20px;")
      ),
      tags$div(msg,
               style = "margin-top: 10px; color: #333;"),
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
      size = "m"
    )
  )
}

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

# ---- UI ----

addResourcePath("www", system.file("shiny_app/www/", package = "RPBCapp"))

ui <- page_navbar(
  title = tagList(
    tags$img(src = "www/logo.png", height = "50px"),
  ),
  theme = bs_theme(version = 5, , bootswatch = "flatly"),
  navbar_options = navbar_options(bg = "#0062cc", underline = TRUE),

  # ---- nav 1 project ----
  nav_panel(
    title = "1. Project",
    layout_columns(
      col_widths = c(4, 8), # two equal columns for cards
      card(
        full_screen = FALSE,
        card_header("New project"),
        card_body(
          tooltiped("1. Create a project", "To work the application needs an initialized project. "),
          shinyFiles::shinySaveButton(
            'createProjectButton', 'Create a project', 'Create a RPBC project as...',
            filetype = "rpbc",
            viewtype = "icon",
            icon = bsicons::bs_icon("cast")
          ),
          tooltiped("2. Select a point cloud or a canopy height model. ", "The application can work with a Canopy Height Model only. The point cloud is not mandatory. The point cloud can be used to build the CHM."),
          fluidRow(
            column(
              width = 5,
              shinyFiles::shinyFilesButton(
                'loadLasFileButton', 'Select point cloud file', 'Please select a point cloud',
                FALSE,
                viewtype = "icon",
                icon = icon("cloud"),
                class = "w-100"
              )
            ),
            column(
              width = 2,
              div(style = "text-align: center; line-height: 38px;", strong("or"))
            ),
            column(
              width = 5,
              shinyFiles::shinyFilesButton(
                'loadCHMFileButton', 'Select CHM file', 'Please select a config file',
                FALSE,
                viewtype = "icon",
                icon = bsicons::bs_icon("grid-3x3"),
                class = "w-100"
              )
            )
          ),
          tags$br(),
          tooltiped("3. Select a boundary file ", "A geospatial file containing plantation boundaries."),
          shinyFiles::shinyFilesButton(
            'loadBoundaryFileButton', 'Select geospatial file', 'Please select a geospatial file',
            FALSE,
            viewtype = "icon",
            icon = bsicons::bs_icon("bounding-box")),
          tags$br(),
          tooltiped("4. Select a database file ", "Select an Excel file containing the plantation database."),
          shinyFiles::shinyFilesButton(
            'loadBlockPatternFileButton', 'Select Excel file', 'Please select an Excel file',
            FALSE,
            viewtype = "icon",
            icon = icon("table")),
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Map preview"),
        div(
          style = "width: 100%; height: 100%;",
          leaflet::leafletOutput("mapPreview",width = "100%", height = "100%")
        )
      ),

      card(
        full_screen = FALSE,
        card_header("Existing project"),
        card_body(
          tooltiped("Select an ‘.rpcb’ config file",  "The file will be read and the previous configuration restored."),
          shinyFiles::shinyFilesButton('loadConfigFileButton', 'Select config file', 'Please select a config file', FALSE, icon = icon("gear")),
        )
      ),

      layout_columns(
        col_widths = c(8,4),
        card(
          full_screen = TRUE,
          card_header("Project Data"),
          card_body(
            DT::DTOutput("fileTable")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Project state"),
          card_body(
            DT::DTOutput("stateTable")
          )
        ),
      )
    )
  ),

  # ---- nav 2 point cloud ----
  nav_panel(
    title = "2. Point cloud",
    layout_sidebar(
      title = "Sidebar 2",
      sidebar = sidebar(width = 300,
        wellPanel(
          tags$p(tags$strong("Ground Filtering")),
          sliderInput("rigidness", "Rigidness", min = 1, max = 3, value = 2, step = 1),
          sliderInput("cloth_resolution", "Cloth resolution", min = 0.1, max = 2, value = 0.5, step = 0.1)
        ),
        wellPanel(
          tooltiped(tags$strong("CHM Parameters"), "Using the default parameters is recommanded"),
          sliderInput("res", "CHM resolution (m)", min = 0.02, max = 1, value = 0.1, step = 0.01)
        ),
        actionButton("processPointCloudButton", "Process")
      ),
      div(
        style = "width: 100%; height: 100%;",
        rgl::rglwidgetOutput("rglplot3d",width = "100%", height = "100%")
      )
    )
  ),

  # ---- nav 3 chm ----
  nav_panel(
    title = "3. CHM",
    layout_sidebar(
      title = "Sidebar 3",
      sidebar = sidebar(width = 300,
        wellPanel(
          tags$p(tags$strong("CHM smoothing")),
          sliderInput("smoothCHM", "CHM smoothing (m)", min = 0, max = 5, value = 2, step = 0.25),
          sliderInput("smoothPasses", "Smoothing passes", min = 0, max = 5, value = 2, step = 1),
          actionButton("smoothCHMButton", "Smooth")
        ),
      ),
      div(
        style = "width: 100%; height: 100%;",
        leaflet::leafletOutput("mapCHM", width = "100%", height = "100%")
      )
    ),
  ),

  # ---- nav 4 layout ----
  nav_panel(
    title = "4. Tree layout",
    layout_sidebar(
      title = "Sidebar 3",
      sidebar = sidebar(width = 300,
        wellPanel(
          tags$p(tags$strong("Tree planting pattern")),
          numericInput("blockSizeInput", "Block size", value = 18.6, min = 5, max = 50, step = 0.1),
          numericInput("treeNumberInput", "Number of Trees", value = 6, min = 2, max = 12, step = 1),

          radioButtons(
            "partternStartChoiceRadioButton", "Select the start point:",
            choices = c("Bottom-left" = "bl",
                        "Bottom-right" = "br",
                        "Top-left" = 'tl',
                        "Top right" = "tr"),
            selected = "bl",
            inline = TRUE
          ),

          radioButtons(
            "partternOrientationChoiceRadioButton", "Select an orientation:",
            choices = c("Vertical" = "v",
                        "Horizontal" = "h"),
            selected = "v",
            inline = TRUE
          ),
        ),
        imageOutput("selectedPatternImage")
      ),
      div(
        style = "width: 100%; height: 100%;",
        imageOutput("plantationLayoutImage",width = "100%", height = "100%")
      )
    )
  ),

  # ---- nav 5 alignment ----
  nav_panel(
    title = "5. Layout alignment",
    layout_sidebar(
      title = "Sidebar 5",

      sidebar = sidebar(width = 300,
        wellPanel(
          tags$p(tags$strong("Tree map")),
          tags$p("If an accurate map of the tree is available load it. Otherwise the map will be generated from the block layout built in step 3."),
          shinyFiles::shinyFilesButton('loadTreeMapFileButton', 'Select file', 'Please select an geospatiale file', FALSE, icon = icon("table")),
        ),
        wellPanel(
          tags$p(tags$strong("Tree Zero")),
          tooltiped("Using the CHM, click on the tree zero", "This tree will be use as pivot point to align the tree layout map real trees"),
          uiOutput("clickTreeZeroInfo")
        ),
        wellPanel(
          tags$p(tags$strong("Run alignment")),
          tooltiped("Run automatic alignment.", "Once the pivot point has been choosen accurately, this button trigger an alignment with real trees"),
          actionButton("alignLayoutButton", "Align trees")
        ),
        wellPanel(
          tags$p(tags$strong("Run adjustment")),
          tooltiped("Run automatic adjustment", "The layout pattern is not 100% accurate. This step starts from the tree plantation layout and retrieve the real positions of the trees"),
          sliderInput("hminAdjustTreesSlider", "Minimal height", min = 0, max = 10, value = 2, step = 0.25),
          actionButton("adjustLayoutButton", "Adjust trees")
        )
      ),
      div(
        style = "width: 100%; height: 100%;",
        leaflet::leafletOutput("mapTreeLayout",width = "100%", height = "100%")
      ),
    )
  ),

  # ---- nav 6 alignment ----
  nav_panel(
    title = "6. Measurement",
    layout_sidebar(
      title = "Sidebar 6",
      sidebar = sidebar(width = 300,
        wellPanel(
          tags$p("Once the trees are detected run tree measurement"),
          sliderInput("hminMeasureTreesSlider", "Minimal height", min = 0, max = 10, value = 2, step = 0.25),
          actionButton("runMeasurementButton", "Measure trees")
        )
      ),
      div(
        style = "width: 100%; height: 100%;",
        leaflet::leafletOutput("mapTrees",width = "100%", height = "100%")
      )
    )
  ),

  # ---- nav 7 statistics ----
  nav_panel(
    title = "7. Statistics",

    layout_column_wrap(
      width = 1/3,  # three boxes in one row
      uiOutput("vb_found"),
      uiOutput("vb_missing"),
      uiOutput("vb_non_measured")
    ),

    # cards with ggplots
    layout_column_wrap(
      width = 1/3,  # 2 cards per row
      !!!lapply(1:6, function(i) {
        card(
          full_screen = TRUE,
          card_header(paste("Plot", i)),
          card_body(
            plotOutput(paste0("ggplot", i))
          )
        )
      })
    )
  ),

  nav_spacer(),

  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

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
  saveProject        <- reactiveVal(0)

  volumes <- c(RPBC = "/home/jr/Documents/Entreprise/clients/RPBC/Plantations/",
               Home = fs::path_home(),
               shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "loadLasFileButton", roots = volumes, filetypes = c('las', "laz"))
  shinyFiles::shinyFileChoose(input, "loadCHMFileButton", roots = volumes, filetypes = c('tif', 'tiff'))
  shinyFiles::shinyFileChoose(input, "loadConfigFileButton", roots = volumes, filetypes = c('rpbc'))
  shinyFiles::shinyFileChoose(input, "loadBoundaryFileButton", roots = volumes, filetypes = c('shp', "gpkg"))
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

        if (!is.null(plantation$schm))
        {
          updateSliderInput(session, "smoothCHM", value = plantation$params$smoothCHM)
          updateSliderInput(session, "smoothPasses", value = plantation$params$smoothPasses)
        }

        update_file_table(runif(1))
        update_state_table(runif(1))
        update_layout_map(runif(1))
        update_tree_map(runif(1))
        update_chm_map(runif(1))
        update_rgl_view(runif(1))
        update_preview_map(runif(1))
        update_stats_ui(runif(1))
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
      showNotification("Loading block layout")

      tryCatch({
        plantation$set_database(file)
        plantation$set_layout_parameter(
          input$blockSizeInput,
          input$treeNumberInput,
          input$partternStartChoiceRadioButton,
          input$partternOrientationChoiceRadioButton)
        update_file_table(runif(1))
        update_layout_plot(runif(1))
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
      res = layout_alignment_angle(
        plantation$layout$tree_layout,
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
      withProgress(message = 'Tree measurement', value = 0, {
        plantation$measure_trees(hmin, progress = incProgress)
      })
      update_tree_map(runif(1))
      update_state_table(runif(1))
      update_stats_ui(runif(1))
    },
    error = function(e)
    {
      popup_error(conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  # ===== OnClick loadLasFileButton ====
  observeEvent(input$loadLasFileButton, {
    path <- shinyFiles::parseFilePaths(volumes, input$loadLasFileButton)$datapath
    if (!is.null(path) && length(path) > 0)
    {
      tryCatch({
        plantation$set_cloud(path)
        update_preview_map(runif(1))
        update_file_table(runif(1))
      },
      error = function(e)
      {
        popup_error(conditionMessage(e))
      })
    }
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
      validate(need(!is.null(plantation$layout), "No block layout file selected yet"))

      plantation$set_layout_parameter(
        input$blockSizeInput,
        input$treeNumberInput,
        input$partternStartChoiceRadioButton,
        input$partternOrientationChoiceRadioButton)

      update_layout_plot(runif(1))
    }
  )

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

  # ===== OnClick tree Zero =====
  output$clickTreeZeroInfo <- renderUI({

    click <- input$mapTreeLayout_click
    if (is.null(click)) return("Click on the map")

    # Convert to NZTM coordinates
    p <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    p <- sf::st_transform(p, 2193)
    coords <- as.numeric(sf::st_coordinates(p))

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
    plantation$leaflet()
  })

  # ===== update table ====
  output$fileTable <- DT::renderDT({
    update_file_table()
    files_df = plantation$get_file_table()
    na.omit(files_df)
  }, options = list(dom = 't', pageLength = 100))

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
      escape = FALSE,   # allow HTML icons
      rownames = FALSE,
      options = list(dom = 't', pageLength = 100)
    )
  })

  # ==== update plot ====
  output$plantationLayoutImage <- renderPlot({
    update_layout_plot()
    validate(need(!is.null(plantation$layout), "No block layout file selected yet"))
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
    N = nrow(plantation$trees)
    n = sum(plantation$trees$ApexFound)
    p = round(n/N*100,1)

    value_box(
      title = "Trees Found",
      value = paste0(n, " (", p, "%)"),
      theme_color = "success"
    )
  })

  output$vb_missing <- renderUI({
    update_stats_ui()

    N = nrow(trees)
    n = sum(plantation$trees$ApexFound | plantation$trees$TreeFound)
    p = round((N-n)/N*100,1)

    value_box(
      title = "Missing Trees",
      value = paste0((N-n), " (", p, "%)"),
      theme_color = "primary"
    )
  })

  output$vb_non_measured <- renderUI({
    update_stats_ui()

    N = nrow(plantation$trees)
    n = sum(!plantation$trees$ApexFound & plantation$trees$TreeFound)
    p = round(n/N*100,1)

    value_box(
      title = "Non-measured Trees",
      value = paste0(n, " (", p, "%)"),
      theme_color = "danger"
    )
  })
}

shinyApp(ui, server)
