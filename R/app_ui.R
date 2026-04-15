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

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)

link_rlidar <- tags$a(
  shiny::icon("r-project"), "r-lidar",
  href = "https://www.r-lidar.com",
  target = "_blank"
)

link_vignette <- tags$a(
  shiny::icon("book"), "Documentation",
  href = "www/Tutorial.html",
  target = "_blank"
)


# ---- UI ----

ui <- page_navbar(
  window_title = "RBPC plantation measurement",
  title = tagList(tags$img(src = "www/logo.png", height = "50px"),
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  navbar_options = navbar_options(bg = "#0062cc", underline = TRUE),

  header = tagList(
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        if (e.ctrlKey && e.key === 'k') {
          e.preventDefault();
          Shiny.setInputValue('reload', new Date().getTime());
        }
      });
    ")),
    tags$style(HTML("
      .irs { font-size: 11px; }
      .form-group { margin-bottom: 6px; }
      .control-label { margin-bottom: 2px; font-size: 12px; }
    "))
  ),

  # ---- nav 1 project ----

  nav_panel(
    title = "1. Project",
    layout_columns(
      col_widths = c(4, 8), # two columns for cards

      card(
        full_screen = FALSE,
        card_header("New project"),
        card_body(
          tooltiped("1. Create a project", "To work, the application needs an initialized project."),
          shinyFiles::shinySaveButton(
            'createProjectButton', 'Create a project', 'Create a RPBC project as...',
            filetype = "rpbc",
            viewtype = "icon",
            icon = bsicons::bs_icon("cast")
          ),
          tooltiped("2. Select a point cloud.",
                    "LAS or LAZ format"),
          shinyFiles::shinyFilesButton(
            'loadLasFileButton', 'Select point cloud file', 'Please select a point cloud',
            FALSE,
            viewtype = "icon",
            icon = icon("cloud"),
            class = "w-100"
          ),
          tooltiped("3. Select a database file", "Select an Excel file containing the plantation database."),
          shinyFiles::shinyFilesButton(
            'loadBlockPatternFileButton', 'Select Excel file', 'Please select an Excel file',
            FALSE,
            viewtype = "icon",
            icon = icon("table")
          ),
          tooltiped("4. Select a boundary file", "A geospatial file containing plantation boundaries. Mandatory if not read from the Excel file."),
          shinyFiles::shinyFilesButton(
            'loadBoundaryFileButton', 'Select geospatial file', 'Please select a geospatial file',
            FALSE,
            viewtype = "icon",
            icon = bsicons::bs_icon("bounding-box")
          )
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Map preview"),
        div(
          style = "width: 100%; height: 100%;",
          leaflet::leafletOutput("mapPreview", width = "100%", height = "100%")
        )
      ),

      div(
        style = "display: flex; flex-direction: column; gap: 1rem;",
        card(
          full_screen = FALSE,
          card_header("Existing project"),
          card_body(
            tooltiped(
              "Select an '.rpbc' config file",
              "The file will be read and the previous configuration restored."
            ),
            shinyFiles::shinyFilesButton(
              'loadConfigFileButton',
              'Select config file',
              'Please select a config file',
              FALSE,
              icon = icon("gear")
            )
          )
        ),
        card(
          full_screen = FALSE,
          card_header("Update project"),
          card_body(
            tooltiped(
              "Update current project configuration",
              "This will exit the app to install the new version."
            ),
            p(strong("Current version: "), textOutput("currentVersion", inline = TRUE)),
            p(strong("Online version available: "), textOutput("onlineVersion", inline = TRUE)),
            actionButton("updateProjectButton", "Update project", icon = icon("refresh"))
          )
        )
      ),


      layout_columns(
        col_widths = c(8, 4),
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
        )
      )
    )
  ),

  # ---- nav 2 point cloud ----
  nav_panel(
    title = "2. Point cloud",
    layout_sidebar(
      title = "Sidebar 2",
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p(
            "Estimated density: ", textOutput("estimatedDensityValue", inline = TRUE),
            "pts/m\u00B2, Recommended fraction: ", textOutput("recommandedFractionValue", inline = TRUE),
            sliderInput("keepRandomFraction", "Load fraction", min = 0.05, max = 1, value = 1, step = 0.05)
          )
        ),
        wellPanel(
          tooltiped(tags$strong("Ground Filtering"), "Using the default parameters is recommended"),
          sliderInput("rigidness", "Rigidness", min = 1, max = 3, value = 2, step = 1),
          sliderInput("cloth_resolution", "Cloth resolution", min = 0.1, max = 2, value = 0.5, step = 0.1)
        ),
        wellPanel(
          tooltiped(tags$strong("CHM Parameters"), "Using the default parameters is recommended"),
          sliderInput("res", "CHM resolution (m)", min = 0.02, max = 0.5, value = 0.1, step = 0.01)
        ),
        actionButton("processPointCloudButton", "Process"),
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
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tooltiped(tags$strong("CHM smoothing"), "Higher values = more agressive smoothing"),
          sliderInput("smoothCHM", "CHM smoothing (m)", min = 0.1, max = 3, value = 1, step = 0.1),
          sliderInput("smoothPasses", "Smoothing passes", min = 1, max = 5, value = 2, step = 1),
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
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p(tags$strong("Tree planting pattern")),
          div(
            style = "display: flex; align-items: center; gap: 6px;",
            "Block size: ",
            numericInput("blockSizeInputX", NULL, value = 18.6, min = 5, max = 50, step = 0.1, width = "100px"),
            tags$span("\u00D7"),
            numericInput("blockSizeInputY", NULL, value = 18.6, min = 5, max = 50, step = 0.1, width = "100px")
          ),
          div(
            style = "display: flex; align-items: center; gap: 6px;",
            "Num. trees: ",
            numericInput("treeNumberInputX", NULL, value = 6, min = 1, max = 15, step = 1, width = "100px"),
            tags$span("\u00D7"),
            numericInput("treeNumberInputY", NULL, value = 6, min = 1, max = 15, step = 1, width = "100px")
          ),
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

      sidebar = sidebar(
        width = 300,
        wellPanel(
          tooltiped(tags$strong("Tree map"), "If an accurate map of the tree is available, load it. Otherwise the map will be generated from the block layout built in step 3."),
          shinyFiles::shinyFilesButton('loadTreeMapFileButton', 'Select file', 'Please select a geospatial file', FALSE, icon = icon("table")),
        ),
        wellPanel(
          tooltiped(tags$strong("Tree Zero"), "This tree will be used as pivot point to align the tree layout map real trees"),
          uiOutput("clickTreeZeroInfo"),
          tooltiped("Run automatic alignment.", "Once the pivot point has been chosen accurately, this button triggers an alignment with real trees"),
          actionButton("alignLayoutButton", "Align trees")
        ),
        wellPanel(
          tooltiped(tags$strong("Optimization by block"), "Tries to optimize the position of the tree locally by block instead of globally."),
          actionButton("optimBlockButton", "Optimize")
        ),
        wellPanel(
          tooltiped(tags$strong("Edit mode"), "When clicking on the edit button on the map, then save, this option enables to switch edition modes. See the tutorial for more details."),
          radioButtons(
            inputId = "editRadioButtons",
            label = NULL,
            choices = c("Align", "Replace"),
            selected = "Align"
          )
        ),
        wellPanel(
          tags$p(tags$strong("Run adjustment")),
          tooltiped("Run automatic adjustment", "The layout pattern is not 100% accurate. This step starts from the tree plantation layout and retrieve the real positions of the trees"),
          sliderInput("hminAdjustTreesSlider",
                      "Minimal height",
                      min = 0, max = 10, value = 2, step = 0.25),
          actionButton("adjustLayoutButton", "Adjust trees")
        )
      ),
      div(
        tags$style(HTML("
          /* Shrink the Leaflet.draw edit/move handles */
          .leaflet-div-icon.leaflet-editing-icon {
            width: 8px !important;
            height: 8px !important;
            margin-left: -4px !important;
            margin-top: -4px !important;
            border-radius: 4px !important;
          }
        ")),
        style = "width: 100%; height: 100%;",
        leaflet::leafletOutput("mapTreeLayout",width = "100%", height = "100%")
      ),
    )
  ),

  # ---- nav 6 measurements ----
  nav_panel(
    title = "6. Segmentation",
    layout_sidebar(
      title = "Sidebar 6",
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p("Once the trees are detected run tree segmentation"),
          sliderInput("hminMeasureTreesSlider", "Minimal height", min = 0, max = 10, value = 3, step = 0.25),
          actionButton("runSegmentButton", "Segment trees")
        )
      ),
      div(
        style = "width: 100%; height: 100%;",
        leaflet::leafletOutput("mapTrees",width = "100%", height = "100%")
      )
    )
  ),

  # ---- nav 7 individual tree ----
  nav_panel(
    title = "7. Individual trees",
    layout_sidebar(
      title = "Sidebar 7",
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p("Once the trees are segmented run tree metrics"),
          sliderInput("fractionMetrics", "Point cloud fraction", min = 0, max = 1, value = 1, step = 0.1),
          sliderInput("alpha", "Alpha shape", min = 0, max = 10, value = 1, step = 0.5),
          sliderInput("zthmetric", "Z Threshold", min = 0, max = 2, value = 1, step = 0.1),
          actionButton("runMeasurementButton", "Compute metrics")
        ),
        wellPanel(
          tags$p("View one tree"),
          sliderInput("sliderViewTreeID", "Tree ID", min = 0, max = 0, value = 0, step = 1),
        ),
        wellPanel(
          tags$p("Export individual point clouds for each trees"),
          actionButton("exportTreesButton", "Export trees")
        )
      ),
      div(
        style = "width: 100%; height: 100%;",
        rgl::rglwidgetOutput("rglplot3dtree",width = "100%", height = "100%")
      )
    )
  ),


  # ---- nav 8 statistics ----
  nav_panel(
    title = "8. Statistics",

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
    nav_item(link_rlidar),
    nav_item(link_vignette)
  )
)
