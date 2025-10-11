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
          tags$p(tags$strong("Ground Filtering")),
          sliderInput("rigidness", "Rigidness", min = 1, max = 3, value = 2, step = 1),
          sliderInput("cloth_resolution", "Cloth resolution", min = 0.1, max = 2, value = 0.5, step = 0.1)
        ),
        wellPanel(
          tooltiped(tags$strong("CHM Parameters"), "Using the default parameters is recommended"),
          sliderInput("res", "CHM resolution (m)", min = 0.02, max = 1, value = 0.1, step = 0.01)
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
          tags$p(tags$strong("CHM smoothing")),
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
          tags$p(tags$strong("Optimization by block")),
          actionButton("optimBlockButton", "Optimize")
        ),
        wellPanel(
          tags$p(tags$strong("Edit mode")),
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
    title = "6. Measurement",
    layout_sidebar(
      title = "Sidebar 6",
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p("Once the trees are detected run tree measurement"),
          sliderInput("hminMeasureTreesSlider", "Minimal height", min = 0, max = 10, value = 4.5, step = 0.25),
          actionButton("runMeasurementButton", "Measure trees")
        ),
        wellPanel(
          actionButton("exportTreesButton", "Export trees")
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
    nav_item(link_rlidar),
    nav_item(link_vignette)
  )
)
