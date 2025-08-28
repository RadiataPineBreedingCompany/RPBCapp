library(shiny)

cat("Loading UI...\n")

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

# ---- UI ----

addResourcePath("www", system.file("app/www/", package = "RPBCapp"))

ui <- page_navbar(
  window_title = "RBPC plantation measurement",
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
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p(
            "Estimated density: ", textOutput("estimatedDensityValue", inline = TRUE),
            "pts/m², Recommended fraction: ", textOutput("recommandedFractionValue", inline = TRUE),
            sliderInput("keepRandomFraction", "Load fraction", min = 0.05, max = 1, value = 1, step = 0.05)
          )
        ),
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
      sidebar = sidebar(
        width = 300,
        wellPanel(
          tags$p(tags$strong("CHM smoothing")),
          sliderInput("smoothCHM", "CHM smoothing (m)", min = 0, max = 3, value = 1, step = 0.1),
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
      sidebar = sidebar(
        width = 300,
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

      sidebar = sidebar(
        width = 300,
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
          sliderInput("hminAdjustTreesSlider",
                      "Minimal height",
                      min = 0, max = 10, value = 2, step = 0.25),
          actionButton("adjustLayoutButton", "Adjust trees")
        )
      ),
      div(
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
    nav_item(link_rlidar)
  )
)
