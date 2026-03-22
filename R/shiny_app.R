#' Launch the NSSH Explorer (Shiny App)
#'
#' This function starts the Shiny application included in the package.
#' The app provides interactive visualizations for growth models,
#' yearly statistics, age composition, and mapping of NSSH catch locations.
#'
#' @return A Shiny application object. Running this function launches the app.
#' @import bslib
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   run_NSSH()
#' }
#' @export
run_NSSH <- function() {
  shiny::addResourcePath('prefix', system.file('www', package = "NSSH"))
  shiny::shinyApp(ui, server)
}

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  bslib::page_navbar(
    title = "NSSH",

    # ----------------------------------------------------------------------------
    # Front page.
    # ----------------------------------------------------------------------------
    bslib::nav_panel(
      "Home",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_body(
            shiny::tags$h3("Norwegian Spring-Spawning Herring (NSSH)"),
            shiny::tags$p(
              "This application provides tools for exploring and visualizing ",
              "data from Norwegian spring-spawning herring. You can investigate ",
              "growth models, yearly statistics, age composition, and ",
              "geographical patterns in catches through plots and maps."
            ),
            shiny::tags$p(
              "Norwegian spring-spawning herring (", shiny::tags$em("Clupea harengus"),  "L.)",
              "is a pelagic saltwater fish belonging to the Atlanto-Scandian herring.",
              "The NSSH is an important economical, cultural and historical fisheries ",
              "resource in Norway. The NSSH spawn during spring months on the ",
              "west and north-west coast of Norway, before the larvae drift toward ",
              "their nursery regions in the Barents Sea. The juvenile herring ",
              "migrate southward and join the spawning stock after 3-4 years."
            ),
          )
        ),
        shiny::tags$figure(
            style = "text-align:center;",
            shiny::tags$img(
                src = "prefix/nssh_herring.jpg",
                style = "max-width: 100%; height: auto;"
            )
        )
      ),
      bslib::card(
        bslib::card_body(
          shiny::tags$h6(
            "Resources"
          ),
          shiny::tags$p(
            shiny::tags$strong("Data:"), "Institute of Marine Research (2022), HerringData [Data set] ",
            "From: https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv"
          ),
          shiny::tags$p(
            shiny::tags$strong("Source:"), "Institue of Marine Research (2025), Norwegian ",
              "spring-spawning herring, From: https://www.hi.no/en/hi/temasider/species/herring"
          ),
          shiny::tags$p(
            shiny::tags$strong("Photo:"), "Grette Hillersoey / Norwegian Seafood Council")
        )
      )
    ),

    # ----------------------------------------------------------------------------
    # Growth models tab.
    # ----------------------------------------------------------------------------
    bslib::nav_panel(
      "Growth Models",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput("model", "Model", c("VBGM", "Gompertz", "Logistic")),
          shiny::sliderInput("Linf", "Linf", min = 0, max = 70, value = 40),
          shiny::sliderInput("k", "k", min = 0.01, max = 1, value = 0.2),
          shiny::sliderInput("t0", "t0", min = -2, max = 2, value = 0),
          shiny::sliderInput("a", "a", min = 0, max = 2, value = 1)
        ),
        shiny::plotOutput("growth_plot")
      )
    ),

    # ----------------------------------------------------------------------------
    # Statistics per year tab
    # ----------------------------------------------------------------------------
    bslib::nav_panel(
      "Statistics",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::radioButtons(
            "stats_mode", "Statistics overview",
            c(
              "Counted fish per year"     = "counts",
              "Total weight per year"     = "weights",
              "Age composition"           = "agecomp",
              "Age summary"               = "agesummary"
            )
          ),
          # Year slider for Age composition
          shiny::conditionalPanel(
            condition = "input.stats_mode == 'agecomp'",
            shiny::sliderInput(
              "year", "Year",
              min = 1935, max = 2019, value = 2000,
              sep = "", step = 1
            )
          )
        ),
        shiny::uiOutput("stats_body")
      )
    ),
    # ----------------------------------------------------------------------------
    # Map of catch per year tab.
    # ----------------------------------------------------------------------------
    bslib::nav_panel(
      "Map",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::sliderInput(
            "map_year", "Year",
            min = 1935, max = 2019, value = 2000,
            sep = "", step = 1
            )
        ),
        leaflet::leafletOutput("map", height = "600px")
      )
    )
  )
)

server <- function(input, output, session) {
  #-----------------------------------------------------------------------------
  # Data download and preparation
  #-----------------------------------------------------------------------------
  # Runs whole data download and processing pipeline.
  herring_data <- process_nssh_data()

  # Assign to separate df.
  clean_herring <- herring_data$clean_herring
  growth_data_small <- herring_data$growth_data_small
  max_age <- herring_data$max_age
  counts_per_year <- herring_data$counts_per_year
  weights_per_year <- herring_data$weights_per_year
  age_counts <- herring_data$age_counts
  age_summary <- herring_data$age_summary
  map_summary <- herring_data$map_summary

  #-----------------------------------------------------------------------------
  # VBGM and Gompertz growth model
  #-----------------------------------------------------------------------------
  # Default state for VBGM:
  shiny::observe({
    shinyjs::enable("t0")
    shinyjs::disable("a")
  })
  # Enable/disable t0/a parameter for VBGM and logistic/Gompertz.
  shiny::observeEvent(input$model, {
    if (input$model == "Gompertz") {
      shinyjs::disable("t0")
      shinyjs::enable("a")
    } else {  # VBGM and logistic.
      shinyjs::enable("t0")
      shinyjs::disable("a")
    }
  })

  # Predict growth models curves with input data from UI.
  pred_data <- shiny::reactive({
    t <- seq(0, max_age + 2, length.out = 100)

    if (input$model == "VBGM") {
      length_pred <- vbgm(t, input$Linf, input$k, input$t0)
    } else if (input$model == "Gompertz") {
      length_pred <- gompertz_model(t, input$Linf, input$k, a = input$a)
    } else if (input$model == "Logistic") {
      length_pred <- logistic_model(t, input$Linf, input$k, input$t0)
    }

    tibble::tibble(age = t, length = length_pred)
  })

  # Plot growth models.
  output$growth_plot <- shiny::renderPlot({
    ggplot2::ggplot(growth_data_small, ggplot2::aes(age, length)) +
      ggplot2::geom_point(color = "grey70", alpha = 0.2) +
      ggplot2::geom_line(data = pred_data(), ggplot2::aes(age, length),
                         color = "steelblue", linewidth = 1.2) +
      ggplot2::labs(x = "Age (years)", y = "Length (cm)",
                    title = paste("Growth model:", input$model)) +
      ggplot2::scale_x_continuous(breaks = seq(0, max_age, by = 2)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 14, face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))
      )
  })

  #-----------------------------------------------------------------------------
  # Stats per year.
  #-----------------------------------------------------------------------------
  # Main switcher for Statistics body.
  output$stats_body <- shiny::renderUI({
    switch(
      input$stats_mode,
      "counts"     = shiny::plotOutput("counts_plot", height = "600px"),
      "weights"    = shiny::plotOutput("weights_plot", height = "600px"),
      "agecomp"    = shiny::plotOutput("agecomp_plot", height = "600px"),
      "agesummary" = DT::dataTableOutput("age_summary_table", height = "600px")
    )
  })

  # Counted fish per year.
  output$counts_plot <- shiny::renderPlot({

    df <- dplyr::rename(counts_per_year, value = .data$n_ids)

    ggplot2::ggplot(df, ggplot2::aes(.data$year, .data$value)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(x = "Year", y = "Number of fish (unique IDs)", title = "Number of fish per year") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 14, face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))
      )
  })

  # Total weight per year.
  output$weights_plot <- shiny::renderPlot({

    df <- dplyr::rename(weights_per_year, value = .data$total_weight)

    ggplot2::ggplot(df, ggplot2::aes(.data$year, .data$value)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(x = "Year", y = "Total weight (tonnes)", title = "Total weight per year") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 14, face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))
      )
  })

  # Age composition (selected year).
  output$agecomp_plot <- shiny::renderPlot({

    df <- dplyr::filter(age_counts, .data$year == input$year)

    shiny::validate(shiny::need(nrow(df) > 0, paste("No age data for", input$year)))

    ggplot2::ggplot(df, ggplot2::aes(x = factor(.data$age), y = .data$n)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(x = "Age (years)", y = "Number of fish (unique IDs)", title = paste("Age composition in", input$year)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 14, face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))
      )
  })

  # Age summary table (all years).
  output$age_summary_table <- DT::renderDataTable({
    age_summary
  })

  # ------------------------------------------------------------------
  # Interactive map per year.
  # ------------------------------------------------------------------
  # Map the catches filtered by input year.
  filtered_catches <- shiny::reactive({
    dplyr::filter(map_summary, .data$year == input$map_year) |>
      dplyr::mutate(
        label = paste0(
          "<b>Year:</b> ", .data$year, "<br/>",
          "<b>Month:</b> ", .data$month, "<br/>",
          "<b>Number of fish:</b> ", .data$n_fish, "<br/>",
          "<b>Mean age:</b> ", .data$mean_age, "<br> years<br/>",
          "<b>Mean weight:</b> ", .data$mean_weight, "<br> grams<br/>"
        )
      )
  })

  # Plot map.
  output$map <- leaflet::renderLeaflet({

    df <- filtered_catches()

    shiny::validate(shiny::need(nrow(df) > 0, "No catch points for this year."))

    leaflet::leaflet(df) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addCircleMarkers(
        lng = ~ lon, lat = ~ lat,
        radius = 5,
        fillColor = "steelblue",
        fillOpacity = 0.7,
        stroke = FALSE,
        label = lapply(df$label, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style   = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )) |>
      leaflet::fitBounds(min(df$lon), min(df$lat), max(df$lon), max(df$lat))
  })
}
