#' Launch the Herring Data Explorer Shiny App
#'
#' This function starts the Shiny application included in the package.
#' The app provides interactive visualizations for growth models,
#' yearly statistics, age composition, and mapping of NSSH catch locations.
#'
#' @return A Shiny application object. Running this function launches the app.
#'
#' @examples
#' \dontrun{
#'   run_app()
#' }
#'
#' @export
run_NSSH <- function() {
  shiny::shinyApp(ui, server)
}

ui <- bslib::page_navbar(
  title = "Norwegian Spring Spawning Herring (NSSH)",
  header = shinyjs::useShinyjs(),

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
        shiny::sliderInput("a", "a", min = -1, max = 1, value = 0)
      ),
      shiny::plotOutput("growth_plot")
    )
  ),

  # ----------------------------------------------------------------------------
  # Statistics per year tab
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    "Statistics",
    bslib:: layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput("stats", "Statistics", c("Counts","Weights"))
      ),
      shiny::plotOutput("stats_plot")
    )
  ),

  # ----------------------------------------------------------------------------
  # Age composition per year tab.
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    "Age Composition",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::radioButtons(
          "age_mode",
          "Display mode",
          c("Composition" = "comp", "Summary (mean & max)" = "summary")
        ),
        shiny::sliderInput(
          "year", "Year",
          min = 1935, max = 2019, value = 2000,
          sep = "", step = 1
        )
      ),
      shiny::uiOutput("age_plot")
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

server <- function(input, output, session) {
  #-----------------------------------------------------------------------------
  # Read precomputed targets
  #-----------------------------------------------------------------------------
  clean_herring      <- targets::tar_read(clean_herring)
  growth_data_small  <- targets::tar_read(growth_data_small)
  max_age            <- targets::tar_read(max_age)
  counts_per_year    <- targets::tar_read(counts_per_year)
  weights_per_year   <- targets::tar_read(weights_per_year)
  age_counts         <- targets::tar_read(age_counts)
  age_summary        <- targets::tar_read(age_summary_per_year)
  catch_locations    <- targets::tar_read(catch_locations_ocean)

  #-----------------------------------------------------------------------------
  # VBGM and Gompertz growth model
  #-----------------------------------------------------------------------------
  # Default state for VBGM:
  shinyjs::enable("t0")
  shinyjs::disable("a")

  # Enable/disable t0/a parameter for VBGM/Gompertz.
  shiny::observeEvent(input$model, {
    if (input$model == "Gompertz") {
      shinyjs::disable("t0")
      shinyjs::enable("a")
    } else {  # VBGM
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
      ggplot2::theme_bw()
  })

  #-----------------------------------------------------------------------------
  # Stats per year.
  #-----------------------------------------------------------------------------
  # UI choose if stats show counts or weight.
  output$stats_plot <- shiny::renderPlot({
    if (input$stats == "Counts") {
      df   <- dplyr::rename(counts_per_year, value = n_ids)
      ylab <- "Number of fish (unique IDs)"
    } else {
      df   <- dplyr::rename(weights_per_year, value = total_weight)
      ylab <- "Total weight (tonnes)"
    }

  # Plot stats per year plot.
    ggplot2::ggplot(df, ggplot2::aes(year, value)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(x = "Year", y = ylab,
                    title = paste(input$stats, "of NSSH per year")) +
      ggplot2::theme_bw()
  })

  # ------------------------------------------------------------------
  # Age composition per year.
  # ------------------------------------------------------------------
  # Choose between agecomp_plot and age_summary_table.
  output$age_plot <- shiny::renderUI({
    if (input$age_mode == "comp") {
      shiny::plotOutput("agecomp_plot")
    } else {
      DT::dataTableOutput("age_summary_table")
    }
  })

  # Plot of age composition in choosen year.
    output$agecomp_plot <- shiny::renderPlot({
      df <- dplyr::filter(age_counts, year == input$year)

      # Validate input from slider, that there is age data at input year.
      shiny::validate(shiny::need(nrow(df) > 0, paste("No age data for", input$year)))

      ggplot2::ggplot(df, ggplot2::aes(x = factor(age), y = n)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::theme_bw() +
        ggplot2::labs(
          x = "Age",
          y = "Count",
          title = paste("Age composition in", input$year))
    })

  # Plot table of number of fish, mean and max age per year.
    output$age_summary_table <- DT::renderDataTable({
      age_summary
    })

  # ------------------------------------------------------------------
  # Interactive map per year.
  # ------------------------------------------------------------------
  # Map the catches filtered by input year.
  filtered_catches <- reactive({
    dplyr::filter(catch_locations, year == input$map_year)
  })

  # Plot map.
  output$map <- leaflet::renderLeaflet({
    df <- filtered_catches()
    validate(need(nrow(df) > 0, "No catch points for this year (after filtering)."))
    leaflet::leaflet(df) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 4,
        fillColor = "steelblue",
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste0(
          "<b>Year:</b> ", year, "<br>",
          "<b>Fish:</b> ", n_fish, "<br>",
          "<b>Lon:</b> ", round(lon, 3), "<br>",
          "<b>Lat:</b> ", round(lat, 3)
        )
      ) |>
      leaflet::fitBounds(min(df$lon), min(df$lat), max(df$lon), max(df$lat))
  })
}
