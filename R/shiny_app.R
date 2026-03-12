library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(leaflet)

ui <- page_navbar(
  title = "Herring Data Explorer",
  nav_panel(
    "Growth Models",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("model", "Model", c("VBGM", "Gompertz")),
        sliderInput("Linf", "Linf", 0, 70, 40),
        sliderInput("k", "k", 0.01, 1, 0.2),
        sliderInput("t0", "t0", -2, 2, 0)
        sliderInput("a", "a", -2, 2, 1)
      ),
      plotOutput("growth_plot")
    )
  ),
  nav_panel(
    "Statistics",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("stats", "Statistics", c("Counts","Weights"))
      ),
      plotOutput("stats_plot")
    )
  ),
  nav_panel(
    "Age Composition",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("year", "Year",
                    min = 1935,
                    max = 2019,
                    value = 2000,
                    sep = "",
                    step = 1
                    )
        )
      ),
      plotOutput("age_plot")
  ),
  nav_panel(
    "Map",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput(
          "map_year",
          "Year",
          min = 1935,
          max = 2019,
          value = 2000,
          sep = "",
          step = 1
        )
      ),
      leafletOutput("map", height = "600px")
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
  catch_locations    <- targets::tar_read(catch_locations_ocean)

  #-----------------------------------------------------------------------------
  # VBGM and Gompertz growth model
  #-----------------------------------------------------------------------------
  # Default state for VBGM:
  shinyjs::enable("t0")
  shinyjs::disable("a")

  # Enable/disable t0/a parameter for VBGM/Gompertz.
  observeEvent(input$model, {
    if (input$model == "Gompertz") {
      shinyjs::disable("t0")
      shinyjs::enable("a")
    } else {  # VBGM
      shinyjs::enable("t0")
      shinyjs::disable("a")
    }
  })

  # Predict growth models curves with imput data from UI.
  pred_data <- reactive({
    t <- seq(0, max_age + 2, length.out = 100)
    if (input$model == "VBGM") {
      len <- vbgm(t, input$Linf, input$k, input$t0)
    } else {
      len <- gompertz_model(t, input$Linf, input$k, input$a)
    }
    tibble::tibble(age = t, length = len)
  })

  # Plot growth models.
  output$growth_plot <- renderPlot({
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
  output$stats_plot <- renderPlot({
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
  # Filter age plot on UI year input.
  output$age_plot <- renderPlot({
    df <- age_counts |> dplyr::filter(year == input$year)

  # Validate input from slider, that there is age data at input year.
    validate(need(nrow(df) > 0, paste("No age data for", input$year)))

  # Plot of age composition in choosen year.
    ggplot2::ggplot(df, ggplot2::aes(x = factor(age), y = n)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(
        x = "Age",
        y = "Count",
        title = paste("Age composition in", input$year)
      ) +
      ggplot2::theme_bw()
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
        radius = ~scales::rescale(n_fish, to = c(3, 12)), # if you added n_fish
        fillColor = "steelblue", fillOpacity = 0.7, stroke = FALSE
      ) |>
      leaflet::fitBounds(min(df$lon), min(df$lat), max(df$lon), max(df$lat))
  })
}

shinyApp(ui, server)


