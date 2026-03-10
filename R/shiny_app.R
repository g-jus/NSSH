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

  # ------------------------------------------------------------------
  # Importing NSSH data
  # ------------------------------------------------------------------
  herring_data <- herring_read()
  clean_herring <- cleaning_herring(herring_data)
  # ------------------------------------------------------------------
  # GROWTH MODEL: Disable t0 for Gompertz
  # ------------------------------------------------------------------
  observeEvent(input$growth_model, {
    if (input$model == "Gompertz") {
      shinyjs::disable("t0")
    } else {
      shinyjs::enable("t0")
    }
  })

  # ------------------------------------------------------------------
  # SMALLER DATASET FOR GROWTH CURVES
  # ------------------------------------------------------------------

  growth_data_small <- reactive({
    clean_herring |>
      dplyr::sample_n(min(5000, nrow(clean_herring)))   # limit to 3000 points
  })

  # ------------------------------------------------------------------
  # REACTIVE PREDICTION CURVE
  # ------------------------------------------------------------------
  pred_data <- reactive({
    t <- seq(0, max(clean_herring$age) + 2, length.out = 100)


    if (input$model == "VBGM") {
      length_pred <- vbgm(t, input$Linf, input$k, input$t0)
    } else {
      length_pred <- gompertz_model(t, input$Linf, input$k, a = 1)
    }

    data.frame(age = t, length = length_pred)
  })

  # Plot data + model
  output$growth_plot <- renderPlot({
    ggplot(growth_data_small(), aes(age, length)) +
      geom_point(color = "grey70", fill = "grey70", alpha = 0.2, shape = 21) +
      geom_line(data = pred_data(), aes(age, length), color = "steelblue", linewidth = 1.2) +
      labs(
        x = "Age (years)",
        y = "Length (cm)",
        title = paste("Growth model:", input$model)
      ) +
      scale_x_continuous(breaks = seq(0, max(clean_herring$age), by = 2)) +
      scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
      theme_bw()
  })

  # ------------------------------------------------------------------
  # STATS PANEL
  # ------------------------------------------------------------------
  output$stats_plot <- renderPlot({
     df <-  if (input$stats == "Count") {
        count_per_year(clean_herring) |>
          dplyr::rename(value = n_ids)
      } else {
        weight_per_year(clean_herring) |>
          dplyr::rename(value = total_weight)
      }


    y_lab <- if (input$stats == "Count") "Number of fish (unique IDs)" else "Total weight"
    ttl   <- paste(input$stats, "of NSSH per year")

    ggplot(df, aes(x = year, y = value)) +
      geom_col(fill = "steelblue") +
      labs(
        x = "Year",
        y = y_lab,
        title = ttl
      ) +
      theme_bw()
  })
  # ------------------------------------------------------------------
  # AGE COMPOSITION PANEL (placeholder)
  # ------------------------------------------------------------------
  output$age_plot <- renderPlot({
    df <- age_count_for_year(clean_herring, input$year)

    validate(
      need(nrow(df) > 0, paste("No age data available for year", input$year))
    )

    ggplot(df, aes(x = factor(age), y = n)) +
      geom_col(fill = "steelblue") +
      labs(
        x = "Age",
        y = "Count",
        title = paste("Age composition in", input$year)
      ) +
      theme_bw()
  })

  # ------------------------------------------------------------------
  # MAP PANEL — Leaflet placeholder
  # ------------------------------------------------------------------
  filtered_catches <- reactive({
    location_catches(clean_herring, input$map_year) |>
    filter_ocean_points()
  })

  output$map <- leaflet::renderLeaflet({
    df <- filtered_catches()

    validate(need(nrow(df) > 0, "No catch points for this year (after filtering)."))

    leaflet(df) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 4,
        fillColor = "steelblue",
        fillOpacity = 0.7,
        stroke = FALSE
      ) |>
      fitBounds(
        min(df$lon), min(df$lat),
        max(df$lon), max(df$lat)
      )
  })
}
shinyApp(ui, server)
