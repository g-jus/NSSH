library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Herring Growth Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Model", choices = c("VBGM", "Gompertz")),
      sliderInput("Linf", "Linf (asymptotic length)",
                  min = 0, max = 50, value = 30),
      sliderInput("k", "Growth rate (k)",
                  min = 0.01, max = 1, value = 0.2),
      # This slider will be disabled for Gompertz:
      sliderInput("t0", "t0 parameter",
                  min = -2, max = 2, value = 0)
    ),
    mainPanel(plotOutput("plot"))))

      tabsetPanel(

        tabPanel("Growth curve",
                 plotOutput("growth_plot")
        ),

        tabPanel("Age distribution",
                 plotOutput("age_plot")
        ),

        tabPanel("Length distribution",
                 plotOutput("length_plot")
        ),

        tabPanel("Summary",
                 tableOutput("summary_table")
        )

      )

    )
  )
)



server <- function(input, output, session) {

  # Disable t0 when Gompertz is chosen
  observeEvent(input$model, {
    if (input$model == "Gompertz") {
      shinyjs::disable("t0")
    } else {
      shinyjs::enable("t0")
    }
  })

  # Example dataset (replace with your real one)
  herring_data <- herring_read()

  # Reactive model predictions
  pred_data <- reactive({
    t <- seq(0, max(herring_data$age) + 2, length.out = 200)

    if (input$model == "VBGM") {
      length_pred <- vbgm(t, input$Linf, input$k, input$t0)
    } else {
      length_pred <- gompertz_model(t, input$Linf, input$k, a = 1)
    }

    data.frame(age = t, length = length_pred)
  })

  # Plot data + model
  output$plot <- renderPlot({

    plot(herring_data$age, herring_data$length,
         pch = 19, col = "black",
         xlab = "Age",
         ylab = "Length",
         main = paste("Growth Model:", input$model))

    lines(pred_data()$age, pred_data()$length,
          col = "blue", lwd = 2)
  })
}

shinyApp(ui, server)

#server <- function(input, output, session) {

  output$plot <- renderPlot({

    age_seq <- seq(min(herring$age), max(herring$age), length.out = 200)

    predicted_length <- input$Linf * (1 - exp(-input$k * (age_seq - input$t0)))

    # Plot the real data (background)
    plot(herring$age, herring$length,
         pch = 16,
         col = "grey70",
         xlab = "Age",
         ylab = "Length",
         main = "Herring Growth Model")

    # Add the VBGM curve
    lines(age_seq, predicted_length,
          col = "blue",
          lwd = 3)

  })

  output$age_plot <- renderPlot({


  })
}
shinyApp(ui, server)
