library(shiny)

ui <- fluidPage(

  titlePanel("Herring Growth Explorer"),

  sidebarLayout(

    sidebarPanel(

      h4("VBGM parameters"),

      sliderInput("Linf", "Linf (asymptotic length)",
                  min = 20, max = 60, value = 35),

      sliderInput("k", "Growth rate (k)",
                  min = 0.01, max = 1, value = 0.2),

      sliderInput("t0", "t0 parameter",
                  min = -2, max = 2, value = 0)
    ),

    mainPanel(

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

server <- function(input, output) {

  output$growth_plot <- renderPlot({

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
}
shinyApp(ui, server)
