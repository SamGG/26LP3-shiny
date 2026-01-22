#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Business layer ====

source( "1_backend.R" )


# UI layer ====

ui <- fluidPage(

  # Application title
  titlePanel("p-value analysis"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      numericInput("delta",
                   "Difference:",
                   min = 0, max = 5, value = 0, step = 0.25),
      sliderInput("n",
                  "Sample size:",
                  min = 5, max = 20, value = 10, step = 1),
      selectInput("N",
                  "Number of simulations:",
                  choices = c(1000, 3000, 10000, 30000)),
      sliderInput("prop",
                  "Proportion true diff:",
                  min = 0, max = 1, value = 1.0),
      selectInput("binwidth",
                  "Bin width (p-value):",
                  choices = c(0.10, 0.05, 0.02, 0.01), selected = 0.1)
    ),

    # Show plots of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("p-value", plotOutput("pvaluePlot")),
                  tabPanel("means", plotOutput("meanPlot")))
    )
  )
)


# server layer ====

server <- function(input, output) {

  the_data <- reactive({
    generate_data(
      n_indiv = as.numeric( input$n ),
      n_var = as.numeric( input$N ),
      diff = as.numeric( input$delta ),
      prop = as.numeric( input$prop )
    )
  })

  output$meanPlot <- renderPlot({
    # retrieve values
    X_diff <- the_data()
    req( X_diff )
    plot_hist( X_diff )
  })

  output$pvaluePlot <- renderPlot({

    # retrieve values
    X_diff <- the_data()
    req( X_diff )
    bin_width <- as.numeric( input$binwidth )
    X_pvalue <- calc_pvalue( X_diff )
    hist( X_pvalue, breaks = seq( 0, 1, bin_width) )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Construction d'une application Shiny
# gestion des couleurs, code hexadecimal, transparence avec alpha
#
# reactive
# reactiveValues
# interactivity avec Shiny
# set.seed
# button
# debogage
# separate model from UI and controller
#
