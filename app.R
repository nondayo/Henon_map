# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
    
      h1("Henon Map"),
      p("The Hénon map takes a point (xn, yn) in the plane and maps it to a new point"),
      withMathJax("$$X_{n+1} = 1 + aX_n^2 + bY_n$$"),
      withMathJax("$$Y_{n+1} = X_n$$"),
      p("Change the value of variables and observe the Henon Map"),
        
      # Select variable for a
      sliderInput(
        inputId = "a",
        label = "a",
        min = 1,
        max = 1.5,
        value = 1.2
      ),
      # Select variable for b
      sliderInput(
        inputId = "b",
        label = "b",
        min = 0.2,
        max = 0.4,
        value = 0.3
      ),
      # Select initial value for Xn
      sliderInput(
        inputId = "initial_Xn",
        label = "initial X",
        min = 0,
        max = 1,
        value = 0.5
      ),
      # Select initial value for Yn
      sliderInput(
        inputId = "initial_Yn",
        label = "initial Y",
        min = 0,
        max = 1,
        value = 0.5
      )
    ),
    
    # Output: Show Henon map
    mainPanel(
      
      plotOutput(outputId = "Henon_map")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$Henon_map <- renderPlot({

    X = c(input$initial_Xn)
    Y = c(input$initial_Yn)
    for(i in 1:10^4){
      Y[i+1] = X[i]
      X[i+1] = 1 - input$a*X[i]^2 + input$b*Y[i]
    }
    df <- data.frame(X = X, Y = Y)

    size = 0.1
    alpha = 0.04
    p <- ggplot() +
      geom_point(aes(X, Y), df, size = size, alpha = alpha) +
      geom_path(aes(X, Y), df, size = size, alpha = alpha)
      theme_classic()
    p
    
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)