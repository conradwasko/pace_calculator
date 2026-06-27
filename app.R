library(shiny)

ui <- fluidPage(
  titlePanel("Target Pace Calculator"),

  sidebarLayout(
    sidebarPanel(
      h4("Finish Time"),
      numericInput("hours", "Hours", value = 0, min = 0, step = 1),
      numericInput("minutes", "Minutes", value = 25, min = 0, max = 59, step = 1),
      numericInput("seconds", "Seconds", value = 0, min = 0, max = 59, step = 1),

      h4("Distance"),
      numericInput("distance", "Distance value", value = 5, min = 0.0001, step = 0.1),
      selectInput("unit", "Distance unit",
                  choices = c("Kilometers" = "km", "Miles" = "mi", "Meters" = "m"),
                  selected = "km")
    ),

    mainPanel(
      h3("Result"),
      verbatimTextOutput("paceResult"),
      br(),
      helpText("Pace is shown per kilometer and per mile, regardless of the unit you entered the distance in.")
    )
  )
)

server <- function(input, output) {

  output$paceResult <- renderText({
    total_seconds <- input$hours * 3600 + input$minutes * 60 + input$seconds

    if (is.na(total_seconds) || total_seconds <= 0) {
      return("Please enter a valid finish time.")
    }
    if (is.na(input$distance) || input$distance <= 0) {
      return("Please enter a valid distance.")
    }

    # Convert distance to km and miles
    distance_km <- switch(input$unit,
                           "km" = input$distance,
                           "mi" = input$distance * 1.60934,
                           "m"  = input$distance / 1000)

    distance_mi <- distance_km / 1.60934

    pace_sec_per_km <- total_seconds / distance_km
    pace_sec_per_mi <- total_seconds / distance_mi

    format_pace <- function(sec) {
      m <- floor(sec / 60)
      s <- round(sec %% 60)
      if (s == 60) { m <- m + 1; s <- 0 }
      sprintf("%d:%02d", m, s)
    }

    format_time <- function(sec) {
      h <- floor(sec / 3600)
      m <- floor((sec %% 3600) / 60)
      s <- round(sec %% 60)
      sprintf("%02d:%02d:%02d", h, m, s)
    }

    paste0(
      "Finish time: ", format_time(total_seconds), "\n",
      "Distance: ", input$distance, " ", input$unit,
      " (", round(distance_km, 3), " km / ", round(distance_mi, 3), " mi)\n\n",
      "Target pace:\n",
      "  ", format_pace(pace_sec_per_km), " per km\n",
      "  ", format_pace(pace_sec_per_mi), " per mile"
    )
  })
}

shinyApp(ui = ui, server = server)
