library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("GPA Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("credits_1", "Credits for Semester 1:", value = 0, min = 0),
      numericInput("gpa_1", "GPA for Semester 1 (0-4):", value = 0, min = 0, max = 4),
      numericInput("credits_2", "Credits for Semester 2:", value = 0, min = 0),
      numericInput("gpa_2", "GPA for Semester 2 (0-4):", value = 0, min = 0, max = 4),
      numericInput("credits_3", "Credits for Semester 3:", value = 0, min = 0),
      numericInput("gpa_3", "GPA for Semester 3 (0-4):", value = 0, min = 0, max = 4),
      actionButton("calculate", "Calculate Final GPA")
    ),
    mainPanel(
      h4("Semester 1 Detail:"),
      textOutput("semester1_detail"),
      h4("Semester 2 Detail:"),
      textOutput("semester2_detail"),
      h4("Semester 3 Detail:"),
      textOutput("semester3_detail"),
      h4("Final GPA:"),
      textOutput("final_gpa")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    # Calculate GPA for each semester
    semester_gpa <- function(credits, gpa) {
      total_credits <- sum(credits)
      weighted_gpa <- sum(credits * gpa)
      final_gpa <- weighted_gpa / total_credits
      list(total_credits = total_credits, weighted_gpa = weighted_gpa, final_gpa = final_gpa)
    }
    
    # Semester 1 GPA calculation
    semester1 <- semester_gpa(input$credits_1, input$gpa_1)
    
    # Semester 2 GPA calculation
    semester2 <- semester_gpa(input$credits_2, input$gpa_2)
    
    # Semester 3 GPA calculation
    semester3 <- semester_gpa(input$credits_3, input$gpa_3)
    
    # Output
    output$semester1_detail <- renderText({
      paste("Total Credits:", semester1$total_credits, "| Weighted GPA:", semester1$weighted_gpa, "| GPA:", round(semester1$final_gpa, 2))
    })
    output$semester2_detail <- renderText({
      paste("Total Credits:", semester2$total_credits, "| Weighted GPA:", semester2$weighted_gpa, "| GPA:", round(semester2$final_gpa, 2))
    })
    output$semester3_detail <- renderText({
      paste("Total Credits:", semester3$total_credits, "| Weighted GPA:", semester3$weighted_gpa, "| GPA:", round(semester3$final_gpa, 2))
    })
    
    # Calculate final GPA
    total_credits <- semester1$total_credits + semester2$total_credits + semester3$total_credits
    weighted_gpa <- semester1$weighted_gpa + semester2$weighted_gpa + semester3$weighted_gpa
    final_gpa <- weighted_gpa / total_credits
    output$final_gpa <- renderText({paste("Final GPA:", round(final_gpa, 2))})
  })
}

# Run the application
shinyApp(ui = ui, server = server)
