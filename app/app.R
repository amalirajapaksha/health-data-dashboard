# Shiny Dashboard for Canada Health Data - Coming Soon
library(shiny)

ui <- fluidPage(
  titlePanel("Canada Health Data Dashboard"),
  sidebarLayout(
    sidebarPanel("Sidebar"),
    mainPanel("Dashboard Visualizations Coming Soon")
  )
)

server <- function(input, output) {}

shinyApp(ui, server)

