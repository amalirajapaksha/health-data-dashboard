library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readxl)
library(ggplot2)

# ------------------------------
# LOAD DATA
# ------------------------------
pop_data <- read_excel("population_pyramid_tidy.xlsx")
pop_data$Age <- as.numeric(pop_data$Age)

life_table_tidy <- read_excel("life_table_tidy.xlsx")
life_table_tidy$Age <- as.numeric(life_table_tidy$Age)


# ------------------------------
# UI
# ------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Health Data Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home"),
      
      menuItem("Demography", icon = icon("users"),
               menuSubItem("Population Pyramid", tabName = "pyramid"),
               menuSubItem("Life Table Functions", tabName = "life")    # <<< NEW TAB
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(HTML("
        html, body { overflow-x: hidden; scroll-behavior: smooth; }
        .content-wrapper, .right-side { background-color: #e6f2ff !important; overflow-y: auto; height: 100vh; }
        .box-title { font-size: 24px !important; text-align: center !important; width: 100%; }
        .left-box { height: 90vh !important; display: flex; flex-direction: column; justify-content: space-between; }
        .left-box .box-body { background-color: #2c3e50 !important; color: white; flex-grow: 1; }
        .plotly { width: 100% !important; height: auto !important; }
      "))
    ),
    
    tabItems(
      
      # ------------------ HOME TAB ------------------
      tabItem(tabName = "home",
              div(style = "background:#003366;color:white;text-align:center;font-size:44px;
             font-weight:800;padding:60px 10px;margin:-20px -20px 30px -20px;
             box-shadow:0 4px 12px rgba(0,0,0,0.4);border-bottom:6px solid #0055aa;",
                  "Welcome to the Australia Health Data Dashboard"),
              
              fluidRow(
                column(width = 12, br(),
                       p(style = "text-align:center; font-size:18px; color:#333;",
                         "This Home page is currently empty. Add widgets or information here later.")
                )
              )
      ),
      
      # ------------------ POPULATION PYRAMID ------------------
      tabItem(tabName = "pyramid",
              fluidRow(
                column(width = 3,
                       box(
                         solidHeader = TRUE,
                         width = 12,
                         class = "left-box",
                         style = "background-color:#2c3e50; color:white; height:90vh;", 
                         
                         sliderInput("year", "Select Year:",
                                     min = min(pop_data$Year),
                                     max = max(pop_data$Year),
                                     value = min(pop_data$Year),
                                     step = 1
                         ),
                         
                         fluidRow(
                           column(4, actionButton("prev_btn", "Prev", icon = icon("arrow-left"),
                                                  width = "90%", style = "padding:5px; font-size:80%;")),
                           column(4, actionButton("play_pause_btn", "Play", icon = icon("play"),
                                                  width = "90%", style = "padding:5px; font-size:80%;")),
                           column(4, actionButton("next_btn", "Next", icon = icon("arrow-right"),
                                                  width = "90%", style = "padding:5px; font-size:80%;"))
                         ),
                         
                         hr(),
                         valueBoxOutput("kpi_total", width = NULL),
                         valueBoxOutput("kpi_male", width = NULL),
                         valueBoxOutput("kpi_female", width = NULL)
                       )
                ),
                
                column(width = 9,
                       box(
                         title = "Population Pyramid",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("pyramidPlot", height = "calc(90vh)")
                       )
                )
              )
      ),
      
      # ------------------ LIFE TABLE FUNCTIONS ------------------
      tabItem(tabName = "life",
              fluidRow(
                
                # LEFT PANEL
                column(width = 3,
                       box(
                         solidHeader = TRUE,
                         width = 12,
                         class = "left-box",
                         style = "background-color:#2c3e50; color:white; height:90vh;",
                         
                         # --- Select State ---
                         selectInput("state_lt", "Select State:",
                                     choices = sort(unique(life_table_tidy$State)),
                                     selected = unique(life_table_tidy$State)[1]),
                         
                         # --- Select Gender ---
                         selectInput("gender_lt", "Select Gender:",
                                     choices = c("Male" = "M", "Female" = "F", "Both" = "Both"),
                                     selected = "Both")
                       )
                ),
                
                # RIGHT PANEL
                column(width = 9,
                       box(
                         title = "Life Table Functions",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         height = "90vh",
                         
                         tabsetPanel(
                           tabPanel("eₓ", plotlyOutput("ex_plot")),
                           tabPanel("qₓ", plotlyOutput("qx_plot")),
                           tabPanel("S(x)", plotlyOutput("sx_plot"))
                         )
                       )
                )
              )
      )
    )
  )
)


# ------------------------------
# SERVER
# ------------------------------
server <- function(input, output, session) {
  
  # ------------------ POPULATION PYRAMID ------------------
  filtered_data <- reactive({
    pop_data %>%
      filter(Year == input$year) %>%
      mutate(Pop_100k = ifelse(Gender == "M", -Population/100000, Population/100000))
  })
  
  output$kpi_total <- renderValueBox({
    valueBox(
      value = format(sum(filtered_data()$Population), big.mark = ","),
      subtitle = "Total Population",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$kpi_male <- renderValueBox({
    valueBox(
      value = format(sum(filtered_data()$Population[filtered_data()$Gender=="M"]), big.mark = ","),
      subtitle = "Male Population",
      icon = icon("male"),
      color = "blue"
    )
  })
  
  output$kpi_female <- renderValueBox({
    valueBox(
      value = format(sum(filtered_data()$Population[filtered_data()$Gender=="F"]), big.mark = ","),
      subtitle = "Female Population",
      icon = icon("female"),
      color = "red"
    )
  })
  
  # Animation controls
  anim_running <- reactiveVal(FALSE)
  observeEvent(input$play_pause_btn, {
    if (input$year == max(pop_data$Year)) {
      updateSliderInput(session, "year", value = min(pop_data$Year))
    }
    anim_running(!anim_running())
    updateActionButton(session, "play_pause_btn",
                       label = ifelse(anim_running(), "Pause", "Play"),
                       icon = icon(ifelse(anim_running(), "pause", "play")))
  })
  
  observeEvent(input$prev_btn, {
    new_year <- input$year - 1
    if(new_year >= min(pop_data$Year)){
      updateSliderInput(session, "year", value = new_year)
    }
    anim_running(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play", icon = icon("play"))
  })
  
  observeEvent(input$next_btn, {
    new_year <- input$year + 1
    if(new_year <= max(pop_data$Year)){
      updateSliderInput(session, "year", value = new_year)
    }
    anim_running(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play", icon = icon("play"))
  })
  
  auto_update <- reactiveTimer(500)
  observe({
    auto_update()
    isolate({
      if (isTRUE(anim_running())) {
        if (input$year < max(pop_data$Year)) {
          updateSliderInput(session, "year", value = input$year + 1)
        } else {
          anim_running(FALSE)
          updateActionButton(session, "play_pause_btn", label = "Play", icon = icon("play"))
        }
      }
    })
  })
  
  # Pyramid Plot
  output$pyramidPlot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = Age, y = Pop_100k, fill = Gender,
                        text = paste("Age:", Age,
                                     "<br>Gender:", Gender,
                                     "<br>Population:", Population))) +
      geom_bar(stat = "identity", width = 0.95) +
      coord_flip() +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ------------------ LIFE TABLE FUNCTIONS ------------------
  life_filtered <- reactive({
    df <- life_table_tidy %>% filter(State == input$state_lt)
    if (input$gender_lt != "Both") df <- df %>% filter(Gender == input$gender_lt)
    df
  })
  
  # e(x)
  output$ex_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, ex, colour = Gender)) +
      geom_line(size = 0.75) +
      theme_minimal(base_size = 14) +
      labs(title = "Life Expectancy eₓ", x = "Age", y = "eₓ")
    ggplotly(p)
  })
  
  # q(x)
  output$qx_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, qx, colour = Gender)) +
      geom_line(size = 0.75) +
      theme_minimal(base_size = 14) +
      labs(title = "Mortality Rate qₓ", x = "Age", y = "qₓ")
    ggplotly(p)
  })
  
  # S(x)
  output$sx_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, Sx, colour = Gender)) +
      geom_line(size = 0.75) +
      theme_minimal(base_size = 14) +
      labs(title = "Survival Function S(x)", x = "Age", y = "S(x)")
    ggplotly(p)
  })
  
}

# ------------------------------
# RUN APP
# ------------------------------
shinyApp(ui, server)
