library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readxl)
library(ggplot2)
library(openxlsx)


# --- Load population data ---
pop_data <- read_excel("population_pyramid_tidy.xlsx")
pop_data$Age <- as.numeric(pop_data$Age)

life_table_tidy <- read_excel("life_table_tidy.xlsx")
life_table_tidy$Age <- as.numeric(life_table_tidy$Age)

# --- UI ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Health Data Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home"),
      
      # --- Main Menu with Subtabs ---
      menuItem("Demography", icon = icon("users"),
               menuSubItem("Life Table Functions", tabName = "life"),
               menuSubItem("Population Pyramid", tabName = "pyramid")
               
               # Later more subtabs will be added like:
               # menuSubItem("Life Table", tabName = "life"),
               # menuSubItem("Mortality Trends", tabName = "mortality")
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
        .control-row .shiny-input-container { margin-bottom: 0px; }
        .left-box .small-box h3, .left-box .small-box p { color: white !important; }
        .box { max-width: 100%; }
        .plotly { width: 100% !important; height: auto !important; }

        /* --- Home banner style --- */
        .home-banner {
          background-color: #003366;
          color: white;
          text-align: center;
          font-size: 44px;
          font-weight: 800;
          padding: 60px 10px;
          margin: -20px -20px 30px -20px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.4);
          border-bottom: 6px solid #0055aa;
        }

        @media (min-height: 800px) { #pyramidPlot { height: calc(90vh) !important; } }
        @media (max-height: 799px) { #pyramidPlot { height: calc(75vh) !important; } }
        @media (max-width: 1600px) { .value-box, .small-box { font-size: 90%; } .box-body { padding: 10px !important; } }
        @media (max-width: 1200px) { .home-banner { font-size: 32px; padding: 40px 5px; } }
        @media (max-width: 992px) { .col-sm-3, .col-sm-9 { width: 100%; } }
      "))
    ),
    
    tabItems(
      # --- Home Tab ---
      tabItem(tabName = "home",
              div(class = "home-banner", "Welcome to the Australia Health Data Dashboard"),
              fluidRow(
                column(width = 12, br(),
                       p(style = "text-align:center; font-size:18px; color:#333;",
                         "This Home page is currently empty. Add widgets or information here later.")
                )
              )
      ),
      
      # --- Population Pyramid (Subtab under Demography) ---
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
                                     choices = c("Male" = "Male", "Female" = "Female", "Both" = "Both"),
                                     selected = "Both"),
                         
                         hr(),
                         
                         valueBoxOutput("kpi_e0_male", width = NULL),
                         valueBoxOutput("kpi_e0_female", width = NULL),
                         downloadButton("download_lt_merged", "Download Life Table (Male & Female)")
                       )
                ),
                
                # RIGHT PANEL
                column(width = 9,
                       box(
                         title = "Life Table Functions",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         height = "75vh",
                         
                         tabsetPanel(
                           tabPanel(
                             "eₓ",
                             div(style = "width:100%;",
                                 plotlyOutput("ex_plot", height = "55vh"),
                                 div(style = "background-color:#e0f3ff; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("This plot shows the life expectancy (eₓ) by age for the selected state and gender.",
                                       style = "font-size:20px;")
                                 )
                             )
                           ),
                           tabPanel(
                             "qₓ",
                             div(style = "width:100%;",
                                 plotlyOutput("qx_plot", height = "55vh"),
                                 div(style = "background-color:#e0f3ff; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("This plot shows the mortality rate (qₓ) by age for the selected state and gender.",
                                       style = "font-size:20px;")
                                 )
                             )
                           ),
                           tabPanel(
                             "S(x)",
                             div(style = "width:100%;",
                                 plotlyOutput("sx_plot", height = "55vh"),
                                 div(style = "background-color:#e0f3ff; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("This plot shows the survival function (S(x)) by age for the selected state and gender.",
                                       style = "font-size:20px;")
                                 )
                             )
                           )
                         )
                       )
                )
              )
      )
    )
  )
  
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # ------------------ POPULATION PYRAMID ------------------
  filtered_data <- reactive({
    pop_data %>%
      filter(Year == input$year) %>%
      mutate(Pop_100k = ifelse(Gender == "M", -Population/100000, Population/100000))
  })
  
  # --- KPIs ---
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
  
  # --- Animation Controls ---
  anim_running <- reactiveVal(FALSE)
  
  observeEvent(input$play_pause_btn, {
    if (input$year == max(pop_data$Year)) {
      updateSliderInput(session, "year", value = min(pop_data$Year))
    }
    
    anim_running(!anim_running())
    
    if (anim_running()) {
      updateActionButton(session, "play_pause_btn", label = "Pause", icon = icon("pause"))
    } else {
      updateActionButton(session, "play_pause_btn", label = "Play", icon = icon("play"))
    }
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
  
  # --- Population Pyramid Plot ---
  output$pyramidPlot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = Age, y = Pop_100k, fill = Gender,
                        text = paste("Age:", Age,
                                     "<br>Gender:", Gender,
                                     "<br>Population:", Population))) +
      geom_bar(stat = "identity", width = 0.95) +
      coord_flip() +
      scale_y_continuous(
        limits = c(-2.56, 2.56),
        breaks = seq(-2.56, 2.56, by = 0.32),
        labels = function(x) paste0(abs(x * 100000 / 1000), "k")
      ) +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      scale_fill_manual(
        values = c("M" = "#1f4e79", "F" = "#8b1a1a"),
        labels = c("M" = "Male", "F" = "Female")
      ) +
      labs(
        title = paste("Year", input$year),
        x = "Age",
        y = "Population"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(uirevision = "constant_pyramid")
  })
  
  # ------------------ LIFE TABLE FUNCTIONS ------------------
  
  # --- KPIs ---
  
  life_birth <- reactive({
    life_table_tidy %>%
      filter(State == input$state_lt, Age == 0)
  })
  
  output$kpi_e0_male <- renderValueBox({
    df <- life_birth() %>% filter(Gender == "Male")
    
    valueBox(
      value = round(df$ex, 1),
      subtitle = HTML("Life Expectancy at Birth<br><b>e<sub>0</sub> (Male)</b>"),
      icon = icon("male"),
      color = "blue"
    )
  })
  
  output$kpi_e0_female <- renderValueBox({
    df <- life_birth() %>% filter(Gender == "Female")
    
    valueBox(
      value = round(df$ex, 1),
      subtitle = HTML("Life Expectancy at Birth<br><b>e<sub>0</sub> (Female)</b>"),
      icon = icon("female"),
      color = "red"
    )
  })
  
  # ---Download---
  
  life_download_merged <- reactive({
    
    male <- life_table_tidy %>%
      filter(State == input$state_lt, Gender == "Male") %>%
      select(Age, lx, qx, Lx, ex) %>%
      rename_with(~ paste0(., "_Male"), -Age)
    
    female <- life_table_tidy %>%
      filter(State == input$state_lt, Gender == "Female") %>%
      select(Age, lx, qx, Lx, ex) %>%
      rename_with(~ paste0(., "_Female"), -Age)
    
    full_join(male, female, by = "Age") %>%
      arrange(Age)
  })
  
  output$download_lt_merged <- downloadHandler(
    filename = function() {
      paste0("Life_Table_", input$state_lt, "_Male_Female.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(
        life_download_merged(),
        file,
        rowNames = FALSE
      )
    }
  )
  
  
  
  # --- Life Table Function Plots ---
  
  life_filtered <- reactive({
    df <- life_table_tidy %>% filter(State == input$state_lt)
    if (input$gender_lt != "Both") df <- df %>% filter(Gender == input$gender_lt)
    df
  })
  
  
  default_gender_colors <- c(
    "Male" = "#619CFF",
    "Female" = "#F8766D"
  )
  
  
  
  
  # e(x)
  output$ex_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, ex, colour = Gender)) +
      geom_line(size = 0.75) +
      scale_colour_manual(values = default_gender_colors) +
      labs(title = "Life Expectancy eₓ", x = "Age", y = "eₓ")+
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # q(x)
  output$qx_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, qx, colour = Gender)) +
      geom_line(size = 0.75) +
      scale_colour_manual(values = default_gender_colors) +
      labs(title = "Mortality Rate qₓ", x = "Age", y = "qₓ" )+
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # S(x)
  output$sx_plot <- renderPlotly({
    p <- ggplot(life_filtered(), aes(Age, Sx, colour = Gender)) +
      geom_line(size = 0.75) +
      scale_colour_manual(values = default_gender_colors) +
      labs(title = "Survival Function S(x)", x = "Age", y = "S(x)")+
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    ggplotly(p) %>% layout(autosize = TRUE)
    
  })
  
}

# --- Run App ---
shinyApp(ui, server)

