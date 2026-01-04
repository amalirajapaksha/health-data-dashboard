library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readxl)
library(ggplot2)
library(openxlsx)
library(tidyr)


# --- Load population data ---
pop_data <- read_excel("population_pyramid_tidy.xlsx")
pop_data$Age <- as.numeric(pop_data$Age)

life_table_tidy <- read_excel("life_table_tidy.xlsx")
life_table_tidy$Age <- as.numeric(life_table_tidy$Age)

age_dis <- read_excel("age_distribution_tidy.xlsx")

current_pop_dis <- read_excel("current_population_distribution_tidy.xlsx")

pop_trend <- read_excel("population_trend_tidy.xlsx")

age_distribution_calculations <- read_excel("age_distribution_calculations.xlsx")
age_group_comparison <- age_distribution_calculations[1:6,7:9]




# ---- Age distribution ----
age_dis_long <- age_dis %>%
  pivot_longer(
    cols = -`Age group (years)`,
    names_to = "State",
    values_to = "Population"
  )

# ---- Population trend ----
pop_trend_long <- pop_trend %>%
  pivot_longer(
    cols = -c(Year, Gender),
    names_to = "State",
    values_to = "Population"
  )

# --- Age structure comparison ---
pop_percent <- age_group_comparison %>%
  group_by(Year) %>%
  mutate(
    Percentage = Population / sum(Population) * 100
  ) %>%
  ungroup()

pop_percent$`Age Group` <- factor(
  pop_percent$`Age Group`,
  levels = c("Young (0-14)", "Working Age (15-64)", "Elderly (65+)")
)



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
               menuSubItem("Popultion Trends", tabName = "trend"),
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
      tabItem( tabName = "home",
               
               # ================= Banner =================
               div(
                 class = "home-banner",
                 tagList(
                   icon("heartbeat"),
                   " Australia Health & Demographic Dashboard"
                 )
               ),
               
               # ================= Academic Intro =================
               fluidRow(
                 column(
                   width = 12,
                   p(
                     style = "text-align:center; font-size:18px; color:#2c3e50; max-width:900px; margin:auto;",
                     "This interactive dashboard presents a comprehensive overview of Australia's 
         population structure and demographic dynamics. It is designed to support 
         academic research, policy analysis, and data-driven decision-making through 
         intuitive visualisations and summary indicators."
                   )
                 )
               ),
               
               br(),
               
               # ================= KPI Value Boxes =================
               fluidRow(
                 valueBox(
                   value = "26+ M",
                   subtitle = "Total Population",
                   icon = icon("users"),
                   color = "aqua",
                   width = 3
                 ),
                 valueBox(
                   value = "17.7%",
                   subtitle = "Children (0–14)",
                   icon = icon("child"),
                   color = "green",
                   width = 3
                 ),
                 valueBox(
                   value = "64.2%",
                   subtitle = "Working-age (15–64)",
                   icon = icon("user-tie"),
                   color = "yellow",
                   width = 3
                 ),
                 valueBox(
                   value = "18.1%",
                   subtitle = "Elderly (65+)",
                   icon = icon("user-clock"),
                   color = "red",
                   width = 3
                 )
               ),
               
               br(),
               
               # ================= Mini Charts =================
               fluidRow(
                 box(
                   width = 6,
                   title = tagList(icon("chart-pie"), " Age Composition Overview"),
                   status = "primary",
                   solidHeader = TRUE,
                   plotOutput("mini_age_pie", height = "250px"),
                   p(
                     style = "font-size:14px; color:#555;",
                     "This chart summarises the proportional distribution of Australia's population 
         across major age groups."
                   )
                 ),
                 
                 box(
                   width = 6,
                   title = tagList(icon("chart-line"), " Population Trend Snapshot"),
                   status = "info",
                   solidHeader = TRUE,
                   plotOutput("mini_population_trend", height = "250px"),
                   p(
                     style = "font-size:14px; color:#555;",
                     "A high-level illustration of population growth and structural change over time."
                   )
                 )
               ),
               
               br(),
               
               # ================= Clickable Navigation Cards =================
               fluidRow(
                 box(
                   width = 12,
                   title = tagList(icon("compass"), "Explore Dashboard Sections"),
                   status = "success",
                   solidHeader = TRUE,
                   
                   fluidRow(
                     column(
                       width = 4,
                       tags$a(
                         href = "#shiny-tab-age",
                         div(
                           style = "background:#ffffff; padding:20px; border-radius:10px; 
                       box-shadow:0 4px 10px rgba(0,0,0,0.15); text-align:center;",
                           icon("chart-bar", style = "font-size:40px; color:#0055aa;"),
                           h4("Age Distribution"),
                           p("Analyse population shares by age group and year.")
                         )
                       )
                     ),
                     
                     column(
                       width = 4,
                       tags$a(
                         href = "#shiny-tab-pyramid",
                         div(
                           style = "background:#ffffff; padding:20px; border-radius:10px; 
                       box-shadow:0 4px 10px rgba(0,0,0,0.15); text-align:center;",
                           icon("chart-area", style = "font-size:40px; color:#009688;"),
                           h4("Population Pyramid"),
                           p("Visualise gender-wise age structure and demographic transitions.")
                         )
                       )
                     ),
                     
                     column(
                       width = 4,
                       tags$a(
                         href = "#shiny-tab-summary",
                         div(
                           style = "background:#ffffff; padding:20px; border-radius:10px; 
                       box-shadow:0 4px 10px rgba(0,0,0,0.15); text-align:center;",
                           icon("table", style = "font-size:40px; color:#f39c12;"),
                           h4("Summary Tables"),
                           p("Review numerical summaries and population indicators.")
                         )
                       )
                     )
                   )
                 )
               ),
               
               br(),
               
               # ================= Thesis-Ready Description =================
               fluidRow(
                 box(
                   width = 12,
                   title = tagList(icon("graduation-cap"), "Academic Relevance"),
                   status = "warning",
                   solidHeader = TRUE,
                   p(
                     "The demographic indicators presented in this dashboard are particularly relevant 
         for studies in public health, economics, social sciences, and population studies. 
         Changes in age composition influence labour supply, healthcare demand, dependency 
         ratios, and long-term economic sustainability."
                   ),
                   p(
                     "This tool enables clear communication of demographic patterns and supports 
         evidence-based policy discussions in academic reports, dissertations, and 
         professional presentations."
                   )
                 )
               ),
               
               br(),
               
               # ================= Footer =================
               fluidRow(
                 box(
                   width = 12,
                   background = "light-blue",
                   p(
                     strong("Data Source: "),
                     "Australian Bureau of Statistics (ABS) and other publicly available demographic datasets."
                   ),
                   p(
                     style = "font-size:13px; color:#555;",
                     "This dashboard is intended for educational and analytical use."
                   )
                 )
               )
      ),
      
      # --- Population Trend ---
      tabItem(
        tabName = "trend",
        
        # Top: Current population
        fluidRow(
          column(
            width = 12,
            box(
              title = "Current Population by State and Gender",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("current_pop_plot", height = "300px"),
              
              br(),
              
              div(
                style = "background-color:#cfe9f6; padding:12px; border-radius:6px;",
                tags$ul(
                  style = "font-size:15px; font-weight:500; margin-bottom:0;",
                  tags$li("New South Wales has the largest population in Australia for both males and females, followed by Victoria and Queensland."),
                  tags$li("In every state and territory, the female population is slightly higher than the male population."),
                  tags$li("The Northern Territory has the smallest population among all states and territories for both genders.")
                )
              )
            )
          )
        ),
        
        # Dropdown for state selection (controls bottom plots)
        fluidRow(
          column(
            width = 3,
            selectInput(
              "trend_state",
              "Select State:",
              choices = colnames(pop_trend)[c(-1, -11)], # all columns except Year
              selected = "Australia"
            )
          )
        ),
        
        # middle: side-by-side plots
        fluidRow(
          column(
            width = 6,
            box(
              title = uiOutput("trend_box_title"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("pop_trend_plot", height = "400px"),
              conditionalPanel(
                condition = "input.trend_state == 'Australia'",
                br(),
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px;",
                  tags$ul(
                    style = "font-size:16px; font-weight:500; color:#003366; margin-bottom:0;",
                    tags$li("This chart shows Australia’s population growth from 1971 to 2025, with separate lines for males and females."),
                    tags$li("Both populations follow a similar upward trend, increasing steadily over time, indicating long-term sustained population growth."), 
                    tags$li("Growth remains gradual until the early 2000s, followed by a clear acceleration after around 2005, consistent with higher net overseas migration and economic expansion."), 
                    tags$li("After 1990, the female population remains slightly higher than the male population.")
                    
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = uiOutput("age_box_title"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("age_dist_plot", height = "400px"),
              
              # Only show this for Australia
              conditionalPanel(
                condition = "input.trend_state == 'Australia'",
                br(),
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px;",
                  tags$ul(
                    style = "font-size:16px; font-weight:500; color:#003366; margin-bottom:0;",
                    tags$li("The youth population (0-14) accounts for 17.63% of Australia's total population, or about 4.8 million people, which has important implications for education planning and future workforce supply."),
                    tags$li("The working-age population (15-64) makes up 65.1% of the population, totaling approximately 17.7 million people, and represents the main source of economic productivity and tax revenue."),
                    tags$li("The elderly population (65+) represents 17.3% of the total population, or around 4.7 million people, placing increasing pressure on healthcare, pension systems, and social services.")
                  )
                )
              )
            )
          )
        ),
        
        # Bottom: Age Structure Comparison
        fluidRow(
          column(
            width = 12,
            box(
              title = "Australia Age Structure Comparison: 1971 vs 2025 ",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              fluidRow(
                # LEFT: Plot
                column(
                  width = 8,
                  plotlyOutput("age_structure_com", height = "300px")
                ),
                
                # RIGHT: Description
                column(
                  width = 4,
                  div(
                    style = "background-color:#cfe9f6; padding:12px; border-radius:6px; height:300px; overflow-y:auto;",
                    tags$ul(
                      style = "font-size:15px; font-weight:500; margin-bottom:0;",
                      tags$li("Comparing 1971 and 2025 age structures reveals Australia's demographic transformation over five decades."),
                      tags$li("Changes in youth, working-age, and elderly proportions demonstrate the country's progression through demographic transition stages and highlight emerging challenges or opportunities."),
                      tags$li("The proportions directly influence economic growth potential, social service demands, and policy priorities.")
                    )
                  )
                )
              )
            )
          )
        )
      ),  
      
      
      
      
      # --- Population Pyramid  ---
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
                         plotlyOutput("pyramidPlot", height = "calc(90vh)"),
                         div(
                           style = "background-color:#cfe9f6; padding:12px; border-radius:6px;",
                           tags$ul(
                             style = "font-size:16px; font-weight:500; color:#003366; margin-bottom:0;",
                             tags$li(
                               "Australia's population has doubled since 1981, growing from 15 million to 27.3 million people due to immigration and economic prosperity."
                             ),
                             tags$li(
                               "The constrictive shape of the current population pyramid indicates an ageing population with fewer young people and lower birth rates, suggesting that future population growth may slow or even decline unless birth rates rise or immigration increases."
                             ),   
                             tags$li(  
                               "This demographic trend presents substantial challenges for economic growth, pension sustainability, and healthcare systems. "
                             )
                           )
                         )
                         
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
                         downloadButton("download_lt_merged", "Download Life Table")
                       )
                ),
                
                # RIGHT PANEL
                column(width = 9,
                       box(
                         title = "Life Table Functions 2022-2024",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         height = "550px",
                         
                         tabsetPanel(
                           tabPanel(
                             "eₓ",
                             div(style = "width:100%;",
                                 plotlyOutput("ex_plot", height = "400px"),
                                 div(style = "background-color:#c6e3f5; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("Throughout the lifespan, women tend to live longer than men, reflecting consistently higher life expectancy for females at all ages.",
                                       style = "font-size:20px;")
                                 )
                             )
                           ),
                           tabPanel(
                             "qₓ",
                             div(style = "width:100%;",
                                 plotlyOutput("qx_plot", height = "400px"),
                                 div(style = "background-color:#c6e3f5; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("Mortality is low and similar for both genders at younger ages, but at older ages, men experience higher mortality than women.",
                                       style = "font-size:20px;")
                                 )
                             )
                           ),
                           tabPanel(
                             "S(x)",
                             div(style = "width:100%;",
                                 plotlyOutput("sx_plot", height = "400px"),
                                 div(style = "background-color:#c6e3f5; padding:10px; margin-top:10px; border-radius:5px;",
                                     p("Although survival is similar for males and females at younger ages, females show higher survival at older ages.",
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
  
  # ------------------ POPULATION TREND ------------------
  
  # --- Current Population by State and Gender ---
  output$current_pop_plot <- renderPlotly({
    
    df <- current_pop_dis %>%
      group_by(State, Gender) %>%
      summarise(Population = sum(Population), .groups = "drop")
    
    p <- ggplot(df, aes(
      x = reorder(State, Population),
      y = Population,
      fill = Gender
    )) +
      geom_col(position = "dodge", width = 0.7) +
      coord_flip() +
      
      scale_y_continuous(
        labels = function(x) formatC(x, format = "d", big.mark = ",")
      ) +
      
      scale_fill_manual(
        values = c(
          "Males" = "#1f4e79",
          "Females" = "#8b1a1a"
        )
      ) +
      labs(
        x = "",
        y = "Population",
        fill = "Gender"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        # 🔹 GRID LINES (VISIBLE)
        panel.grid = element_line(color = "white", size = 1),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    
    ggplotly(p)
  })
  
  
  
  
  # --- Population Trend ---
  
  output$trend_box_title <- renderUI({
    paste0("Population Trend – ", input$trend_state)
  })
  
  output$pop_trend_plot <- renderPlotly({
    
    df <- pop_trend_long %>%
      filter(State == input$trend_state)
    
    p <- ggplot(df, aes(Year, Population, colour = Gender)) +
      geom_line(size = 1) +
      scale_y_continuous(
        labels = function(x) formatC(x / 1e6, format = "f", digits = 0, big.mark = ",")
      ) +
      labs(
        x = "Year",
        y = "Population (millions)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    
    ggplotly(p)
  })
  
  
  
  # --- Age Distribution ---
  
  output$age_box_title <- renderUI({
    paste0("Age Distribution, Mar 2025 – ", input$trend_state)
  })
  
  output$age_dist_plot <- renderPlotly({
    
    df <- age_dis_long %>%
      filter(State == input$trend_state)
    
    df$`Age group (years)` <- factor(
      df$`Age group (years)`,
      levels = unique(df$`Age group (years)`)
    )
    
    p <- ggplot(df, aes(`Age group (years)`, Population)) +
      geom_col(fill = "#5A5AFF", width = 0.8) +
      scale_y_continuous(
        labels = function(x) formatC(x / 1e6, format = "f", digits = 1, big.mark = ",")
      ) +
      labs(
        x = "Age Group",
        y = "Population (millions)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
    
    ggplotly(p)
  })
  
  
  
  # --- Age Stucture Comparison ---
  output$age_structure_com <- renderPlotly({
    
    p <- ggplot(pop_percent,
                aes(x = `Age Group`, y = Percentage, fill = factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        x = "Age Group",
        y = "Population Percentage (%)",
        fill = "Year"
      ) +
      scale_fill_manual(
        values = c(
          "1971" = "#F39C12",
          "2025" = "#27AE60"
        ) ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  
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
  
  auto_update <- reactiveTimer(1000)
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
      labs(title = paste0("Life Expectancy eₓ - ", input$state_lt), x = "Age", y = "eₓ")+
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
      labs(title = paste0("Mortality Rate qₓ - ", input$state_lt), x = "Age", y = "qₓ" )+
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
      labs(title = paste0("Survival Function S(x) - ", input$state_lt), x = "Age", y = "S(x)")+
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
  
  output$mini_age_pie <- renderPlot({
    
    # Example age structure data (replace with real data later)
    age_data <- data.frame(
      AgeGroup = c("0–14", "15–64", "65+"),
      Percentage = c(17.7, 64.2, 18.1)
    )
    
    ggplot(age_data, aes(x = "", y = Percentage, fill = AgeGroup)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(
        title = "Population Age Composition (%)",
        fill = "Age Group"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$mini_population_trend <- renderPlot({
    
    # Example population trend data (millions)
    pop_trend <- data.frame(
      Year = seq(2000, 2024, by = 4),
      Population = c(19.1, 20.3, 21.5, 22.8, 24.1, 25.4, 26.6)
    )
    
    ggplot(pop_trend, aes(x = Year, y = Population)) +
      geom_line(linewidth = 1.2, color = "#0055aa") +
      geom_point(size = 2, color = "#0055aa") +
      labs(
        title = "Population Growth Trend (Millions)",
        x = "Year",
        y = "Population"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
}

# --- Run App ---
shinyApp(ui, server)

