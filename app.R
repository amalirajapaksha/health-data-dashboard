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

age_dis <- read_excel("age_distribution_tidy_2025March.xlsx")

current_pop_dis <- read_excel("current_population_distribution_tidy_2025March.xlsx")

pop_trend <- read_excel("population_trend_tidy.xlsx")

age_structure_comparison_1971vs2025 <- read_excel("age_structure_comparison_1971vs2025.xlsx")

rates_data <- read_excel("ratios_tidy.xlsx")

GR_by_age <- read_excel("Gender_ratio_by_age_tidy.xlsx")

rates <- read_excel("rates_tidy.xlsx")



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
pop_percent <- age_structure_comparison_1971vs2025 %>%
  group_by(Year) %>%
  mutate(
    Percentage = Population / sum(Population) * 100
  ) %>%
  ungroup()

pop_percent$`Age Group` <- factor(
  pop_percent$`Age Group`,
  levels = c("Young (0-14)", "Working Age (15-64)", "Elderly (65+)")
)

# --- GR by Age ---
rates_long <- GR_by_age %>%
  pivot_longer(
    cols = -year,
    names_to = "Age",
    values_to = "GenderRatio"
  )

age_order <- c("Birth", "5 years old", "15 years old", "20 years old",
               "30 years old", "40 years old", "50 years old",
               "60 years old", "70 years old", "80 years old", "90 years old", "100+ years old")

rates_long$Age <- factor(rates_long$Age, levels = age_order)


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
               menuSubItem("Population Pyramid", tabName = "pyramid"),
               menuSubItem("Ratios and Rates ", tabName = "rates")
               
               # Later more subtabs will be added 
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
        
        /* Home page immage */
  .home-image-container {
    width: 100%;
    text-align: center;
    margin-top: 10px;
  }

  .home-image {
    width: 100%;
    max-height: 500px;
    object-fit: cover;
    border-radius: 12px;
    box-shadow: 0 6px 18px rgba(0,0,0,0.3);
  }

        @media (min-height: 800px) { #pyramidPlot { height: calc(90vh) !important; } }
        @media (max-height: 799px) { #pyramidPlot { height: calc(75vh) !important; } }
        @media (max-width: 1600px) { .value-box, .small-box { font-size: 90%; } .box-body { padding: 10px !important; } }
        @media (max-width: 1200px) { .home-banner { font-size: 32px; padding: 40px 5px; } }
        @media (max-width: 992px) { .col-sm-3, .col-sm-9 { width: 100%; } }
        
        /* ---------- Professional Home Footer ---------- */

        .home-footer{
          background:#002b45;
          color:white;
          padding:30px 30px;
          margin-top:40px;
          border-top:6px solid #0055aa;
        }
        
        .home-footer h4{
          font-weight:700;
          margin-bottom:12px;
        }
        
        .home-footer p{
          font-size:14px;
          color:#d9e6f2;
        }
        
        .home-footer ul{
          list-style:none;
          padding-left:0;
        }
        
        .home-footer ul li{
          margin-bottom:6px;
          font-size:14px;
        }
        
        .home-footer a{
          color:#9fd3ff;
          text-decoration:none;
        }
        
        .home-footer a:hover{
          text-decoration:underline;
        }
        
        .footer-bottom{
          text-align:center;
          margin-top:25px;
          font-size:13px;
          color:#bfc9d3;
        }
      "))
    ),
    
    tabItems(
      # --- Home Tab ---
      tabItem(tabName = "home",
              div(class = "home-banner", "Welcome to the Australian Health Data Dashboard"),
              fluidRow(
                column(
                  width = 12,
                  div(class = "home-image-container",
                      img(src = "home_page.png", class = "home-image")
                  )
                )
              ),
              
              div(
                class = "home-footer",
                
                div(class = "footer-bottom",
                    HTML("© 2026 Health & Demographic Dashboard | Developed for Academic Research | 
              Data provided by ABS and public demographic sources")
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
              title = "Current Population by State and Gender ( Mar 2025 )",
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
      
      
      # --- Rate and Ratio Tab ---
      tabItem(
        tabName = "rates",
        
        # --- Ratios ---
        
        fluidRow(
          column(
            width = 9,
              selectInput(
                "ratio_select",
                "Select Ratio:",
                choices = c(
                  "Child Dependency Ratio",
                  "Aged Dependency Ratio",
                  "Dependency Ratio",
                  "Child-Women Ratio",
                  "Gender Ratio"
                )
            )
          ),
          
          column(
            width = 12,
            box(
              title = uiOutput("ratio_title"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("ratio_plot", height = "500px"),
              
              # --- Show interpretation only for Child Dependency Ratio ---
              conditionalPanel(
                condition = "input.ratio_select == 'Child Dependency Ratio'",
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    "The Child Dependency Ratio (CDR) shows a steady decline from about 50 in 1960 to below 30 by 2025. 
         This indicates fewer children relative to the working-age population, reflecting demographic transition 
         and declining fertility rates over time."
                  ),
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    HTML("Formula: CDR = (Population aged 0–14 / Population aged 15–64) × 100")
                  )
                )
              ),
              
              # --- Show interpretation only for Aged Dependency Ratio ---
              conditionalPanel(
                condition = "input.ratio_select == 'Aged Dependency Ratio'",
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    "The Aged Dependency Ratio (ADR) has risen steadily from the 1960s to 2025, 
         moving from around 14 to nearly 28. This reflects the growing proportion of elderly 
         individuals relative to the working-age population, with sharper increases since 2010. 
         It signals mounting pressure on healthcare, pensions, and social support systems."
                  ),
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    HTML("Formula: ADR = (Population aged 65+ / Population aged 15–64) × 100")
                  )
                )
              ),
              
              # --- Show interpretation only for Dependency Ratio ---    
              conditionalPanel(
                condition = "input.ratio_select == 'Dependency Ratio'",
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    "From 1960 to 1990, the dependency ratio decreased from about 64 to below 50 because fewer children were being born and more people were in the working-age group. Between 1990 and 2010, the ratio reached its lowest level, meaning there were more workers than dependents, which is called a demographic dividend. From 2010 to 2025, the ratio increased again to around 55 due to an ageing population and more elderly dependents. Overall, the trend shows a change from high birth rates to a period of economic advantage, and then to increasing pressure from an ageing population."
                  ),
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    HTML("Formula: ADR = [(Population aged 0–14) + (Population aged 65+)] / Population aged 15–64) × 100")
                  )
                )
              ),
              
              # --- Show interpretation only for Child-Women Ratio ---
              conditionalPanel(
                condition = "input.ratio_select == 'Child-Women Ratio'",
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    "The Child Dependency Ratio (CDR) shows a steady decline from about 50 in 1960 to below 30 by 2025. 
         This indicates fewer children relative to the working-age population, reflecting demographic transition 
         and declining fertility rates over time."
                  ),
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    HTML("Formula: CDR = (Population aged 0–4 / Female population aged 15–49) × 1000")
                  )
                )
              ),
              
              
              # --- Show interpretation only for Gender Ratio ---    
              conditionalPanel(
                condition = "input.ratio_select == 'Gender Ratio'",
                div(
                  style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    "In 1960, the gender ratio was above 102, showing slightly more males than females. 
Over the decades, the ratio steadily declined, reaching below 98.5 by 2025. 
This indicates that the female population has gradually become larger relative to the male population. 
Overall, the trend reflects a shift from a male-heavy population in the 1960s to a female-heavy population by 2025."
                  ),
                  tags$p(
                    style = "font-size:15px; font-weight:500; color:#003366;",
                    HTML("Formula: ADR = (Total number of males / Total number of females) × 100")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              title = "Gender Ratio by Age",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("gender_ratio_all_plot", height = "500px"),
              div(
                style = "background-color:#cfe9f6; padding:12px; border-radius:6px; margin-top:10px;",
                tags$p(
                  style = "font-size:15px; font-weight:500; color:#003366;",
                  "This plot shows the gender ratio for all age groups over time. 
           A value above 100 indicates more males than females, while a value below 100 indicates more females. 
           Over the decades, you can observe the shift from a male-heavy population in some ages to a female-heavy population in others."
                )
              )
            )
          )
        ),
          
          # --- Rates ---
          fluidRow(
            column(
              width = 12,
            selectInput(
                  "rate_select",
                  "Select Rate:",
                  choices = c(
                    "Crude Birth Rate",
                    "Crude Death Rate",
                    "Age Standardized Death Rate",
                    "Fertility Rate"
                  )
                )
            ),
            
            column(
              width = 12,
              box(
                title = uiOutput("rate_title"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotlyOutput("rates_plot", height = "500px")
              )
            )
          )
        ),
      
      # --- LIFE TABLE FUNCTIONS ---
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
      geom_line(size = 0.7) +
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
  
  
  # --- Time series plots of ratios and rates ---
  
  output$ratio_title <- renderUI({
    paste0("Time Series Plot of ", input$ratio_select)
  })
  
  output$ratio_plot <- renderPlotly({
    
    p <- plot_ly(rates_data, x = ~Year)
    
    if (input$ratio_select == "Child Dependency Ratio") {
      p <- p %>% add_lines(y = ~Child_Dependency_Ratio, name = "Child Dependency Ratio")
    }
    
    if (input$ratio_select == "Aged Dependency Ratio") {
      p <- p %>% add_lines(y = ~Aged_Dependency_Ratio, name = "Aged Dependency Ratio")
    }
    
    if (input$ratio_select == "Dependency Ratio") {
      p <- p %>% add_lines(y = ~Dependency_Ratio, name = "Dependency Ratio")
    }
    
    if (input$ratio_select == "Gender Ratio") {
      p <- p %>% add_lines(y = ~Gender_Ratio, name = "Gender Ratio")
    }
    
    if (input$ratio_select == "Child-Women Ratio") {
      p <- p %>% add_lines(y = ~Child_Women_Ratio, name = "Child-Women Ratio")
    }
    
    p %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value"),
        showlegend = TRUE
      )
  })
  
  output$gender_ratio_all_plot <- renderPlotly({
    
    p <- ggplot(rates_long, aes(x = year, y = GenderRatio, color = Age)) +
      geom_line(size = 0.5) +
      geom_point(size = 0.5) +
      labs(
        x = "Year",
        y = "Gender Ratio",
        title = "Time Series of Gender Ratio by Age"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      )
    
    ggplotly(p)
  })
  
  output$rate_title <- renderUI({
    paste0("Time Series Plot of ", input$rate_select)
  })
  
  output$rates_plot <- renderPlotly({
    
    p <- plot_ly(rates, x = ~Year)
    
    if (input$rate_select == "Crude Birth Rate") {
      p <- p %>% add_lines(
        y = ~`Crude_Birth_Rate(per_1000)`,
        name = "Crude Birth Rate"
      )
    }
    
    if (input$rate_select == "Crude Death Rate") {
      p <- p %>% add_lines(
        y = ~`Crude_Death_Rate(per_1000)`,
        name = "Crude Death Rate"
      )
    }
    
    if (input$rate_select == "Age Standardized Death Rate") {
      p <- p %>% add_lines(
        y = ~`Age_Standardized_Death_Rate(per_100000)`,
        name = "ASDR"
      )
    }
    
    if (input$rate_select == "Fertility Rate") {
      p <- p %>% add_lines(
        y = ~`Fertility_Rate(per_woman)`,
        name = "Fertility Rate"
      )
    }
    
    p %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value"),
        showlegend = TRUE
      )
  })
  
}  

# --- Run App ---
shinyApp(ui, server)

