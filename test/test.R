library(dplyr)
library(tidyr)
library(plotly)
library(readxl)
read_excel("population_trend_tidy")

# Convert data to long format
pop_long <- population_trend_tidy %>%
  select(Year, Gender, Australia, New_South_Wales, Victoria,
         Queensland, South_Australia, Western_Australia,
         Tasmania, Northern_Territory) %>%
  pivot_longer(cols = Australia:Northern_Territory,
               names_to = "State",
               values_to = "Population")

# Produce initial plot for Australia
p <- plot_ly(pop_long %>% filter(State == "Australia"),
             x = ~Year, y = ~Population, color = ~Gender,
             type = 'scatter', mode = 'lines+markers')

# Create dropdown menu for states
states <- unique(pop_long$State)

buttons <- lapply(states, function(st) {
  list(
    method = "restyle",
    args = list(
      list(
        x = list(pop_long %>% filter(State == st) %>% pull(Year),
                 pop_long %>% filter(State == st) %>% pull(Year)),
        y = list(pop_long %>% filter(State == st & Gender == "Male") %>% pull(Population),
                 pop_long %>% filter(State == st & Gender == "Female") %>% pull(Population)),
        color = list(c("Male","Female"))
      )
    ),
    label = st
  )
})

# Attach dropdown
p <- p %>%
  layout(
    title = "Population Trend by Gender (Select State)",
    updatemenus = list(list(
      y = 1.1,
      buttons = buttons
    )),
    xaxis = list(title = "Year"),
    yaxis = list(title = "Population")
  )

p
