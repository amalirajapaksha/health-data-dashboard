pop_data <- read_excel("population_pyramid_tidy.xlsx")
saveRDS(pop_data, "population_pyramid_tidy.rds")

age_dis <- read_excel("age_distribution_tidy.xlsx")
saveRDS(age_dis, "age_distribution_tidy.rds")

pop_trend <- read_excel("population_trend_tidy.xlsx")
saveRDS(pop_trend, "population_trend_tidy.rds")

age_distribution_calculations <- read_excel("age_distribution_calculations.xlsx")
saveRDS(age_distribution_calculations, "age_distribution_calculations.rds")

life_table_tidy <- read_excel("life_table_tidy.xlsx")
saveRDS(life_table_tidy, "life_table_tidy.rds")

current_pop_dis <- read_excel("current_population_distribution_tidy.xlsx")
saveRDS(current_pop_dis, "current_population_distribution_tidy.rds")