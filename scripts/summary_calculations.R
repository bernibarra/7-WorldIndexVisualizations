library("dplyr")
summary_calculations <- function(dataset) {
  results <- list()
  transformed_data <- transform(dataset, world_happiness_report_score =
                                  as.numeric(world_happiness_report_score),
                                health_expenditure_pct_of_gdp =
                                  as.numeric(health_expenditure_pct_of_gdp),
                                health_expenditure_per_person =
                                  as.numeric(health_expenditure_per_person)
                                )
  
  results$avg_health_expenditure_pct <- transformed_data %>% 
    select(health_expenditure_pct_of_gdp) %>% 
    summarise(avg = mean(health_expenditure_pct_of_gdp, na.rm = TRUE))
  
  results$avg_world_happiness <- transformed_data %>%
    select(world_happiness_report_score) %>% 
    summarise(avg = mean(world_happiness_report_score, na.rm = TRUE))
  
  results$avg_health_expenditure_per_person <- transformed_data %>% 
    select(health_expenditure_per_person) %>% 
    summarise(avg = mean(health_expenditure_per_person, na.rm = TRUE))

  return (results)
}