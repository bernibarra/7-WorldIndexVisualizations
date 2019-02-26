library(dplyr)
library(ggplot2)

# Makes a graph of the average world happiness score of countries that fall
# within a certain population range
make_graph <- function(data) {
  data <- transform(
    data, world_happiness_report_score = as.numeric(
      as.character(world_happiness_report_score)
      )
    )
  data %>%
    filter(!is.na(world_happiness_report_score)) %>%
    mutate(cuts = cut(population, c(1, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10))) %>%
    group_by(cuts) %>%
    summarise(avg_happiness_score = mean(world_happiness_report_score)) %>%
    ggplot() +
      geom_bar(
        aes(
          x = cuts, y = avg_happiness_score, fill = avg_happiness_score
        ), stat = "identity"
      ) +
      labs(
        x = "Population Range", y = "Average World Happiness Score",
        title = "Average World Happiness Score by Population"
      ) +
      scale_x_discrete(
        labels = c(
          "100 thousand - 1 million", "1 million - 10 million",
          "10 million - 100 million", "100 million - 1 billion", 
          "1 billion - 10 billion"
        )
      ) +
      guides(fill = FALSE)
}