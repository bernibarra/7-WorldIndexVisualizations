library(dplyr)
library(plotly)

# Make an interactive plot that shows the relationship between the percentage
# of GDP that's spent on education expenditure and unemployment rate

chart_2 <- function(data){
  data_frame_num <- transform(data, education_expenditure_pct_of_gdp =
                            as.numeric(education_expenditure_pct_of_gdp),
                          unemployment_pct = as.numeric(unemployment_pct))
  
  data_for_graph <- data_frame_num %>%
    select(country_name, education_expenditure_pct_of_gdp, unemployment_pct) %>%
    rename(country = country_name,
           education_expenditure =
             education_expenditure_pct_of_gdp)

  interactive_plot <- plot_ly(
    data = data_for_graph, # data frame to show
    x = ~education_expenditure, # variable for the x-axis
    y = ~unemployment_pct, # variable for the y-axis
    marker = list(size = 10),
    hoverinfo = "text",
    text = ~paste0("Country: ", country,
                  "<br>Percent GDP in Education Expenditure: ",
                  round(education_expenditure, 2), "%",
                  "<br>Unemployment Rate: ",
                  unemployment_pct, "%")
  ) %>%
    layout(
      title = "Education Expenditure vs Unemployment Rate",
      xaxis = list(title = "Percentage of GDP in Education Expenditure"),
      yaxis = list(title = "Unemployment Rate")
    )
}
