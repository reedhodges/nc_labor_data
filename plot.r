library(ggplot2)
library(dplyr)

data <- read.csv("/Users/reedhodges/Documents/GitHub/nc_labor_data/qcew_data/DETAILQCEW.csv")

data$Year <- as.numeric(as.character(data$Year))

process_industry_data <- function(df, industry_name) {
  df %>%
    filter(Industry == industry_name) %>%
    group_by(Year) %>%
    summarise(Average_Annual_Employment = mean(`Average.Employment`, na.rm = TRUE)) %>%
    mutate(Percent_of_Max = (Average_Annual_Employment / max(Average_Annual_Employment, na.rm = TRUE)),
           Industry = industry_name)
}

information_data <- process_industry_data(data, 'Information')
manufacturing_data <- process_industry_data(data, 'Manufacturing')

combined_data <- bind_rows(information_data, manufacturing_data)

ggplot(combined_data, aes(x = Year, y = Percent_of_Max, color = Industry)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Average Annual Employment as Percentage of Industry Maximum",
       subtitle = "Comparing Information vs. Manufacturing Industries",
       x = "Year",
       y = "% of Industry's Maximum Employment")
