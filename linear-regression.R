library(ggplot2)
library(dplyr)

data <- read.csv("/Users/reedhodges/Documents/GitHub/nc_labor_data/qcew_data/data_annual.csv")

# NAICS Code 11 (Agriculture, Forestry, Fishing and Hunting)
industry_data <- filter(data, NAICS.Code == 11)

# linear regression model
model <- lm(Average.Employment ~ Year, data = industry_data)

summary(model)

ggplot(industry_data, aes(x = Year, y = Average.Employment)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Trend of Average Employment Over Years in Agriculture, Forestry, Fishing and Hunting",
       x = "Year",
       y = "Average Employment") +
  theme_minimal()
