library(ggplot2)
library(dplyr)

data <- read.csv("/Users/reedhodges/Documents/GitHub/nc_labor_data/qcew_data/data_quarterly.csv")

# NAICS Code 11 (Agriculture, Forestry, Fishing, and Hunting)
industry_data <- filter(data, NAICS.Code == 11)

industry_data$Quarter <- as.factor(industry_data$Quarter)

# ANOVA to test for differences in average employment across quarters
anova_result <- aov(Average.Employment ~ Quarter, data = industry_data)
summary(anova_result)

# If significant, perform a post-hoc test to find out which quarters are different
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_result <- TukeyHSD(anova_result)
  print(posthoc_result)
}

ggplot(industry_data, aes(x = Quarter, y = Average.Employment, fill = Quarter)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Seasonal Variation of Employment in Agriculture, Forestry, Fishing, and Hunting",
       x = "Quarter",
       y = "Average Employment")
