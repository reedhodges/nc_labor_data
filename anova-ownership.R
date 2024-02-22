library(ggplot2)
library(dplyr)

data <- read.csv("/Users/reedhodges/Documents/GitHub/nc_labor_data/qcew_data/data_quarterly.csv")

data$Ownership <- as.factor(data$Ownership)
data$Average.Weekly.Wage <- as.numeric(data$Average.Weekly.Wage)

data_clean <- filter(data, !is.na(Average.Weekly.Wage))

# ANOVA to test for differences in average weekly wages across different ownership types
anova_result <- aov(Average.Weekly.Wage ~ Ownership, data = data_clean)
summary(anova_result)

# If significant, we can perform a post-hoc test to find out which groups are different
# Use Tukey's Honest Significant Difference test
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

ggplot(data_clean, aes(x = Ownership, y = Average.Weekly.Wage, fill = Ownership)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparison of Average Weekly Wages by Ownership Type",
       x = "Ownership Type",
       y = "Average Weekly Wage ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
