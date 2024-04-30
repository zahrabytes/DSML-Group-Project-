water_potability <- read.csv("C:/Users/maxch/Downloads/water_potability.csv")

library(tidyverse)  # For data manipulation and visualization
library(caret)      # For modeling and validation
library(broom)  # For tidy statistical summaries

# Convert the 'Potability' column to a factor
water_potability$Potability <- factor(water_potability$Potability, levels = c(0, 1), labels = c("Not Potable", "Potable"))

# View the first few rows and summary of the data
head(water_potability)
summary(water_potability)

# Check for missing values
colSums(is.na(water_potability))

# Create histograms for each variable
water_potability %>% 
  gather(key = "variable", value = "value", -Potability) %>% 
  ggplot(aes(x = value, fill = Potability)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Histograms of Variables by Potability", x = "Value", y = "Frequency") +
  theme(legend.position = "bottom")

# Create boxplots for each variable
water_potability %>% 
  gather(key = "variable", value = "value", -Potability) %>% 
  ggplot(aes(x = Potability, y = value, fill = Potability)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Variables by Potability", x = "", y = "Value") +
  theme(legend.position = "none")


# List of predictors
predictors <- names(water_potability)[names(water_potability) != "Potability"]


# Function to fit logistic regression for each predictor
fit_individual_logistic <- function(predictor) {
  formula <- as.formula(paste("Potability ~", predictor))
  model <- glm(formula, data = water_potability, family = binomial())
  tidy(model)
}

# Apply the function to each predictor and store results
results <- map_df(predictors, fit_individual_logistic, .id = "Predictor")

# View results
print(results)

full_model <- glm(Potability ~ ph + Hardness + Solids + Chloramines + Sulfate + Conductivity + Organic_carbon + Trihalomethanes + Turbidity, 
                  data = water_potability, family = binomial())
summary(full_model)

