install.packages("dplyr")
library(dplyr)

water_potability <- dplyr::mutate(water_potability, dplyr::across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

head(water_potability)

library(caret)
set.seed(123)

splitIndex <- createDataPartition(water_potability$Potability, p = 0.8, list = FALSE)
train_set <- water_potability[splitIndex, ]
test_set <- water_potability[-splitIndex, ]

water_potability_model <- glm(Potability ~ ., data=train_set, family=binomial)
summary(water_potability_model)

test_prob <- predict(water_potability_model, newdata = test_set, type= "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)

confusionMatrix(as.factor(test_pred), as.factor(test$Potability))

# shuffled approach
set.seed(123)
shuffled_data <- water_potability[sample(1:nrow(water_potability)), ]

train_index <- sample(1:nrow(shuffled_data), size = floor(0.8 * nrow(shuffled_data)))

train_set <- shuffled_data[train_index, ]
test_set <- shuffled_data[-train_index, ]

water_potability_model <- glm(Potability ~ ., data = train_set, family = binomial)
summary(water_potability_model)

# fitting the model
water_potability_model <- glm(Potability ~ Solids + Organic_carbon, data = train_set, family = binomial)
summary(water_potability_model)
