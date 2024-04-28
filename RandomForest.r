water_data$Potability <- as.factor(water_data$Potability)

# Preliminary Random Forest Model with all variables
preliminary.RFModel <- randomForest(Potability ~ ph + Hardness + Solids 
                                    + Chloramines + Sulfate + Conductivity
                                    + Organic_carbon + Trihalomethanes
                                    + Turbidity
                                    , data = water_data, 
                                    mtry = sqrt(10), importance = TRUE)

preliminary.RFModel

# Call:
# randomForest(formula = Potability ~ ph + Hardness + Solids +
#                                     Chloramines + Sulfate + 
#                                     Conductivity + Organic_carbon + 
#                                     Trihalomethanes + Turbidity, 
#                                     data = water_data, mtry = sqrt(10), importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3

# OOB estimate of  error rate: 32.23%
# Confusion matrix:
#      0   1 class.error
# 0 1758 240   0.1201201
# 1  816 462   0.6384977

varImpPlot(preliminary.RFModel, main="Preliminary Random Forest Model Variable Importance Plot")

# Didn't remove any variables based on this plot, so proceed with preliminary.RFModel

# Set the seed for reproducibility
set.seed(123)

# Create a vector to store error rates
error.rate = rep(0, 10)

# Perform 10-fold cross-validation
for(i in 1:10) {
  set.seed(i)
  
  # Create indices for the training set
  index = sample.int(n = nrow(water_data), 
                     size = floor(0.8 * nrow(water_data)), 
                     replace = FALSE)
  
  # Create the training and test sets
  train = water_data[index, ]
  test = water_data[-index, ]
  
  # Build the random forest model
  result.rf = randomForest(Potability ~ ph + Hardness + Solids +
                           Chloramines + Sulfate + Conductivity 
                           + Organic_carbon + Trihalomethanes + Turbidity,
                           data = train, 
                           mtry = sqrt(10), 
                           importance = TRUE)
  
  # Make predictions on the test set
  yhat.rf = predict(result.rf, newdata = test)
  
  # Create confusion matrix
  con.matrix = table(yhat.rf, test$Potability)
  
  # Calculate error rate
  error.rate[i] = (con.matrix[1, 2] + con.matrix[2, 1]) / sum(con.matrix)
}

# Print error rates
error.rate
# 0.3262195 0.3185976 0.3109756 0.3094512 0.3521341 0.3003049 0.3048780 0.3246951 0.3125000 0.3216463
# Print mean error rate
mean(error.rate)
# 0.3181402
