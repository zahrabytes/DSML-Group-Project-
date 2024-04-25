install.packages("dplyr")
library(dplyr)

water_potability <- dplyr::mutate(water_potability, dplyr::across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

head(water_potability)
